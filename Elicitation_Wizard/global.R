library(shiny)
library(bslib)
library(ellmer)
library(future)
library(promises)
library(shinyjs)

future::plan(multisession)

source("helpers.R")
source("pages/expert_responses.R")
source("pages/prior_summary.R")
source("pages/opinion_pooling.R")
source("pages/delphi.R")

env_url <- Sys.getenv("BASE_URL")
env_key <- Sys.getenv("API_KEY")

`%||%` <- function(x, y) if (is.null(x)) y else x

extract_stan <- function(text) {
  # Match ```stan, ```r, ```R, or bare ``` fences; returns first matched block.
  m <- regmatches(text, gregexpr("(?s)```(?:stan|[rR])?[ \t]*\n(.+?)\n```", text, perl = TRUE))[[1]]
  if (length(m) > 0) sub("^```(?:stan|[rR])?[ \t]*\n", "", sub("\n```$", "", m[[1]])) else text
}

render_markdown <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) return(NULL)
  text <- gsub("\\$\\$([\\s\\S]+?)\\$\\$", "\\\\[\\1\\\\]", text, perl = TRUE)
  text <- gsub("\\$([^$\n]+?)\\$", "\\\\(\\1\\\\)", text, perl = TRUE)
  markdown(text)
}

elicitation_prompt <- function(clinical_context, parameter_topic) {
  paste0(
    "You are a biostatistics expert participating in a structured Bayesian prior elicitation exercise for a clinical trial.\n\n",
    "## Clinical Context\n",
    clinical_context, "\n\n",
    "## Parameter to Elicit Prior For\n",
    parameter_topic, "\n\n",
    "**Important instructions for setting the prior:**\n",
    "- Draw on published clinical trials, empirical data, and established domain knowledge directly relevant to the clinical context described above.\n",
    "- Your prior must be **informative** — not weakly-informative or non-informative. Avoid vague or default values without justification.\n",
    "- Base your answer on realistic data or strong prior experience relevant to the specific setting, population, and endpoint described.\n",
    "- If the context involves a control arm, anchor your prior to observed control arm rates from comparable trials or registries.\n\n",
    "Respond with **only** the final prior distribution(s) in the form: `DistributionName(param1=value, param2=value)`. ",
    "If multiple parameters are required (as specified in the model), provide one distribution per line. ",
    "Do not include explanatory text, evidence summaries, or uncertainty ratings — only the distribution specification(s) with numeric values."
  )
}

parse_prior_distribution <- function(response_text) {
  if (is.null(response_text) || !is.character(response_text) ||
      nchar(trimws(response_text)) == 0) {
    return("Could not parse")
  }

  lines <- strsplit(response_text, "\n", fixed = TRUE)[[1]]

  section_start <- grep("(?i)^\\s*2\\..*prior\\s+distribution", lines, perl = TRUE)
  if (length(section_start) == 0)
    section_start <- grep("(?i)\\*{1,2}prior\\s+distribution\\*{1,2}", lines, perl = TRUE)

  if (length(section_start) > 0) {
    start_idx    <- section_start[1]
    remaining    <- lines[(start_idx + 1):length(lines)]
    next_section <- grep("^\\s*[3-9][\\.\\)]\\s", remaining, perl = TRUE)
    end_idx      <- if (length(next_section) > 0) start_idx + next_section[1] - 1 else length(lines)
    search_lines <- lines[start_idx:end_idx]
  } else {
    search_lines <- lines
  }

  dist_families <- paste0(
    "(?:",
    paste(c(
      "Beta", "Normal", "Log[- ]?Normal", "Lognormal", "LogNormal",
      "Gamma", "Inverse[- ]?Gamma", "Half[- ]?Normal", "Half[- ]?Cauchy",
      "Cauchy", "Exponential", "Uniform", "Student[- ]?t", "Weibull",
      "Log[- ]?Logistic", "Pareto",
      "Dirichlet", "Binomial", "Poisson"
    ), collapse = "|"),
    ")"
  )

  pattern <- paste0("(", dist_families, ")\\s*\\(([^)]*\\d[^)]*)\\)")

  extract_param_label <- function(line, match_start) {
    prefix <- substr(line, 1, match_start - 1)
    prefix <- trimws(prefix)
    prefix <- sub("^[-*#>`]+\\s*", "", prefix)
    prefix <- gsub("\\*{1,2}([^*]+)\\*{1,2}", "\\1", prefix)
    m <- regmatches(prefix, regexpr("[A-Za-z][A-Za-z0-9_.]*\\s*(?:[~:=]\\s*)?$", prefix, perl = TRUE))
    if (length(m) == 0 || nchar(m) == 0) return("")
    sub("\\s*[~:=]\\s*$", "", trimws(m))
  }

  all_matches <- list()
  for (line in search_lines) {
    m_pos  <- gregexpr(pattern, line, perl = TRUE)[[1]]
    if (m_pos[1] == -1) next
    m_strs <- regmatches(line, gregexpr(pattern, line, perl = TRUE))[[1]]
    for (k in seq_along(m_strs)) {
      ds <- gsub("\\s+", " ", trimws(m_strs[k]))
      if (nchar(ds) == 0) next
      all_matches[[length(all_matches) + 1]] <- c(dist = ds, label = extract_param_label(line, m_pos[k]))
    }
  }

  if (length(all_matches) > 0) {
    dist_strs <- sapply(all_matches, `[[`, "dist")
    labels    <- sapply(all_matches, `[[`, "label")
    keep      <- !duplicated(dist_strs)
    result    <- dist_strs[keep]; names(result) <- labels[keep]
    return(result)
  }

  tilde_pattern <- paste0("~\\s*(", dist_families, ")\\s*\\(([^)]*\\d[^)]*)\\)")
  tilde_matches <- list()
  for (line in search_lines) {
    m_pos  <- gregexpr(tilde_pattern, line, perl = TRUE)[[1]]
    if (m_pos[1] == -1) next
    m_strs <- regmatches(line, gregexpr(tilde_pattern, line, perl = TRUE))[[1]]
    for (k in seq_along(m_strs)) {
      ds <- gsub("^~\\s*", "", gsub("\\s+", " ", trimws(m_strs[k])))
      if (nchar(ds) == 0) next
      tilde_matches[[length(tilde_matches) + 1]] <- c(dist = ds, label = extract_param_label(line, m_pos[k]))
    }
  }
  if (length(tilde_matches) > 0) {
    dist_strs <- sapply(tilde_matches, `[[`, "dist")
    labels    <- sapply(tilde_matches, `[[`, "label")
    keep      <- !duplicated(dist_strs)
    result    <- dist_strs[keep]; names(result) <- labels[keep]
    return(result)
  }

  # Tightened fallback: require the family name to be immediately
  # followed by '(' and contain at least one digit inside, so generic sentences
  # like "gamma radiation was 5 mSv" are not matched.
  fallback_pattern <- paste0("(?i)(?:", paste(c(
    "Beta", "Normal", "Log[- ]?Normal", "Lognormal", "LogNormal",
    "Gamma", "Inverse[- ]?Gamma", "Half[- ]?Normal", "Half[- ]?Cauchy",
    "Cauchy", "Exponential", "Uniform", "Student[- ]?t", "Weibull",
    "Log[- ]?Logistic", "Pareto", "Poisson"
  ), collapse = "|"), ")\\s*\\([^)]*[0-9][^)]*\\)")
  candidate_lines <- search_lines[grepl(fallback_pattern, search_lines, perl = TRUE)]
  if (length(candidate_lines) > 0) {
    snippets <- sapply(candidate_lines, function(l) {
      s <- trimws(l)
      s <- sub("^[-*#>]+\\s*", "", s)
      s <- gsub("\\*{1,2}([^*]+)\\*{1,2}", "\\1", s)
      if (nchar(s) > 120) s <- paste0(substr(s, 1, 117), "...")
      s
    })
    return(unique(snippets))
  }

  return("Could not parse")
}

parse_dist_params <- function(dist_str) {
  if (is.null(dist_str) || !is.character(dist_str) || nchar(trimws(dist_str)) == 0) return(NULL)
  dist_str <- trimws(dist_str)

  m <- regmatches(dist_str, regexec("^([A-Za-z][A-Za-z_ -]*)\\((.*)\\)$", dist_str, perl = TRUE))[[1]]
  if (length(m) < 3) return(NULL)

  raw_family <- trimws(m[2])
  raw_args   <- trimws(m[3])

  family <- gsub("[_ ]", "-", raw_family)
  family <- gsub("(?i)^lognormal$",     "Log-Normal",    family)
  family <- gsub("(?i)^log-normal$",    "Log-Normal",    family, perl = TRUE)
  family <- gsub("(?i)^half-normal$",   "Half-Normal",   family, perl = TRUE)
  family <- gsub("(?i)^halfnormal$",    "Half-Normal",   family, perl = TRUE)
  family <- gsub("(?i)^half-cauchy$",   "Half-Cauchy",   family, perl = TRUE)
  family <- gsub("(?i)^student-t$",     "Student-t",     family, perl = TRUE)
  family <- gsub("(?i)^inverse-gamma$", "Inverse-Gamma", family, perl = TRUE)
  family <- gsub("(?i)^log-logistic$",  "Log-Logistic", family, perl = TRUE)
  family <- gsub("(?i)^loglogistic$",   "Log-Logistic", family, perl = TRUE)
  if (!grepl("-", family))
    family <- paste0(toupper(substr(family, 1, 1)), substring(family, 2))

  if (tolower(gsub("-", "", family)) %in% c("dirichlet", "binomial")) return(NULL)

  arg_parts <- trimws(strsplit(raw_args, ",")[[1]])
  arg_parts <- arg_parts[nchar(arg_parts) > 0]
  if (length(arg_parts) == 0) return(NULL)

  param_names <- character(length(arg_parts))
  param_vals  <- numeric(length(arg_parts))

  for (j in seq_along(arg_parts)) {
    part <- arg_parts[j]
    if (grepl("=", part, fixed = TRUE)) {
      kv             <- strsplit(part, "=", fixed = TRUE)[[1]]
      param_names[j] <- tolower(trimws(gsub("[^A-Za-z0-9_]", "", kv[1])))
      param_vals[j]  <- suppressWarnings(as.numeric(trimws(kv[2])))
    } else {
      param_names[j] <- ""
      param_vals[j]  <- suppressWarnings(as.numeric(trimws(part)))
    }
  }

  if (any(is.na(param_vals))) return(NULL)

  unnamed_idx <- which(param_names == "")
  if (length(unnamed_idx) > 0) {
    fam_key   <- tolower(gsub("-", "", family))
    canonical <- if (fam_key == "halfcauchy" && length(unnamed_idx) == 1) {
      c("scale")
    } else {
      switch(fam_key,
        "normal"       = c("mean", "sd"),
        "lognormal"    = c("meanlog", "sdlog"),
        "beta"         = c("alpha", "beta"),
        "gamma"        = c("shape", "rate"),
        "exponential"  = c("rate"),
        "halfnormal"   = c("sigma"),
        "cauchy"       = c("location", "scale"),
        "uniform"      = c("min", "max"),
        "weibull"      = c("shape", "scale"),
        "poisson"      = c("lambda"),
        "studentt"     = c("df", "location", "scale"),
        "halfcauchy"   = c("location", "scale"),
        "inversegamma" = c("shape", "scale"),
        "loglogistic"  = c("location", "scale"),
        "pareto"       = c("shape", "scale"),
        NULL
      )
    }
    for (k in seq_along(unnamed_idx)) {
      idx            <- unnamed_idx[k]
      param_names[idx] <- if (!is.null(canonical) && k <= length(canonical)) canonical[k] else paste0("p", idx)
    }
  }

  names(param_vals) <- param_names
  list(family = family, params = param_vals)
}

resolve_param <- function(params, ...) {
  for (a in c(...)) if (a %in% names(params)) return(params[[a]])
  NA_real_
}

# Returns NULL if params are valid, or a short error message string if not.
validate_dist_params <- function(family, params) {
  fam <- tolower(gsub("-", "", family))
  rp  <- function(...) resolve_param(params, ...)
  switch(fam,
    "normal"       = { sd <- rp("sd","sigma","scale","stdev"); if (is.na(sd)||sd<=0) "sd must be > 0" else NULL },
    "lognormal"    = { sl <- rp("sdlog","sigma","sd","scale"); if (is.na(sl)||sl<=0) "sdlog must be > 0" else NULL },
    "beta"         = { a <- rp("alpha","shape1","a"); b <- rp("beta","shape2","b"); if (anyNA(c(a,b))||a<=0||b<=0) "alpha and beta must be > 0" else NULL },
    "gamma"        = { sh <- rp("shape","alpha","a"); rt <- rp("rate","beta","b"); if (is.na(rt)){sc<-rp("scale");if(!is.na(sc)&&sc>0)rt<-1/sc}; if (anyNA(c(sh,rt))||sh<=0||rt<=0) "shape and rate must be > 0" else NULL },
    "exponential"  = { rt <- rp("rate","lambda","rate_alpha","rate_beta"); if (is.na(rt)||rt<=0) "rate must be > 0" else NULL },
    "halfnormal"   = { sg <- rp("sigma","sd","scale"); if (is.na(sg)||sg<=0) "sigma must be > 0" else NULL },
    "cauchy"       = { sc <- rp("scale","gamma","sigma"); if (is.na(sc)||sc<=0) "scale must be > 0" else NULL },
    "halfcauchy"   = { sc <- rp("scale","gamma","sigma"); if (is.na(sc)||sc<=0) "scale must be > 0" else NULL },
    "uniform"      = { mn <- rp("min","a","lower"); mx <- rp("max","b","upper"); if (anyNA(c(mn,mx))||mn>=mx) "min must be < max" else NULL },
    "weibull"      = { sh <- rp("shape","k","alpha"); sc <- rp("scale","lambda","beta"); if (anyNA(c(sh,sc))||sh<=0||sc<=0) "shape and scale must be > 0" else NULL },
    "poisson"      = { lm <- rp("lambda","rate","mean"); if (is.na(lm)||lm<=0) "lambda must be > 0" else NULL },
    "studentt"     = { df <- rp("df","nu"); if (is.na(df)||df<=0) "df must be > 0" else NULL },
    "inversegamma" = { sh <- rp("shape","alpha","a"); sc <- rp("scale","beta","b"); if (anyNA(c(sh,sc))||sh<=0||sc<=0) "shape and scale must be > 0" else NULL },
    "loglogistic"  = { mu <- rp("location","mu","alpha"); s <- rp("scale","sigma","beta","shape"); if (anyNA(c(mu,s))||mu<=0||s<=0) "location and scale must be > 0" else NULL },
    "pareto"       = { al <- rp("shape","alpha","k","a"); xm <- rp("scale","xmin","min","sigma"); if (anyNA(c(al,xm))||al<=0||xm<=0) "shape and scale (xmin) must be > 0" else NULL },
    NULL  # unknown family: treat as valid
  )
}

dist_density <- function(family, params, x) {
  fam <- tolower(gsub("-", "", family))
  tryCatch({
    switch(fam,
      "normal"       = { mu <- resolve_param(params,"mean","mu","location"); sd <- resolve_param(params,"sd","sigma","scale","stdev"); if (anyNA(c(mu,sd))||sd<=0) return(NULL); dnorm(x,mu,sd) },
      "lognormal"    = { ml <- resolve_param(params,"meanlog","mu","mean"); sl <- resolve_param(params,"sdlog","sigma","sd","scale"); if (anyNA(c(ml,sl))||sl<=0) return(NULL); dlnorm(x,ml,sl) },
      "beta"         = { a <- resolve_param(params,"alpha","shape1","a"); b <- resolve_param(params,"beta","shape2","b"); if (anyNA(c(a,b))||a<=0||b<=0) return(NULL); dbeta(x,a,b) },
      "gamma"        = { sh <- resolve_param(params,"shape","alpha","a"); rt <- resolve_param(params,"rate","beta","b"); if (is.na(rt)){sc<-resolve_param(params,"scale");if(!is.na(sc)&&sc>0)rt<-1/sc}; if (anyNA(c(sh,rt))||sh<=0||rt<=0) return(NULL); dgamma(x,sh,rt) },
      "exponential"  = { rt <- resolve_param(params,"rate","lambda","rate_alpha","rate_beta"); if (is.na(rt)||rt<=0) return(NULL); dexp(x,rt) },
      "halfnormal"   = { sg <- resolve_param(params,"sigma","sd","scale"); if (is.na(sg)||sg<=0) return(NULL); ifelse(x>=0,2*dnorm(x,0,sg),0) },
      "cauchy"       = { lo <- resolve_param(params,"location","x0","mu"); sc <- resolve_param(params,"scale","gamma","sigma"); if (is.na(lo)) lo<-0; if (is.na(sc)||sc<=0) return(NULL); dcauchy(x,lo,sc) },
      "halfcauchy"   = { lo <- resolve_param(params,"location","x0","mu"); sc <- resolve_param(params,"scale","gamma","sigma"); if (is.na(lo)) lo<-0; if (is.na(sc)||sc<=0) return(NULL); ifelse(x>=lo,2*dcauchy(x,lo,sc),0) },
      "uniform"      = { mn <- resolve_param(params,"min","a","lower"); mx <- resolve_param(params,"max","b","upper"); if (anyNA(c(mn,mx))||mn>=mx) return(NULL); dunif(x,mn,mx) },
      "weibull"      = { sh <- resolve_param(params,"shape","k","alpha"); sc <- resolve_param(params,"scale","lambda","beta"); if (anyNA(c(sh,sc))||sh<=0||sc<=0) return(NULL); dweibull(x,sh,sc) },
      "poisson"      = { lm <- resolve_param(params,"lambda","rate","mean"); if (is.na(lm)||lm<=0) return(NULL); dpois(round(x),lm) },
      "studentt"     = { df <- resolve_param(params,"df","nu"); lo <- resolve_param(params,"location","mu"); sc <- resolve_param(params,"scale","sigma"); if (is.na(df)||df<=0) return(NULL); if (is.na(lo)) lo<-0; if (is.na(sc)) sc<-1; dt((x-lo)/sc,df)/sc },
      "inversegamma" = { sh <- resolve_param(params,"shape","alpha","a"); sc <- resolve_param(params,"scale","beta","b"); if (anyNA(c(sh,sc))||sh<=0||sc<=0) return(NULL); ifelse(x>0,(sc^sh)/gamma(sh)*x^(-sh-1)*exp(-sc/x),0) },
      "loglogistic" = {
        mu <- resolve_param(params, "location", "mu", "alpha")
        s  <- resolve_param(params, "scale",    "sigma", "beta", "shape")
        if (anyNA(c(mu, s)) || mu <= 0 || s <= 0) return(NULL)
        ifelse(x > 0, (s / mu) * (x / mu)^(s - 1) / (1 + (x / mu)^s)^2, 0)
      },
      "pareto" = {
        al   <- resolve_param(params, "shape", "alpha", "k", "a")
        xmin <- resolve_param(params, "scale", "xmin",  "min", "sigma")
        if (anyNA(c(al, xmin)) || al <= 0 || xmin <= 0) return(NULL)
        ifelse(x >= xmin, al * xmin^al / x^(al + 1), 0)
      },
      NULL
    )
  }, error = function(e) NULL)
}

dist_xrange <- function(family, params) {
  fam <- tolower(gsub("-", "", family))
  tryCatch({
    switch(fam,
      "normal"       = { mu<-resolve_param(params,"mean","mu","location"); sd<-resolve_param(params,"sd","sigma","scale","stdev"); if(anyNA(c(mu,sd))||sd<=0)return(c(-5,5)); c(mu-4*sd,mu+4*sd) },
      "lognormal"    = { ml<-resolve_param(params,"meanlog","mu","mean"); sl<-resolve_param(params,"sdlog","sigma","sd","scale"); if(anyNA(c(ml,sl))||sl<=0)return(c(0,10)); c(0,qlnorm(0.999,ml,sl)) },
      "beta"         = c(0,1),
      "gamma"        = { sh<-resolve_param(params,"shape","alpha","a"); rt<-resolve_param(params,"rate","beta","b"); if(is.na(rt)){sc<-resolve_param(params,"scale");if(!is.na(sc)&&sc>0)rt<-1/sc}; if(anyNA(c(sh,rt))||sh<=0||rt<=0)return(c(0,10)); c(0,qgamma(0.999,sh,rt)) },
      "exponential"  = { rt<-resolve_param(params,"rate","lambda","rate_alpha","rate_beta"); if(is.na(rt)||rt<=0)return(c(0,10)); c(0,qexp(0.999,rt)) },
      "halfnormal"   = { sg<-resolve_param(params,"sigma","sd","scale"); if(is.na(sg)||sg<=0)return(c(0,5)); c(0,qnorm(0.9995,0,sg)) },
      "cauchy"       = { lo<-resolve_param(params,"location","x0","mu"); sc<-resolve_param(params,"scale","gamma","sigma"); if(is.na(lo))lo<-0; if(is.na(sc)||sc<=0)return(c(-10,10)); c(lo-10*sc,lo+10*sc) },
      "halfcauchy"   = { lo<-resolve_param(params,"location","x0","mu"); sc<-resolve_param(params,"scale","gamma","sigma"); if(is.na(lo))lo<-0; if(is.na(sc)||sc<=0)return(c(0,10)); c(lo,lo+20*sc) },
      "uniform"      = { mn<-resolve_param(params,"min","a","lower"); mx<-resolve_param(params,"max","b","upper"); if(anyNA(c(mn,mx)))return(c(0,1)); mg<-(mx-mn)*0.05; c(mn-mg,mx+mg) },
      "weibull"      = { sh<-resolve_param(params,"shape","k","alpha"); sc<-resolve_param(params,"scale","lambda","beta"); if(anyNA(c(sh,sc))||sh<=0||sc<=0)return(c(0,10)); c(0,qweibull(0.999,sh,sc)) },
      "poisson"      = { lm<-resolve_param(params,"lambda","rate","mean"); if(is.na(lm)||lm<=0)return(c(0,20)); c(0,qpois(0.999,lm)) },
      "studentt"     = { df<-resolve_param(params,"df","nu"); lo<-resolve_param(params,"location","mu"); sc<-resolve_param(params,"scale","sigma"); if(is.na(df)||df<=0)return(c(-5,5)); if(is.na(lo))lo<-0; if(is.na(sc))sc<-1; c(lo-6*sc,lo+6*sc) },
      "inversegamma" = { sh<-resolve_param(params,"shape","alpha","a"); sc<-resolve_param(params,"scale","beta","b"); if(anyNA(c(sh,sc))||sh<=0||sc<=0)return(c(0,10)); if(sh>2){ig_mean<-sc/(sh-1);ig_var<-sc^2/((sh-1)^2*(sh-2));c(0,ig_mean+4*sqrt(ig_var))}else c(0,sc*10) },
      "loglogistic" = {
        mu <- resolve_param(params, "location", "mu", "alpha")
        s  <- resolve_param(params, "scale",    "sigma", "beta", "shape")
        if (anyNA(c(mu, s)) || mu <= 0 || s <= 0) return(c(0, 10))
        upper <- mu * (0.999 / 0.001)^(1 / s)
        c(0, min(upper, mu * 100))
      },
      "pareto" = {
        al   <- resolve_param(params, "shape", "alpha", "k", "a")
        xmin <- resolve_param(params, "scale", "xmin",  "min", "sigma")
        if (anyNA(c(al, xmin)) || al <= 0 || xmin <= 0) return(c(0, 10))
        upper <- xmin * (100)^(1 / al)
        c(xmin * 0.95, min(upper, xmin * 200))
      },
      c(-5, 5)
    )
  }, error = function(e) c(-5, 5))
}

# Shared pooling helpers (used by opinion_pooling.R and delphi.R)

# Resolve a human-readable label for a distribution group.
# Returns "Family (param_label)" if a label is available, else "Family" or "Family #N".
resolve_fam_label <- function(family, param_idx, grp_dists) {
  param_labels <- Filter(function(l) !is.null(l) && nchar(l) > 0,
                         lapply(grp_dists, `[[`, "param_label"))
  if (length(param_labels) > 0) {
    best <- names(sort(table(unlist(param_labels)), decreasing = TRUE))[1]
    paste0(family, " (", best, ")")
  } else {
    if (param_idx > 1) paste0(family, " #", param_idx) else family
  }
}

# Compute per-group pooled parameters from a parsed distributions list.
# Groups by (family, param_idx) with param_idx normalised to integer.
# Only valid distributions (per validate_dist_params) contribute to averaging.
# Returns a list of group entries, each: list(family, param_idx, fam_label,
#   avg_params, n_valid, n_total, dist_str, label, all_valid).
# When n_valid == 0 or all param names are empty, avg_params and dist_str are NULL
# (group should be skipped / shown as a warning in the Stan prompt).
compute_group_pools <- function(all_dists) {
  groups <- unique(lapply(all_dists, function(d)
    list(family = d$family, param_idx = as.integer(d$param_idx))
  ))
  lapply(groups, function(grp) {
    grp_dists <- Filter(
      function(d) d$family == grp$family && as.integer(d$param_idx) == grp$param_idx,
      all_dists
    )
    valid   <- Filter(function(d) is.null(validate_dist_params(d$family, d$params)), grp_dists)
    n_valid <- length(valid)
    n_total <- length(grp_dists)

    param_labels <- Filter(function(l) !is.null(l) && nchar(l) > 0,
                           lapply(grp_dists, `[[`, "param_label"))
    label <- if (length(param_labels) > 0)
      names(sort(table(unlist(param_labels)), decreasing = TRUE))[1]
    else NULL

    fam_label <- resolve_fam_label(grp$family, grp$param_idx, grp_dists)

    # Skip group when no valid distributions or no param names
    if (n_valid == 0) {
      return(list(family = grp$family, param_idx = grp$param_idx,
                  fam_label = fam_label, avg_params = NULL,
                  n_valid = 0L, n_total = n_total,
                  dist_str = NULL, label = label, all_valid = FALSE))
    }
    all_param_names <- unique(unlist(lapply(valid, function(d) names(d$params))))
    if (length(all_param_names) == 0) {
      return(list(family = grp$family, param_idx = grp$param_idx,
                  fam_label = fam_label, avg_params = NULL,
                  n_valid = 0L, n_total = n_total,
                  dist_str = NULL, label = label, all_valid = FALSE))
    }
    avg_params <- sapply(all_param_names, function(pname) {
      vals <- sapply(valid, function(d)
        if (pname %in% names(d$params)) d$params[[pname]] else NA_real_
      )
      mean(vals, na.rm = TRUE)
    })
    param_str <- paste(names(avg_params), "=",
                       format(avg_params, digits = 4, nsmall = 2), collapse = ", ")
    list(family    = grp$family,
         param_idx = grp$param_idx,
         fam_label = fam_label,
         avg_params = avg_params,
         n_valid   = n_valid,
         n_total   = n_total,
         dist_str  = paste0(grp$family, "(", param_str, ")"),
         label     = label,
         all_valid = (n_valid == n_total))
  })
}

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
source("pages/delphi.R")

env_url <- Sys.getenv("BASE_URL")
env_key <- Sys.getenv("API_KEY")

`%||%` <- function(x, y) if (is.null(x)) y else x

extract_stan <- function(text) {
  m <- regmatches(text, regexpr("(?s)```(?:stan)?[ \t]*\n(.+?)\n```", text, perl = TRUE))
  if (length(m) > 0) sub("^```(?:stan)?[ \t]*\n", "", sub("\n```$", "", m)) else text
}

render_markdown <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) return(NULL)
  # Convert LLM dollar-delimited math to MathJax-compatible delimiters
  # $$...$$ must be processed before $...$ to avoid double-conversion
  text <- gsub("\\$\\$([\\s\\S]+?)\\$\\$", "\\\\[\\1\\\\]", text, perl = TRUE)
  text <- gsub("\\$([^$\n]+?)\\$", "\\\\(\\1\\\\)", text, perl = TRUE)
  markdown(text)
}

proposal_prompt <- function(clinical_context, data_variables = "") {
  data_section <- if (nchar(trimws(data_variables)) > 0)
    paste0("## Available Data Variables\n", data_variables, "\n\n")
  else ""
  paste0(
    "You are a biostatistics expert participating in a Bayesian model design and prior elicitation exercise for a clinical trial.\n\n",
    "## Clinical Context\n",
    clinical_context, "\n\n",
    data_section,
    "## Your Task\n\n",
    "**1. Propose a Statistical Model**\n",
    "Based on the clinical context and available data variables above, propose an appropriate statistical model for the primary outcome of interest. ",
    "Describe the likelihood, any hierarchical structure (e.g. patients nested within sites), link functions, and all model parameters using mathematical notation.\n\n",
    "**2. Prior Distributions**\n",
    "For every parameter you named in the model above, specify an informative prior distribution grounded in published clinical evidence, registries, or established domain knowledge for this disease setting and population.\n\n",
    "**Important instructions:**\n",
    "- Priors must be **informative** — not weakly-informative or non-informative. Avoid vague or default values without justification.\n",
    "- Base each prior on realistic data or strong prior experience relevant to the specific setting, population, and endpoint described.\n",
    "- If the model includes a control arm, anchor priors to observed rates from comparable published trials or registries.\n\n",
    "Format each prior on its own line using exactly:\n",
    "`parameter_name ~ DistributionName(param1=value, param2=value)`\n\n",
    "Do not omit any parameter. Specify priors for all hyperparameters if the model is hierarchical."
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

  candidate_lines <- search_lines[
    grepl("(?i)(beta|normal|log[- ]?normal|gamma|cauchy|exponential|uniform|weibull|poisson)", search_lines, perl = TRUE) &
    grepl("[0-9]", search_lines)
  ]
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
      c(-5, 5)
    )
  }, error = function(e) c(-5, 5))
}

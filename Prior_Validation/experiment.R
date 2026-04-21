# =============================================================================
# experiment.R
#
# Validation pipeline for the hierarchical Poisson-Gamma model.
# Implements two experiment skeletons from the Arai et al. paper:
#   1. Stratified K-fold cross-validation (Table 1)
#   2. Sample-efficiency curve (Table 2)
#
# This file is pure R logic -- no Shiny reactives. It is intended to be
# sourced from a driver script or Shiny server that passes a compiled
# stanmodel object and the patient-level data frame.
#
# Dependencies: rstan
# =============================================================================

library(rstan)


# =============================================================================
# compute_lpd
# =============================================================================
# Computes the Log Predictive Density (LPD) for a vector of held-out patient
# AE counts using posterior samples of the Gamma hyperparameters alpha and
# beta (Equations 5-6, Arai et al.).
#
# For each held-out patient with observed count y_obs[k]:
#   1. Draw lambda_new^(i) ~ Gamma(alpha^(i), rate = beta^(i)) for each
#      posterior sample i = 1..S.
#   2. Compute log P(y_obs[k] | lambda_new^(i)) = Poisson log-pmf.
#   3. Aggregate via log-mean-exp for numerical stability:
#        LPD_k = log( (1/S) * sum_i exp(log P(y_obs[k] | lambda_new^(i))) )
#              = log_sum_exp(log_probs) - log(S)
#
# Arguments:
#   y_obs          integer vector of held-out patient AE counts
#   alpha_samples  numeric vector of S posterior draws for alpha
#   beta_samples   numeric vector of S posterior draws for beta (rate param)
#
# Returns:
#   Numeric vector of per-patient LPD values (length == length(y_obs)).
#   The mean of this vector is the aggregate LPD reported in Table 1/2.
compute_lpd <- function(y_obs, alpha_samples, beta_samples) {
  stopifnot(length(alpha_samples) == length(beta_samples))
  stopifnot(length(alpha_samples) > 0)

  S <- length(alpha_samples)

  lambda_matrix <- matrix(
    rgamma(S * length(y_obs), shape = alpha_samples, rate = beta_samples),
    nrow = S,
    ncol = length(y_obs)
  )

  log_prob_matrix <- matrix(
    dpois(
      rep(y_obs, each = S),
      lambda = as.vector(lambda_matrix),
      log = TRUE
    ),
    nrow = S,
    ncol = length(y_obs)
  )

  apply(log_prob_matrix, 2, function(log_probs) {
    max_lp <- max(log_probs)
    max_lp + log(mean(exp(log_probs - max_lp)))
  })
}


# =============================================================================
# check_rhat
# =============================================================================
#' Check Gelman-Rubin R-hat convergence diagnostic for the Gamma hyperparameters.
#'
#' @param fit  A stanfit object from rstan::sampling()
#' @return     Named list:
#'               rhat_alpha  -- R-hat for alpha
#'               rhat_beta   -- R-hat for beta
#'               rhat_max    -- max R-hat across alpha and beta
#'               converged   -- TRUE if rhat_max < 1.1
check_rhat <- function(fit) {
  s <- rstan::summary(fit)$summary
  if (!"Rhat" %in% colnames(s)) {
    warning("[check_rhat] Rhat column not found in Stan summary. Skipping convergence check.")
    return(list(rhat_alpha = NA_real_, rhat_beta = NA_real_, rhat_max = NA_real_, converged = NA))
  }
  rhat_alpha <- if ("alpha" %in% rownames(s)) unname(s["alpha", "Rhat"]) else NA_real_
  rhat_beta  <- if ("beta"  %in% rownames(s)) unname(s["beta",  "Rhat"]) else NA_real_
  rhat_max   <- max(rhat_alpha, rhat_beta, na.rm = TRUE)
  if (!is.finite(rhat_max)) rhat_max <- NA_real_
  list(
    rhat_alpha = rhat_alpha,
    rhat_beta  = rhat_beta,
    rhat_max   = rhat_max,
    converged  = if (is.na(rhat_max)) NA else rhat_max < 1.1
  )
}


# =============================================================================
# 1. stratify_sites
# =============================================================================
#' Compute per-site patient counts and assign strata.
#'
#' Strata follow Appendix B.2 of Arai et al.:
#'   - "small":  n_patients <= 2
#'   - "medium": n_patients 3-4
#'   - "large":  n_patients >= 5
#'
#' @param data  Data frame with columns: patient_id, site_id, ae_count
#' @return      Data frame with columns: site_id, n_patients, stratum
#'              (one row per site, sorted by site_id)
stratify_sites <- function(data) {
  stopifnot(all(c("patient_id", "site_id", "ae_count") %in% names(data)))

  site_df <- aggregate(
    patient_id ~ site_id,
    data = data,
    FUN = length
  )
  names(site_df) <- c("site_id", "n_patients")

  site_df$stratum <- ifelse(
    site_df$n_patients <= 2, "small",
    ifelse(site_df$n_patients <= 4, "medium", "large")
  )

  site_df <- site_df[order(site_df$site_id), ]
  rownames(site_df) <- NULL
  site_df
}


# =============================================================================
# 2. make_cv_folds
# =============================================================================
#' Assign sites to K folds with stratification by site-size stratum.
#'
#' @param site_strata  Data frame from stratify_sites()
#' @param n_folds      Number of folds (default 5)
#' @param seed         RNG seed (default 42)
#' @return             Named integer vector: names = site_id, values = fold 1..K
make_cv_folds <- function(site_strata, n_folds = 5, seed = 42) {
  stopifnot(n_folds >= 2)
  stopifnot(nrow(site_strata) >= n_folds)

  set.seed(seed)

  fold_assignment <- integer(nrow(site_strata))
  names(fold_assignment) <- as.character(site_strata$site_id)

  for (stratum_name in unique(site_strata$stratum)) {
    idx <- which(site_strata$stratum == stratum_name)
    shuffled <- sample(idx)
    fold_assignment[shuffled] <- rep_len(seq_len(n_folds), length(shuffled))
  }

  fold_assignment
}


# =============================================================================
# 3. make_stan_data
# =============================================================================
#' Build the named list expected by the poisson_gamma.stan data block.
#'
#' @param train_data   Patient-level data frame (patient_id, site_id, ae_count)
#' @param rate_alpha   Exponential hyperprior rate for alpha (scalar > 0)
#' @param rate_beta    Exponential hyperprior rate for beta  (scalar > 0)
make_stan_data <- function(train_data, rate_alpha, rate_beta) {
  stopifnot(all(c("patient_id", "site_id", "ae_count") %in% names(train_data)))
  stopifnot(is.numeric(rate_alpha), length(rate_alpha) == 1, rate_alpha > 0)
  stopifnot(is.numeric(rate_beta),  length(rate_beta)  == 1, rate_beta  > 0)

  # BUG 10.3 -- Validate ae_count before passing to Stan
  y_int <- as.integer(train_data$ae_count)
  if (any(is.na(y_int)))
    stop("ae_count contains NA or non-integer values. Check your data before fitting Stan.")

  unique_sites <- sort(unique(train_data$site_id))
  J <- length(unique_sites)
  site_map <- setNames(seq_len(J), as.character(unique_sites))

  list(
    N          = nrow(train_data),
    J          = J,
    y          = y_int,
    site       = as.integer(site_map[as.character(train_data$site_id)]),
    rate_alpha = rate_alpha,
    rate_beta  = rate_beta
  )
}


# =============================================================================
# 4. run_cv
# =============================================================================
#' Stratified K-fold cross-validation for the hierarchical Poisson-Gamma model.
#'
#' @param data         Patient-level data frame (patient_id, site_id, ae_count)
#' @param rate_alpha   Exponential hyperprior rate for alpha (scalar > 0)
#' @param rate_beta    Exponential hyperprior rate for beta  (scalar > 0)
#' @param n_folds      Number of CV folds (default 5)
#' @param stan_model   Compiled stanmodel object (from rstan::stan_model())
#' @param seed         RNG seed (default 42)
#' @param chains       Number of MCMC chains (default 4)
#' @param iter         Total iterations per chain (default 2000)
#' @param warmup       Warmup iterations per chain (default 1000)
#' @return             Data frame: fold, n_test_patients, lpd_mean, lpd_sd,
#'                     rate_alpha, rate_beta
run_cv <- function(data,
                   rate_alpha,
                   rate_beta,
                   n_folds    = 5,
                   stan_model,
                   seed       = 42,
                   chains     = 4,
                   iter       = 2000,
                   warmup     = 1000) {

  site_strata <- stratify_sites(data)
  fold_vec    <- make_cv_folds(site_strata, n_folds = n_folds, seed = seed)

  results <- vector("list", n_folds)

  for (k in seq_len(n_folds)) {
    message(sprintf("[CV] Fitting fold %d / %d ...", k, n_folds))

    test_site_ids  <- names(fold_vec[fold_vec == k])
    train_site_ids <- names(fold_vec[fold_vec != k])

    train_data <- data[as.character(data$site_id) %in% train_site_ids, ]
    test_data  <- data[as.character(data$site_id) %in% test_site_ids,  ]

    # BUG 10.4 -- Guard against empty test set
    if (nrow(test_data) == 0)
      stop(sprintf("Fold %d: test set is empty. Check that n_folds (%d) <= number of sites.", k, n_folds))

    stan_data <- make_stan_data(train_data, rate_alpha, rate_beta)

    fit_result <- tryCatch(
      withCallingHandlers(
        {
          fit <- rstan::sampling(
            stan_model,
            data    = stan_data,
            chains  = chains,
            iter    = iter,
            warmup  = warmup,
            seed    = seed + k,
            refresh = 0
          )

          posterior   <- rstan::extract(fit, pars = c("alpha", "beta"))
          alpha_draws <- posterior$alpha
          beta_draws  <- posterior$beta

          y_test   <- as.integer(test_data$ae_count)
          lpd_vals <- compute_lpd(y_test, alpha_draws, beta_draws)

          rg <- check_rhat(fit)
          if (!rg$converged) {
            warning(sprintf("[CV] Fold %d: R-hat convergence warning (max R-hat = %.3f)", k, rg$rhat_max))
          }

          list(
            lpd_mean        = mean(lpd_vals),
            lpd_sd          = sd(lpd_vals),
            n_test_patients = length(y_test),
            rhat_alpha      = rg$rhat_alpha,
            rhat_beta       = rg$rhat_beta,
            rhat_max        = rg$rhat_max,
            converged       = rg$converged
          )
        },
        warning = function(w) {
          # Demote rstan ESS / divergence warnings to messages so the fold
          # result is still returned rather than aborting.
          message(sprintf("[CV] Fold %d warning: %s", k, conditionMessage(w)))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        warning(sprintf("[CV] Fold %d failed: %s", k, conditionMessage(e)))
        list(
          lpd_mean        = NA_real_,
          lpd_sd          = NA_real_,
          n_test_patients = nrow(test_data),
          rhat_alpha      = NA_real_,
          rhat_beta       = NA_real_,
          rhat_max        = NA_real_,
          converged       = NA
        )
      }
    )

    results[[k]] <- data.frame(
      fold            = k,
      n_test_patients = fit_result$n_test_patients,
      lpd_mean        = fit_result$lpd_mean,
      lpd_sd          = fit_result$lpd_sd,
      rhat_alpha      = fit_result$rhat_alpha,
      rhat_beta       = fit_result$rhat_beta,
      rhat_max        = fit_result$rhat_max,
      converged       = fit_result$converged,
      rate_alpha      = rate_alpha,
      rate_beta       = rate_beta,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, results)
}


# =============================================================================
# 5. run_sample_efficiency
# =============================================================================
#' Sample-efficiency experiment: fit on progressively larger training subsets.
#'
#' @param data              Patient-level data frame (patient_id, site_id, ae_count)
#' @param rate_alpha        Exponential hyperprior rate for alpha (scalar > 0)
#' @param rate_beta         Exponential hyperprior rate for beta  (scalar > 0)
#' @param subsample_levels  Fractions of training sites to use
#' @param n_reps            Replications per subsample level (default 20)
#' @param train_frac        Outer train/test split fraction (default 0.7)
#' @param stan_model        Compiled stanmodel object
#' @param seed              RNG seed (default 42)
#' @param chains            MCMC chains (default 4)
#' @param iter              Iterations per chain (default 2000)
#' @param warmup            Warmup iterations (default 1000)
#' @return                  Data frame: subsample_level, rep, n_train_patients,
#'                          lpd_mean, lpd_sd, rate_alpha, rate_beta
run_sample_efficiency <- function(data,
                                  rate_alpha,
                                  rate_beta,
                                  subsample_levels = c(0.2, 0.4, 0.6, 0.8, 1.0),
                                  n_reps           = 20,
                                  train_frac       = 0.7,
                                  stan_model,
                                  seed             = 42,
                                  chains           = 4,
                                  iter             = 2000,
                                  warmup           = 1000) {

  site_strata <- stratify_sites(data)
  set.seed(seed)

  train_site_ids <- character(0)
  test_site_ids  <- character(0)

  for (stratum_name in unique(site_strata$stratum)) {
    stratum_sites <- as.character(site_strata$site_id[site_strata$stratum == stratum_name])
    n_train       <- max(1, round(length(stratum_sites) * train_frac))
    shuffled      <- sample(stratum_sites)
    train_site_ids <- c(train_site_ids, shuffled[seq_len(n_train)])
    if (n_train < length(shuffled))
      test_site_ids <- c(test_site_ids, shuffled[(n_train + 1):length(shuffled)])
  }

  test_data <- data[as.character(data$site_id) %in% test_site_ids, ]
  y_test    <- as.integer(test_data$ae_count)

  if (length(y_test) == 0)
    stop("Test set is empty after the 70/30 split. Check data and train_frac.")

  full_train_data <- data[as.character(data$site_id) %in% train_site_ids, ]
  train_strata    <- site_strata[as.character(site_strata$site_id) %in% train_site_ids, ]

  message(sprintf(
    "[Sample Efficiency] %d training sites (%d patients), %d test sites (%d patients)",
    length(train_site_ids), nrow(full_train_data),
    length(test_site_ids), nrow(test_data)
  ))

  results    <- vector("list", length(subsample_levels) * n_reps)
  result_idx <- 0L

  for (level in subsample_levels) {
    for (rep_i in seq_len(n_reps)) {

      result_idx <- result_idx + 1L
      rep_seed   <- seed + result_idx

      message(sprintf(
        "[Sample Efficiency] level=%.0f%%, rep=%d/%d ...",
        level * 100, rep_i, n_reps
      ))

      set.seed(rep_seed)
      subsample_site_ids <- character(0)

      for (stratum_name in unique(train_strata$stratum)) {
        stratum_sites <- as.character(
          train_strata$site_id[train_strata$stratum == stratum_name]
        )
        n_pick <- max(1, round(length(stratum_sites) * level))
        picked <- sample(stratum_sites, size = n_pick, replace = FALSE)
        subsample_site_ids <- c(subsample_site_ids, picked)
      }

      sub_train_data <- full_train_data[
        as.character(full_train_data$site_id) %in% subsample_site_ids,
      ]

      stan_data <- make_stan_data(sub_train_data, rate_alpha, rate_beta)

      fit_result <- tryCatch(
        withCallingHandlers(
          {
            fit <- rstan::sampling(
              stan_model,
              data    = stan_data,
              chains  = chains,
              iter    = iter,
              warmup  = warmup,
              seed    = rep_seed,
              refresh = 0
            )

            posterior   <- rstan::extract(fit, pars = c("alpha", "beta"))
            alpha_draws <- posterior$alpha
            beta_draws  <- posterior$beta

            lpd_vals <- compute_lpd(y_test, alpha_draws, beta_draws)

            rg <- check_rhat(fit)
            if (!rg$converged) {
              warning(sprintf("[Sample Efficiency] level=%.0f%% rep=%d: R-hat convergence warning (max R-hat = %.3f)",
                              level * 100, rep_i, rg$rhat_max))
            }

            list(
              n_train_patients = nrow(sub_train_data),
              lpd_mean         = mean(lpd_vals),
              lpd_sd           = sd(lpd_vals),
              rhat_alpha       = rg$rhat_alpha,
              rhat_beta        = rg$rhat_beta,
              rhat_max         = rg$rhat_max,
              converged        = rg$converged
            )
          },
          warning = function(w) {
            message(sprintf("[Sample Efficiency] level=%.0f%% rep=%d warning: %s",
                            level * 100, rep_i, conditionMessage(w)))
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          warning(sprintf(
            "[Sample Efficiency] level=%.0f%% rep=%d failed: %s",
            level * 100, rep_i, conditionMessage(e)
          ))
          list(
            n_train_patients = nrow(sub_train_data),
            lpd_mean         = NA_real_,
            lpd_sd           = NA_real_,
            rhat_alpha       = NA_real_,
            rhat_beta        = NA_real_,
            rhat_max         = NA_real_,
            converged        = NA
          )
        }
      )

      results[[result_idx]] <- data.frame(
        subsample_level  = level,
        rep              = rep_i,
        n_train_patients = fit_result$n_train_patients,
        lpd_mean         = fit_result$lpd_mean,
        lpd_sd           = fit_result$lpd_sd,
        rhat_alpha       = fit_result$rhat_alpha,
        rhat_beta        = fit_result$rhat_beta,
        rhat_max         = fit_result$rhat_max,
        converged        = fit_result$converged,
        rate_alpha       = rate_alpha,
        rate_beta        = rate_beta,
        stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, results)
}


# =============================================================================
# 6. summarise_cv
# =============================================================================
#' Aggregate fold-level CV results into a single summary (Table 1 format).
#'
#' @param cv_results  Data frame from run_cv()
#' @return            Named list: lpd_mean, lpd_sd
summarise_cv <- function(cv_results) {
  stopifnot("lpd_mean" %in% names(cv_results))

  valid <- cv_results$lpd_mean[!is.na(cv_results$lpd_mean)]

  if (length(valid) == 0) {
    warning("[summarise_cv] All folds returned NA.")
    return(list(lpd_mean = NA_real_, lpd_sd = NA_real_))
  }

  list(lpd_mean = mean(valid), lpd_sd = sd(valid))
}


# =============================================================================
# 7. summarise_sample_efficiency
# =============================================================================
#' Aggregate rep-level results per subsample level (Table 2 format).
#'
#' @param se_results  Data frame from run_sample_efficiency()
#' @return            Data frame: subsample_level, lpd_mean, lpd_sd
summarise_sample_efficiency <- function(se_results) {
  stopifnot(all(c("subsample_level", "lpd_mean") %in% names(se_results)))

  levels <- sort(unique(se_results$subsample_level))

  rows <- lapply(levels, function(lev) {
    valid <- se_results$lpd_mean[
      se_results$subsample_level == lev & !is.na(se_results$lpd_mean)
    ]

    if (length(valid) == 0) {
      data.frame(subsample_level = lev, lpd_mean = NA_real_, lpd_sd = NA_real_,
                 stringsAsFactors = FALSE)
    } else {
      data.frame(subsample_level = lev, lpd_mean = mean(valid), lpd_sd = sd(valid),
                 stringsAsFactors = FALSE)
    }
  })

  do.call(rbind, rows)
}


# =============================================================================
# 8. run_robustness
# =============================================================================
#' Prior robustness analysis: evaluate LPD sensitivity across a hyperprior grid.
#'
#' Runs stratified K-fold CV for each combination of rate_alpha and rate_beta
#' in the supplied grids. Returns one row per grid point with aggregate LPD
#' statistics and convergence diagnostics.
#'
#' @param data             Patient-level data frame (patient_id, site_id, ae_count)
#' @param alpha_grid       Numeric vector of rate_alpha values to evaluate
#' @param beta_grid        Numeric vector of rate_beta values to evaluate
#' @param n_folds          Number of CV folds (default 5)
#' @param stan_model       Compiled stanmodel object (from rstan::stan_model())
#' @param seed             RNG seed (default 42)
#' @param chains           Number of MCMC chains (default 4)
#' @param iter             Total iterations per chain (default 2000)
#' @param warmup           Warmup iterations per chain (default 1000)
#' @param progress_fn      Optional callback: function(current, total, msg) called
#'                         after each grid point finishes. Used by Shiny for
#'                         progress bar updates.
#' @return                 Data frame with columns: rate_alpha, rate_beta,
#'                         lpd_mean, lpd_sd, rhat_max, n_converged, n_folds,
#'                         all_converged
run_robustness <- function(data,
                           alpha_grid,
                           beta_grid,
                           n_folds    = 5,
                           stan_model,
                           seed       = 42,
                           chains     = 4,
                           iter       = 2000,
                           warmup     = 1000,
                           progress_fn = NULL) {

  stopifnot(length(alpha_grid) > 0, length(beta_grid) > 0)
  stopifnot(all(alpha_grid > 0), all(beta_grid > 0))

  grid <- expand.grid(rate_alpha = alpha_grid, rate_beta = beta_grid,
                      KEEP.OUT.ATTRS = FALSE)
  total <- nrow(grid)

  results <- vector("list", total)

  for (i in seq_len(total)) {
    ra <- grid$rate_alpha[i]
    rb <- grid$rate_beta[i]

    msg <- sprintf("[Robustness] Grid point %d/%d: rate_alpha=%.4f, rate_beta=%.4f",
                   i, total, ra, rb)
    message(msg)

    cv_res <- tryCatch(
      withCallingHandlers(
        run_cv(data, rate_alpha = ra, rate_beta = rb,
               n_folds = n_folds, stan_model = stan_model,
               seed = seed, chains = chains, iter = iter, warmup = warmup),
        warning = function(w) {
          message(sprintf("[Robustness] Grid point %d warning: %s", i, conditionMessage(w)))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        warning(sprintf("[Robustness] Grid point %d failed: %s", i, conditionMessage(e)))
        NULL
      }
    )

    if (!is.null(cv_res)) {
      summary    <- summarise_cv(cv_res)
      rhat_max   <- max(cv_res$rhat_max, na.rm = TRUE)
      if (!is.finite(rhat_max)) rhat_max <- NA_real_
      n_conv     <- sum(cv_res$converged, na.rm = TRUE)
      n_total_f  <- sum(!is.na(cv_res$converged))
    } else {
      summary   <- list(lpd_mean = NA_real_, lpd_sd = NA_real_)
      rhat_max  <- NA_real_
      n_conv    <- 0L
      n_total_f <- 0L
    }

    results[[i]] <- data.frame(
      rate_alpha    = ra,
      rate_beta     = rb,
      lpd_mean      = summary$lpd_mean,
      lpd_sd        = summary$lpd_sd,
      rhat_max      = rhat_max,
      n_converged   = n_conv,
      n_folds       = n_total_f,
      all_converged = (n_conv == n_total_f && n_total_f > 0),
      stringsAsFactors = FALSE
    )

    # Report progress to caller (Shiny progress bar)
    if (is.function(progress_fn)) {
      progress_fn(i, total, msg)
    }
  }

  do.call(rbind, results)
}

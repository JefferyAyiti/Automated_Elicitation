library(shiny)
library(shinyjs)
library(rstan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

source("experiment.R")

function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Reactive: uploaded data (raw CSV) -- SHARED between both tabs
  # ---------------------------------------------------------------------------
  val_raw_data <- reactive({
    req(input$val_file)
    tryCatch(
      read.csv(input$val_file$datapath, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
  })

  # Dynamic column selectors once a file is uploaded
  output$val_col_patient <- renderUI({
    df <- val_raw_data()
    if (is.null(df)) return(NULL)
    cols <- names(df)
    default_pat <- grep("(?i)pat|patient|id", cols, value = TRUE, perl = TRUE)[1]
    selectInput("val_patient_col", "Patient ID col:", choices = cols,
                selected = if (!is.na(default_pat)) default_pat else cols[1])
  })

  output$val_col_site <- renderUI({
    df <- val_raw_data()
    if (is.null(df)) return(NULL)
    cols <- names(df)
    default_site <- grep("(?i)site|center|hosp", cols, value = TRUE, perl = TRUE)[1]
    selectInput("val_site_col", "Site ID col:", choices = cols,
                selected = if (!is.na(default_site)) default_site else cols[min(2, length(cols))])
  })

  output$val_col_ae <- renderUI({
    df <- val_raw_data()
    if (is.null(df)) return(NULL)
    cols <- names(df)
    default_ae <- grep("(?i)ae|count|adverse|event", cols, value = TRUE, perl = TRUE)[1]
    selectInput("val_ae_col", "AE count col:", choices = cols,
                selected = if (!is.na(default_ae)) default_ae else cols[min(3, length(cols))])
  })

  output$val_data_preview <- renderUI({
    df <- val_raw_data()
    if (is.null(df)) return(NULL)
    req(input$val_patient_col, input$val_site_col, input$val_ae_col)
    n_patients <- nrow(df)
    n_sites    <- length(unique(df[[input$val_site_col]]))
    div(
      class = "alert alert-info p-2 mt-1",
      style = "font-size: 0.85em;",
      sprintf("Loaded: %d patients across %d sites.", n_patients, n_sites)
    )
  })

  # ---------------------------------------------------------------------------
  # Reactive: remapped data with canonical column names for experiment.R
  # SHARED between both tabs
  # ---------------------------------------------------------------------------
  val_data <- reactive({
    df <- val_raw_data()
    if (is.null(df)) return(NULL)
    req(input$val_patient_col, input$val_site_col, input$val_ae_col)

    # Ensure selected columns are distinct
    if (length(unique(c(input$val_patient_col, input$val_site_col, input$val_ae_col))) < 3) {
      showNotification("Patient ID, Site ID, and AE count must be mapped to different columns.", type = "error")
      return(NULL)
    }

    # Detect non-numeric ae_count values before coercion
    ae_raw    <- df[[input$val_ae_col]]
    ae_coerced <- suppressWarnings(as.integer(ae_raw))
    n_lost <- sum(is.na(ae_coerced)) - sum(is.na(ae_raw))
    if (n_lost > 0)
      showNotification(
        sprintf("%d row(s) dropped: non-numeric values in AE count column.", n_lost),
        type = "warning", duration = 8
      )

    out <- data.frame(
      patient_id = df[[input$val_patient_col]],
      site_id    = as.character(df[[input$val_site_col]]),
      ae_count   = ae_coerced,
      stringsAsFactors = FALSE
    )
    out[!is.na(out$ae_count), ]
  })

  # ===========================================================================
  # TAB 1: VALIDATION (existing logic, unchanged)
  # ===========================================================================

  val_cv_results <- reactiveVal(NULL)
  val_se_results <- reactiveVal(NULL)
  val_running    <- reactiveVal(FALSE)

  observeEvent(input$val_run_btn, {
    data <- val_data()
    if (is.null(data) || nrow(data) == 0) {
      showNotification("Please upload a valid dataset first.", type = "error")
      return()
    }

    rate_alpha <- input$rate_alpha
    rate_beta  <- input$rate_beta
    if (is.na(rate_alpha) || rate_alpha <= 0 || is.na(rate_beta) || rate_beta <= 0) {
      showNotification("rate_alpha and rate_beta must be positive numbers.", type = "error")
      return()
    }

    # Validate MCMC settings: warmup must be strictly less than iterations
    if (input$mcmc_warmup >= input$mcmc_iter) {
      showNotification("Warmup must be less than iterations.", type = "error")
      return()
    }

    # Validate subsample levels: at least one must be selected
    if (length(input$se_subsample_levels) == 0) {
      showNotification("Select at least one subsample level.", type = "error")
      return()
    }

    # Ensure CV folds does not exceed number of sites
    n_sites <- length(unique(data$site_id))
    if (input$cv_folds > n_sites) {
      showNotification(
        sprintf("CV folds (%d) exceeds number of sites (%d). Reduce CV folds.", input$cv_folds, n_sites),
        type = "error"
      )
      return()
    }

    stan_file <- "poisson_gamma.stan"
    if (!file.exists(stan_file)) {
      showNotification("Stan model file not found.", type = "error")
      return()
    }

    val_running(TRUE)
    val_cv_results(NULL)
    val_se_results(NULL)
    shinyjs::disable("val_run_btn")

    withProgress(message = "Compiling Stan model...", value = 0, {
      sm <- tryCatch(
        rstan::stan_model(file = stan_file),
        error = function(e) { showNotification(paste("Stan error:", e$message), type = "error"); NULL }
      )
      if (is.null(sm)) { val_running(FALSE); shinyjs::enable("val_run_btn"); return() }

      incProgress(0.2, message = "Running cross-validation...")
      cv_res <- tryCatch(
        run_cv(data, rate_alpha, rate_beta, stan_model = sm,
               n_folds = input$cv_folds,
               chains = input$mcmc_chains, iter = input$mcmc_iter, warmup = input$mcmc_warmup),
        error = function(e) { showNotification(paste("CV error:", e$message), type = "error"); NULL }
      )
      val_cv_results(cv_res)

      incProgress(0.5, message = "Running sample efficiency experiment...")
      se_res <- tryCatch(
        run_sample_efficiency(data, rate_alpha, rate_beta,
                              subsample_levels = as.numeric(input$se_subsample_levels),
                              n_reps = input$se_n_reps, stan_model = sm,
                              chains = input$mcmc_chains, iter = input$mcmc_iter,
                              warmup = input$mcmc_warmup),
        error = function(e) { showNotification(paste("SE error:", e$message), type = "error"); NULL }
      )
      val_se_results(se_res)

      incProgress(1, message = "Done.")
    })

    val_running(FALSE)
    shinyjs::enable("val_run_btn")
  })

  # ---------------------------------------------------------------------------
  # CV results table (Table 1)
  # ---------------------------------------------------------------------------
  output$val_cv_table_ui <- renderUI({
    if (val_running()) {
      return(div(class = "text-muted", "Running..."))
    }
    cv <- val_cv_results()
    if (is.null(cv)) {
      return(p("Upload data and click 'Run Validation'.", class = "text-muted", style = "font-size:0.88em;"))
    }
    summary <- summarise_cv(cv)
    df_display <- data.frame(
      Fold       = cv$fold,
      `N Test`   = cv$n_test_patients,
      `LPD Mean` = round(cv$lpd_mean, 4),
      `LPD SD`   = round(cv$lpd_sd, 4),
      `Rhat Max` = round(cv$rhat_max, 3),
      check.names = FALSE
    )

    # Convergence warning alert (if any fold failed to converge)
    n_warn <- sum(!cv$converged, na.rm = TRUE)
    warn_box <- NULL
    if (n_warn > 0) {
      warn_box <- div(class = "alert alert-warning p-2 mb-2", style = "font-size:0.85em;",
        sprintf("\u26A0 %d fold(s) did not converge (R-hat \u2265 1.1). Results may be unreliable.", n_warn))
    }

    tagList(
      warn_box,
      div(
        class = "alert alert-success p-2 mb-2",
        style = "font-size: 0.85em;",
        sprintf("Overall LPD: %.4f \u00B1 %.4f", summary$lpd_mean, summary$lpd_sd)
      ),
      tags$table(
        class = "table table-sm table-bordered table-striped",
        tags$thead(tags$tr(lapply(names(df_display), tags$th))),
        tags$tbody(apply(df_display, 1, function(row) {
          tags$tr(lapply(row, tags$td))
        }))
      )
    )
  })

  # ---------------------------------------------------------------------------
  # Sample efficiency table (Table 2)
  # ---------------------------------------------------------------------------
  output$val_se_table_ui <- renderUI({
    if (val_running()) {
      return(div(class = "text-muted", "Running..."))
    }
    se <- val_se_results()
    if (is.null(se)) {
      return(p("Upload data and click 'Run Validation'.", class = "text-muted", style = "font-size:0.88em;"))
    }
    summary <- summarise_sample_efficiency(se)
    summary$subsample_pct <- paste0(round(summary$subsample_level * 100), "%")

    # Per-level max R-hat computed inline from raw SE results
    rhat_by_level <- tapply(se$rhat_max, se$subsample_level, max, na.rm = TRUE)
    summary$rhat_max <- unname(rhat_by_level[as.character(summary$subsample_level)])

    df_display <- data.frame(
      `Sample %` = summary$subsample_pct,
      `LPD Mean` = round(summary$lpd_mean, 4),
      `LPD SD`   = round(summary$lpd_sd, 4),
      `Rhat Max` = round(summary$rhat_max, 3),
      check.names = FALSE
    )

    # Convergence warning alert (if any rep failed to converge)
    n_warn <- sum(!se$converged, na.rm = TRUE)
    warn_box <- NULL
    if (n_warn > 0) {
      warn_box <- div(class = "alert alert-warning p-2 mb-2", style = "font-size:0.85em;",
        sprintf("\u26A0 %d replicate(s) did not converge (R-hat \u2265 1.1).", n_warn))
    }

    tagList(
      warn_box,
      tags$table(
        class = "table table-sm table-bordered table-striped",
        tags$thead(tags$tr(lapply(names(df_display), tags$th))),
        tags$tbody(apply(df_display, 1, function(row) {
          tags$tr(lapply(row, tags$td))
        }))
      )
    )
  })

  # ---------------------------------------------------------------------------
  # Plot drawing helpers (shared between renderPlot and downloadHandler)
  # ---------------------------------------------------------------------------
  draw_cv_plot <- function() {
    cv <- val_cv_results()
    req(cv)
    valid_idx <- !is.na(cv$lpd_mean)
    if (sum(valid_idx) == 0) return(invisible(NULL))
    folds   <- cv$fold[valid_idx]
    means   <- cv$lpd_mean[valid_idx]
    sds     <- cv$lpd_sd[valid_idx]; sds[is.na(sds)] <- 0
    overall <- summarise_cv(cv)
    y_lo    <- means - sds; y_hi <- means + sds
    y_range <- range(c(y_lo, y_hi), na.rm = TRUE)
    y_pad   <- diff(y_range) * 0.15
    if (y_pad == 0) y_pad <- abs(y_range[1]) * 0.1 + 0.01
    par(mar = c(4, 4.5, 2.5, 1))
    plot(folds, means,
         xlim = c(min(folds) - 0.5, max(folds) + 0.5),
         ylim = c(y_range[1] - y_pad, y_range[2] + y_pad),
         xlab = "Fold", ylab = "LPD", main = "Per-Fold LPD",
         pch = 19, col = "#2c7bb6", cex = 1.4, bty = "l", xaxt = "n",
         cex.lab = 1.1, cex.main = 1.2)
    axis(1, at = folds)
    arrows(folds, y_lo, folds, y_hi, angle = 90, code = 3,
           length = 0.06, col = "#2c7bb6", lwd = 1.5)
    if (!is.na(overall$lpd_mean)) {
      abline(h = overall$lpd_mean, lty = 2, col = "#d7191c", lwd = 1.5)
      legend("bottomright",
             legend = sprintf("Overall mean: %.4f", overall$lpd_mean),
             lty = 2, col = "#d7191c", lwd = 1.5, bty = "n", cex = 0.85)
    }
  }

  draw_se_plot <- function() {
    se <- val_se_results()
    req(se)
    summary   <- summarise_sample_efficiency(se)
    valid_idx <- !is.na(summary$lpd_mean)
    if (sum(valid_idx) == 0) return(invisible(NULL))
    summary <- summary[valid_idx, , drop = FALSE]
    x_pct   <- summary$subsample_level * 100
    means   <- summary$lpd_mean
    sds     <- summary$lpd_sd; sds[is.na(sds)] <- 0
    y_lo    <- means - sds; y_hi <- means + sds
    y_range <- range(c(y_lo, y_hi), na.rm = TRUE)
    y_pad   <- diff(y_range) * 0.15
    if (y_pad == 0) y_pad <- abs(y_range[1]) * 0.1 + 0.01
    par(mar = c(4, 4.5, 2.5, 1))
    plot(x_pct, means,
         xlim = c(min(x_pct) - 5, max(x_pct) + 5),
         ylim = c(y_range[1] - y_pad, y_range[2] + y_pad),
         xlab = "Training Data (%)", ylab = "LPD", main = "Sample Efficiency",
         type = "n", bty = "l", xaxt = "n", cex.lab = 1.1, cex.main = 1.2)
    axis(1, at = x_pct)
    polygon(c(x_pct, rev(x_pct)), c(y_hi, rev(y_lo)),
            col = rgb(0.17, 0.48, 0.71, 0.2), border = NA)
    lines(x_pct, means, col = "#2c7bb6", lwd = 2)
    points(x_pct, means, pch = 19, col = "#2c7bb6", cex = 1.3)
  }

  # ---------------------------------------------------------------------------
  # CV diagnostic plot
  # ---------------------------------------------------------------------------
  output$val_cv_plot <- renderPlot({
    req(!val_running())
    draw_cv_plot()
  })

  # ---------------------------------------------------------------------------
  # Sample Efficiency diagnostic plot
  # ---------------------------------------------------------------------------
  output$val_se_plot <- renderPlot({
    req(!val_running())
    draw_se_plot()
  })

  # ---------------------------------------------------------------------------
  # Download handlers (Validation tab)
  # ---------------------------------------------------------------------------
  output$dl_cv_plot <- downloadHandler(
    filename = "cv_lpd_plot.png",
    content  = function(file) {
      png(file, width = 800, height = 520, res = 120)
      on.exit(dev.off(), add = TRUE)
      draw_cv_plot()
    }
  )

  output$dl_se_plot <- downloadHandler(
    filename = "sample_efficiency_plot.png",
    content  = function(file) {
      png(file, width = 800, height = 520, res = 120)
      on.exit(dev.off(), add = TRUE)
      draw_se_plot()
    }
  )

  output$dl_cv_csv <- downloadHandler(
    filename = function() sprintf("cv_results_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")),
    content  = function(file) {
      cv <- val_cv_results()
      if (is.null(cv)) {
        showNotification("No CV results to download. Run validation first.", type = "error")
        return()
      }
      out <- cv
      out$mcmc_chains  <- input$mcmc_chains
      out$mcmc_iter    <- input$mcmc_iter
      out$mcmc_warmup  <- input$mcmc_warmup
      out$cv_folds     <- input$cv_folds
      out$dataset_file <- if (!is.null(input$val_file)) input$val_file$name else NA_character_
      out$exported_at  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      write.csv(out, file, row.names = FALSE)
    }
  )

  output$dl_se_csv <- downloadHandler(
    filename = function() sprintf("se_results_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")),
    content  = function(file) {
      se <- val_se_results()
      if (is.null(se)) {
        showNotification("No SE results to download. Run validation first.", type = "error")
        return()
      }
      out <- se
      out$mcmc_chains  <- input$mcmc_chains
      out$mcmc_iter    <- input$mcmc_iter
      out$mcmc_warmup  <- input$mcmc_warmup
      out$se_n_reps    <- input$se_n_reps
      out$dataset_file <- if (!is.null(input$val_file)) input$val_file$name else NA_character_
      out$exported_at  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      write.csv(out, file, row.names = FALSE)
    }
  )

  # ===========================================================================
  # TAB 2: PRIOR ROBUSTNESS
  # ===========================================================================

  rob_results <- reactiveVal(NULL)
  rob_running <- reactiveVal(FALSE)

  # ---------------------------------------------------------------------------
  # Helper: parse a comma-separated string into a sorted numeric vector.
  # Returns NULL if parsing fails or any value is non-positive.
  # ---------------------------------------------------------------------------
  parse_grid_values <- function(text_input) {
    if (is.null(text_input) || nchar(trimws(text_input)) == 0) return(NULL)
    parts <- trimws(unlist(strsplit(text_input, ",")))
    vals  <- suppressWarnings(as.numeric(parts))
    if (any(is.na(vals)) || any(vals <= 0)) return(NULL)
    sort(unique(vals))
  }

  # ---------------------------------------------------------------------------
  # Grid preview: show how many grid points will be evaluated
  # ---------------------------------------------------------------------------
  output$rob_grid_preview <- renderUI({
    alpha_vals <- parse_grid_values(input$rob_alpha_grid)
    beta_vals  <- parse_grid_values(input$rob_beta_grid)
    if (is.null(alpha_vals) || is.null(beta_vals)) {
      return(div(class = "alert alert-warning p-2 mt-1", style = "font-size: 0.85em;",
                 "Enter valid comma-separated positive numbers for both grids."))
    }
    n_total <- length(alpha_vals) * length(beta_vals)
    div(
      class = "alert alert-info p-2 mt-1",
      style = "font-size: 0.85em;",
      sprintf("Grid: %d alpha x %d beta = %d combinations.",
              length(alpha_vals), length(beta_vals), n_total)
    )
  })

  # ---------------------------------------------------------------------------
  # Run robustness analysis
  # ---------------------------------------------------------------------------
  observeEvent(input$rob_run_btn, {
    data <- val_data()
    if (is.null(data) || nrow(data) == 0) {
      showNotification("Please upload a valid dataset first.", type = "error")
      return()
    }

    alpha_vals <- parse_grid_values(input$rob_alpha_grid)
    beta_vals  <- parse_grid_values(input$rob_beta_grid)
    if (is.null(alpha_vals)) {
      showNotification("Invalid rate_alpha grid. Enter comma-separated positive numbers.", type = "error")
      return()
    }
    if (is.null(beta_vals)) {
      showNotification("Invalid rate_beta grid. Enter comma-separated positive numbers.", type = "error")
      return()
    }

    # Validate MCMC settings
    if (input$rob_mcmc_warmup >= input$rob_mcmc_iter) {
      showNotification("Warmup must be less than iterations.", type = "error")
      return()
    }

    # Validate CV folds vs number of sites
    n_sites <- length(unique(data$site_id))
    if (input$rob_cv_folds > n_sites) {
      showNotification(
        sprintf("CV folds (%d) exceeds number of sites (%d). Reduce CV folds.",
                input$rob_cv_folds, n_sites),
        type = "error"
      )
      return()
    }

    stan_file <- "poisson_gamma.stan"
    if (!file.exists(stan_file)) {
      showNotification("Stan model file not found.", type = "error")
      return()
    }

    n_total <- length(alpha_vals) * length(beta_vals)

    rob_running(TRUE)
    rob_results(NULL)
    shinyjs::disable("rob_run_btn")

    withProgress(message = "Compiling Stan model...", value = 0, {
      sm <- tryCatch(
        rstan::stan_model(file = stan_file),
        error = function(e) { showNotification(paste("Stan error:", e$message), type = "error"); NULL }
      )
      if (is.null(sm)) {
        rob_running(FALSE)
        shinyjs::enable("rob_run_btn")
        return()
      }

      incProgress(0.05, message = sprintf("Running robustness grid (%d points)...", n_total))

      # progress_fn callback updates the Shiny progress bar after each grid point
      progress_fn <- function(current, total, msg) {
        incProgress(0.90 / total,
                    message = sprintf("Grid point %d / %d ...", current, total))
      }

      rob_res <- tryCatch(
        run_robustness(data,
                       alpha_grid = alpha_vals,
                       beta_grid  = beta_vals,
                       n_folds    = input$rob_cv_folds,
                       stan_model = sm,
                       chains     = input$rob_mcmc_chains,
                       iter       = input$rob_mcmc_iter,
                       warmup     = input$rob_mcmc_warmup,
                       progress_fn = progress_fn),
        error = function(e) {
          showNotification(paste("Robustness error:", e$message), type = "error")
          NULL
        }
      )

      rob_results(rob_res)
      incProgress(1, message = "Done.")
    })

    rob_running(FALSE)
    shinyjs::enable("rob_run_btn")
  })

  # ---------------------------------------------------------------------------
  # Convergence warning
  # ---------------------------------------------------------------------------
  output$rob_convergence_warning <- renderUI({
    res <- rob_results()
    if (is.null(res)) return(NULL)
    n_not_converged <- sum(!res$all_converged, na.rm = TRUE)
    if (n_not_converged == 0) return(NULL)
    div(
      class = "alert alert-warning p-2 mb-2",
      style = "font-size: 0.85em;",
      sprintf("\u26A0 %d of %d grid point(s) had at least one fold with R-hat \u2265 1.1. Check the Rhat Max column.",
              n_not_converged, nrow(res))
    )
  })

  # ---------------------------------------------------------------------------
  # Heatmap drawing helper (base R only)
  # ---------------------------------------------------------------------------
  draw_rob_heatmap <- function() {
    res <- rob_results()
    req(res)
    valid <- !is.na(res$lpd_mean)
    if (sum(valid) == 0) return(invisible(NULL))

    alpha_vals <- sort(unique(res$rate_alpha))
    beta_vals  <- sort(unique(res$rate_beta))
    na <- length(alpha_vals)
    nb <- length(beta_vals)

    # Build the LPD matrix: rows = rate_alpha, cols = rate_beta
    z_mat <- matrix(NA_real_, nrow = na, ncol = nb)
    for (i in seq_len(nrow(res))) {
      ri <- match(res$rate_alpha[i], alpha_vals)
      ci <- match(res$rate_beta[i],  beta_vals)
      z_mat[ri, ci] <- res$lpd_mean[i]
    }

    # filled.contour requires at least 2 rows and 2 cols for interpolation.
    # For degenerate grids (single row or col), fall back to image().
    if (na < 2 || nb < 2) {
      par(mar = c(5, 5, 3, 1))
      z_range <- range(z_mat, na.rm = TRUE)
      col_pal <- colorRampPalette(
        c("#2166ac", "#67a9cf", "#d1e5f0", "#f7f7f7",
          "#fddbc7", "#ef8a62", "#b2182b"))(64)
      image(seq_along(alpha_vals), seq_along(beta_vals), z_mat,
            col = col_pal, xlab = expression(rate_alpha),
            ylab = expression(rate_beta),
            main = "LPD Sensitivity to Hyperprior Rates",
            xaxt = "n", yaxt = "n", cex.main = 1.2, cex.lab = 1.1)
      axis(1, at = seq_along(alpha_vals), labels = alpha_vals, las = 2, cex.axis = 0.9)
      axis(2, at = seq_along(beta_vals),  labels = beta_vals, las = 1, cex.axis = 0.9)
      for (ri in seq_len(na)) {
        for (ci in seq_len(nb)) {
          val <- z_mat[ri, ci]
          if (!is.na(val)) text(ri, ci, sprintf("%.3f", val), cex = 0.8, col = "grey20")
        }
      }
      return(invisible(NULL))
    }

    # Color palette: blue (low/worse) to red (high/better) with white midpoint
    # Use filled.contour for the heatmap.
    # filled.contour reserves space for the color key on the right and calls
    # plot.axes for custom axis/overlay inside the plot region.
    filled.contour(
      x = seq_along(alpha_vals),
      y = seq_along(beta_vals),
      z = z_mat,
      color.palette = function(n) colorRampPalette(
        c("#2166ac", "#67a9cf", "#d1e5f0", "#f7f7f7",
          "#fddbc7", "#ef8a62", "#b2182b"))(n),
      xlab = "",
      ylab = "",
      main = "LPD Sensitivity to Hyperprior Rates",
      cex.main = 1.2,
      plot.axes = {
        axis(1, at = seq_along(alpha_vals), labels = alpha_vals, las = 2, cex.axis = 0.9)
        axis(2, at = seq_along(beta_vals),  labels = beta_vals, las = 1, cex.axis = 0.9)

        # Annotate each cell with LPD value
        for (ri in seq_len(na)) {
          for (ci in seq_len(nb)) {
            val <- z_mat[ri, ci]
            if (!is.na(val)) {
              text(ri, ci, sprintf("%.3f", val), cex = 0.7, col = "grey20")
            }
          }
        }

        # Mark baseline (0.1, 0.1) with a prominent symbol if it is in the grid
        bl_ri <- match(0.1, alpha_vals)
        bl_ci <- match(0.1, beta_vals)
        if (!is.na(bl_ri) && !is.na(bl_ci)) {
          points(bl_ri, bl_ci, pch = 4, cex = 2.5, col = "black", lwd = 3)
          text(bl_ri, bl_ci, "baseline", pos = 3, cex = 0.75, col = "black", font = 2, offset = 0.7)
        }
      },
      plot.title = {
        title(xlab = expression(rate_alpha), line = 3.5, cex.lab = 1.1)
        title(ylab = expression(rate_beta),  line = 3.5, cex.lab = 1.1)
      }
    )
  }

  # ---------------------------------------------------------------------------
  # Heatmap UI container (only show plotOutput when results exist)
  # ---------------------------------------------------------------------------
  output$rob_heatmap_ui <- renderUI({
    if (rob_running()) {
      return(div(class = "text-muted", "Running robustness analysis..."))
    }
    res <- rob_results()
    if (is.null(res)) {
      return(p("Configure the hyperprior grid and click 'Run Robustness Analysis'.",
               class = "text-muted", style = "font-size:0.88em;"))
    }
    # Dynamic height based on grid size for readability
    beta_vals <- sort(unique(res$rate_beta))
    plot_h <- max(360, length(beta_vals) * 55 + 100)
    plotOutput("rob_heatmap_plot", height = paste0(plot_h, "px"))
  })

  output$rob_heatmap_plot <- renderPlot({
    req(!rob_running())
    draw_rob_heatmap()
  })

  # ---------------------------------------------------------------------------
  # Results table
  # ---------------------------------------------------------------------------
  output$rob_table_ui <- renderUI({
    if (rob_running()) {
      return(div(class = "text-muted", "Running..."))
    }
    res <- rob_results()
    if (is.null(res)) {
      return(p("No results yet.", class = "text-muted", style = "font-size:0.88em;"))
    }

    # Find the best LPD (highest mean) for highlighting
    best_idx <- which.max(res$lpd_mean)

    df_display <- data.frame(
      `rate_alpha` = res$rate_alpha,
      `rate_beta`  = res$rate_beta,
      `LPD Mean`   = round(res$lpd_mean, 4),
      `LPD SD`     = round(res$lpd_sd, 4),
      `Rhat Max`   = round(res$rhat_max, 3),
      `Conv.`      = ifelse(res$all_converged, "\u2713", "\u26A0"),
      check.names  = FALSE
    )

    # Build table rows, highlight best and baseline
    rows <- lapply(seq_len(nrow(df_display)), function(i) {
      row_class <- ""
      if (i == best_idx) row_class <- "table-success"
      # Mark baseline row
      is_baseline <- (res$rate_alpha[i] == 0.1 && res$rate_beta[i] == 0.1)
      cells <- lapply(df_display[i, ], function(val) tags$td(as.character(val)))
      if (is_baseline) {
        # Append a small baseline marker to the first cell
        cells[[1]] <- tags$td(tags$strong(as.character(df_display[i, 1])),
                              tags$span(" (baseline)", style = "font-size:0.75em; color:#888;"))
      }
      tags$tr(class = row_class, cells)
    })

    # Summary: best grid point
    best_row <- res[best_idx, ]
    summary_box <- div(
      class = "alert alert-info p-2 mb-2",
      style = "font-size: 0.85em;",
      sprintf("Best LPD: %.4f at rate_alpha=%.4f, rate_beta=%.4f",
              best_row$lpd_mean, best_row$rate_alpha, best_row$rate_beta)
    )

    tagList(
      summary_box,
      div(
        style = "max-height: 500px; overflow-y: auto;",
        tags$table(
          class = "table table-sm table-bordered table-striped",
          tags$thead(tags$tr(lapply(names(df_display), tags$th))),
          tags$tbody(rows)
        )
      )
    )
  })

  # ---------------------------------------------------------------------------
  # Download handlers (Robustness tab)
  # ---------------------------------------------------------------------------
  output$dl_rob_plot <- downloadHandler(
    filename = "robustness_heatmap.png",
    content  = function(file) {
      png(file, width = 900, height = 700, res = 120)
      on.exit(dev.off(), add = TRUE)
      draw_rob_heatmap()
    }
  )

  output$dl_rob_csv <- downloadHandler(
    filename = function() sprintf("robustness_results_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")),
    content  = function(file) {
      res <- rob_results()
      if (is.null(res)) {
        showNotification("No robustness results to download. Run analysis first.", type = "error")
        return()
      }
      out <- res
      out$mcmc_chains  <- input$rob_mcmc_chains
      out$mcmc_iter    <- input$rob_mcmc_iter
      out$mcmc_warmup  <- input$rob_mcmc_warmup
      out$cv_folds     <- input$rob_cv_folds
      out$dataset_file <- if (!is.null(input$val_file)) input$val_file$name else NA_character_
      out$exported_at  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      write.csv(out, file, row.names = FALSE)
    }
  )
}

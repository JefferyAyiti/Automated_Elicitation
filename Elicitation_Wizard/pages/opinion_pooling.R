# =============================================================================
# Page: Opinion Pooling
# UI and server logic for the "Opinion Pooling" tab.
# Implements linear opinion pooling: f(theta) = sum w_i f_i(theta), equal weights.
# Groups by (family, param_idx) so e.g. Exponential(rate_alpha) and
# Exponential(rate_beta) are pooled separately.
# Depends on shared reactive: parsed_distributions()
# =============================================================================

opinion_pooling_ui <- function() {
  tabPanel(
    "Opinion Pooling",
    br(),
    div(
      style = "display: flex; justify-content: space-between; align-items: baseline; margin-bottom: 4px;",
      h4("Opinion Pooling \u2014 Linear Combination"),
      uiOutput("pooling_stan_btn_ui")
    ),
    uiOutput("pooling_summary_ui"),
    plotOutput("pooling_plot"),
    uiOutput("pooling_stan_card_ui")
  )
}

opinion_pooling_server <- function(input, output, session, parsed_distributions,
                                   active_url, active_key, elicitation_context) {

  # ---------------------------------------------------------------------------
  # Stan code generation state
  # ---------------------------------------------------------------------------
  pooling_stan_result    <- reactiveVal(NULL)   # list(code) or NULL
  pooling_stan_in_flight <- reactiveVal(FALSE)

  # ---------------------------------------------------------------------------
  # compute_group_pools() and resolve_fam_label() are defined in global.R
  # and shared with delphi.R.
  # ---------------------------------------------------------------------------

  # ---------------------------------------------------------------------------
  # Stan button: rendered independently so it is never re-created by data updates
  # ---------------------------------------------------------------------------
  output$pooling_stan_btn_ui <- renderUI({
    in_flight <- pooling_stan_in_flight()
    btn_label <- if (in_flight) {
      tagList(tags$span(class = "expert-spinner"))
    } else {
      tagList(icon("code"), " Generate Stan Code")
    }
    actionButton("pooling_stan_btn", btn_label,
                 class = "btn btn-sm btn-outline-dark")
  })

  # ---------------------------------------------------------------------------
  # Stan code generation observer
  # ---------------------------------------------------------------------------
  observeEvent(input$pooling_stan_btn, {
    url     <- active_url()
    api_key <- active_key()
    if (nchar(url) == 0 || nchar(api_key) == 0) {
      showNotification("Please provide a Base URL and API Key.", type = "error")
      return()
    }

    all_dists <- parsed_distributions()
    if (length(all_dists) == 0) {
      showNotification("Run elicitation first.", type = "warning")
      return()
    }

    ctx <- elicitation_context()
    model_spec_section <- if (!is.null(ctx) && nchar(trimws(ctx$parameter_topic)) > 0) {
      paste0("## Model Specification\n", ctx$parameter_topic, "\n\n")
    } else {
      showNotification(
        "No model specification found. Stan code will be generated without a model spec.",
        type = "warning", duration = 6
      )
      ""
    }

    pools <- compute_group_pools(all_dists)
    pooled_lines <- vapply(pools, function(p) {
      if (is.null(p$dist_str)) {
        # All experts invalid for this group — include as a warning comment
        return(paste0("  # SKIPPED: ", p$fam_label,
                      " (all ", p$n_total, " expert(s) had invalid parameters)"))
      }
      label_suffix <- if (!is.null(p$label) && nchar(p$label) > 0)
        paste0(" [", p$label, "]") else ""
      paste0("  ", p$dist_str, label_suffix,
             "  # pooled from ", p$n_valid, "/", p$n_total, " valid expert(s)")
    }, character(1))

    stan_prompt <- paste0(
      "You are an expert Stan programmer. Below is a Bayesian model specification ",
      "and pooled prior distributions obtained via linear opinion pooling across multiple experts.\n\n",
      model_spec_section,
      "## Pooled Prior Distributions (equal-weight linear opinion pool)\n",
      paste(pooled_lines, collapse = "\n"), "\n\n",
      "Your task:\n",
      "1. Translate this into a complete, valid Stan model.\n",
      "2. Use the pooled prior distributions exactly as specified.\n",
      "3. Include a `generated quantities` block that computes `log_lik` ",
      "(log-likelihood per observation) to enable LOO-CV with the `loo` package.\n",
      "4. Add brief inline comments explaining each block.\n\n",
      "Respond with **only** the Stan model code in a single ```stan code block. ",
      "Do not include any explanation outside the code block."
    )

    pooling_stan_result(NULL)
    pooling_stan_in_flight(TRUE)
    shinyjs::disable("pooling_stan_btn")

    fut_url      <- url
    fut_key      <- api_key
    fut_model_id <- stan_coder_model_id
    fut_prompt   <- stan_prompt

    p <- promises::future_promise({
      library(ellmer)
      chat_obj <- chat_openai_compatible(
        base_url      = fut_url,
        name          = "CHAT-AI",
        credentials   = local({ k <- fut_key; function() k }),
        model         = fut_model_id,
        system_prompt = "You are an expert Stan and R programmer."
      )
      chat_obj$chat(fut_prompt)
    }, seed = NULL)

    promises::then(p,
      onFulfilled = function(response_text) {
        pooling_stan_result(list(code = response_text))
        pooling_stan_in_flight(FALSE)
        shinyjs::enable("pooling_stan_btn")
      },
      onRejected = function(err) {
        pooling_stan_result(list(code = paste("Error:", conditionMessage(err))))
        pooling_stan_in_flight(FALSE)
        shinyjs::enable("pooling_stan_btn")
      }
    )
    NULL
  }, ignoreInit = TRUE)

  # ---------------------------------------------------------------------------
  # Opinion pooling summary table
  # ---------------------------------------------------------------------------
  output$pooling_summary_ui <- renderUI({
    all_dists <- parsed_distributions()
    if (length(all_dists) == 0) {
      return(p("No results yet. Run elicitation first.", class = "text-muted"))
    }

    pools  <- compute_group_pools(all_dists)
    # Build a lookup from "family:param_idx" -> pool entry for O(1) access
    pool_map <- list()
    for (pl in pools) pool_map[[paste0(pl$family, ":", pl$param_idx)]] <- pl

    table_rows <- list()
    for (pl in pools) {
      grp_key   <- paste0(pl$family, ":", pl$param_idx)
      grp_dists <- Filter(
        function(d) d$family == pl$family && as.integer(d$param_idx) == pl$param_idx,
        all_dists
      )
      n_valid <- pl$n_valid
      n_total <- pl$n_total

      for (d in grp_dists) {
        param_str <- paste(names(d$params), "=",
                           format(d$params, digits = 4, nsmall = 2), collapse = ", ")
        err <- validate_dist_params(d$family, d$params)
        is_valid <- is.null(err)
        valid_badge <- if (is_valid) {
          tags$span(style = "color: green; font-weight: bold;", "\u2713")
        } else {
          tags$span(style = "color: darkorange;", tags$b("\u26a0"), " ", err)
        }
        weight_cell <- if (is_valid) {
          paste0("1/", n_valid)
        } else {
          tags$em(style = "color: darkorange; font-size: 0.85em;", "excluded")
        }
        table_rows[[length(table_rows) + 1]] <- tags$tr(
          tags$td(pl$fam_label),
          tags$td(paste0("Expert ", d$expert_num)),
          tags$td(d$model_label),
          tags$td(weight_cell),
          tags$td(tags$code(style = "font-size: 0.85em;", param_str)),
          tags$td(valid_badge)
        )
      }

      # Pooled row
      if (is.null(pl$avg_params)) {
        # All invalid — show skipped row
        table_rows[[length(table_rows) + 1]] <- tags$tr(
          style = "background-color: #fff3cd;",
          tags$td(pl$fam_label),
          tags$td(tags$span(style = "color: darkorange;", "Skipped (0/", n_total, " valid)")),
          tags$td("\u2014"), tags$td("\u2014"),
          tags$td(tags$em(style = "color: #888;", "no valid distributions")),
          tags$td("\u2014")
        )
      } else {
        avg_param_str <- paste(names(pl$avg_params), "=",
                               format(pl$avg_params, digits = 4, nsmall = 2), collapse = ", ")
        pooled_source_label <- if (n_valid < n_total) {
          tags$span(style = "color: darkorange;",
                    paste0("Pooled (", n_valid, "/", n_total, " valid)"))
        } else {
          paste0("Pooled (n=", n_total, ")")
        }
        table_rows[[length(table_rows) + 1]] <- tags$tr(
          style = "background-color: #f0f0f0; font-weight: bold;",
          tags$td(pl$fam_label),
          tags$td(pooled_source_label),
          tags$td("\u2014"),
          tags$td("Equal"),
          tags$td(tags$code(style = "font-size: 0.85em;", avg_param_str)),
          tags$td("\u2014")
        )
      }
    }

    tagList(
      p(style = "font-size: 0.9em; color: #555; margin-bottom: 8px;",
        "The pooled prior is formed by linear opinion pooling: ",
        tags$em("f(\u03b8) = \u03a3 w\u1d62 f\u1d62(\u03b8)"),
        " with equal weights ", tags$em("w\u1d62 = 1/n"),
        " across the n ", tags$strong("valid"), " experts for each distribution."),
      tags$table(
        class = "table table-bordered table-sm",
        tags$thead(tags$tr(
          tags$th("Family"), tags$th("Source"), tags$th("Model"),
          tags$th("Weight"), tags$th("Parameters"), tags$th("Valid")
        )),
        tags$tbody(table_rows)
      )
    )
  })

  # ---------------------------------------------------------------------------
  # Opinion pooling density plot
  # ---------------------------------------------------------------------------
  plot_height <- reactive({
    all_dists <- parsed_distributions()
    if (length(all_dists) == 0) return(200)
    n_grps <- length(unique(lapply(all_dists, function(d)
      list(family = d$family, param_idx = as.integer(d$param_idx))
    )))
    n_col <- if (n_grps <= 1) 1 else if (n_grps == 2) 2 else 3
    n_row <- ceiling(n_grps / n_col)
    max(350, n_row * 320)
  })

  output$pooling_plot <- renderPlot(height = function() plot_height(), {
    all_dists <- parsed_distributions()
    if (length(all_dists) == 0) {
      par(mar = c(1, 1, 1, 1)); plot.new()
      text(0.5, 0.5,
           "No distributions could be parsed from the expert responses.\nRun elicitation first.",
           cex = 1.2, col = "grey40", font = 3)
      return(invisible(NULL))
    }

    base_colors   <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                       "#9467bd", "#8c564b", "#e377c2", "#7f7f7f")
    expert_idxs   <- as.integer(sapply(all_dists, `[[`, "expert_idx"))
    expert_idxs   <- expert_idxs[is.finite(expert_idxs) & expert_idxs > 0L]
    n_experts     <- if (length(expert_idxs) > 0L) max(expert_idxs) else 1L
    expert_colors <- rep_len(base_colors, n_experts)

    plot_groups <- unique(lapply(all_dists, function(d)
      list(family = d$family, param_idx = as.integer(d$param_idx))
    ))
    n_fam <- length(plot_groups)

    if (n_fam <= 1)        { n_row <- 1; n_col <- 1
    } else if (n_fam == 2) { n_row <- 1; n_col <- 2
    } else if (n_fam <= 4) { n_row <- 2; n_col <- 2
    } else if (n_fam <= 6) { n_row <- 2; n_col <- 3
    } else                 { n_row <- ceiling(n_fam / 3); n_col <- 3 }

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    par(mfrow = c(n_row, n_col), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))

    for (grp in plot_groups) {
      fam       <- grp$family
      grp_dists <- Filter(function(d) d$family == fam && as.integer(d$param_idx) == grp$param_idx, all_dists)
      is_discrete <- tolower(gsub("-", "", fam)) == "poisson"
      fam_label   <- resolve_fam_label(fam, grp$param_idx, grp_dists)
      n_exp       <- length(grp_dists)

      xranges <- lapply(grp_dists, function(d) {
        xr <- dist_xrange(fam, d$params)
        if (any(!is.finite(xr))) c(-5, 5) else xr
      })
      global_xmin <- min(sapply(xranges, `[`, 1))
      global_xmax <- max(sapply(xranges, `[`, 2))
      if (!is.finite(global_xmin) || !is.finite(global_xmax) ||
          global_xmin >= global_xmax) { global_xmin <- -5; global_xmax <- 5 }

      if (is_discrete) {
        x_int <- seq(max(0, floor(global_xmin)), ceiling(global_xmax))
        y_vals_list <- lapply(grp_dists, function(d) {
          yv <- dist_density(fam, d$params, x_int)
          if (is.null(yv)) NULL else yv
        })
        valid_mask    <- !sapply(y_vals_list, is.null)
        y_vals_valid  <- y_vals_list[valid_mask]
        grp_dists_v   <- grp_dists[valid_mask]
        n_valid       <- length(y_vals_valid)
        if (n_valid == 0) next
        pooled_y <- Reduce(`+`, y_vals_valid) / n_valid
        max_y <- max(unlist(y_vals_valid), pooled_y, na.rm = TRUE)
        if (!is.finite(max_y) || max_y <= 0) max_y <- 1

        plot(NULL, xlim = c(min(x_int) - 0.5, max(x_int) + 0.5),
             ylim = c(0, max_y * 1.15), xlab = "Value", ylab = "Probability",
             main = paste(fam_label, "- Pooled"), xaxt = "n")
        axis(1, at = x_int); grid(ny = NULL, nx = NA)
        if (n_valid < n_exp)
          mtext(paste0(n_exp - n_valid, " expert(s) excluded (invalid params)"),
                side = 3, line = -1.2, cex = 0.75, col = "darkorange")

        offsets <- seq(-0.15, 0.15, length.out = n_valid)
        for (j in seq_along(grp_dists_v)) {
          d <- grp_dists_v[[j]]; yv <- y_vals_valid[[j]]
          segments(x_int + offsets[j], 0, x_int + offsets[j], yv,
                   col = adjustcolor(expert_colors[d$expert_idx], alpha.f = 0.5), lwd = 2)
          points(x_int + offsets[j], yv,
                 col = adjustcolor(expert_colors[d$expert_idx], alpha.f = 0.5), pch = 16, cex = 0.9)
        }
        segments(x_int, 0, x_int, pooled_y, col = "black", lwd = 4)
        points(x_int, pooled_y, col = "black", pch = 16, cex = 1.4)

        leg_labels <- c(sapply(grp_dists_v, function(d) paste0("Expert ", d$expert_num, " (", d$model_label, ")")), "Pooled")
        leg_cols   <- c(sapply(grp_dists_v, function(d) adjustcolor(expert_colors[d$expert_idx], alpha.f = 0.5)), "black")
        legend("topright", legend = leg_labels, col = leg_cols,
               lwd = c(rep(2, n_valid), 4), pch = 16, cex = 0.65, bg = "white")

      } else {
        x <- seq(global_xmin, global_xmax, length.out = 500)
        y_vals_list <- lapply(grp_dists, function(d) {
          yv <- dist_density(fam, d$params, x)
          if (is.null(yv)) NULL else yv
        })
        valid_mask   <- !sapply(y_vals_list, is.null)
        y_vals_valid <- y_vals_list[valid_mask]
        grp_dists_v  <- grp_dists[valid_mask]
        n_valid      <- length(y_vals_valid)
        if (n_valid == 0) next
        pooled_y <- Reduce(`+`, y_vals_valid) / n_valid

        all_y_finite <- c(
          unlist(lapply(y_vals_valid, function(yv) {
            yv_c <- yv[is.finite(yv)]; if (length(yv_c) == 0) 0 else max(yv_c)
          })),
          max(pooled_y[is.finite(pooled_y)], na.rm = TRUE)
        )
        max_y <- max(all_y_finite, na.rm = TRUE)
        if (!is.finite(max_y) || max_y <= 0) max_y <- 1

        plot(NULL, xlim = c(global_xmin, global_xmax), ylim = c(0, max_y * 1.1),
             xlab = "Parameter value", ylab = "Density",
             main = paste(fam_label, "- Pooled"))
        grid()
        if (n_valid < n_exp)
          mtext(paste0(n_exp - n_valid, " expert(s) excluded (invalid params)"),
                side = 3, line = -1.2, cex = 0.75, col = "darkorange")

        lty_cycle <- c(1, 2, 4, 5)
        for (j in seq_along(grp_dists_v)) {
          d <- grp_dists_v[[j]]; yv <- y_vals_valid[[j]]
          yv[!is.finite(yv)] <- NA
          lines(x, yv, col = adjustcolor(expert_colors[d$expert_idx], alpha.f = 0.5),
                lwd = 1.8, lty = lty_cycle[((j - 1) %% 4) + 1])
        }
        pooled_y[!is.finite(pooled_y)] <- NA
        lines(x, pooled_y, col = "black", lwd = 3.5, lty = 1)

        leg_labels <- c(sapply(grp_dists_v, function(d) paste0("Expert ", d$expert_num, " (", d$model_label, ")")), "Pooled")
        leg_cols   <- c(sapply(grp_dists_v, function(d) adjustcolor(expert_colors[d$expert_idx], alpha.f = 0.5)), "black")
        leg_lty    <- c(lty_cycle[((seq_along(grp_dists_v) - 1) %% 4) + 1], 1)
        legend("topright", legend = leg_labels, col = leg_cols,
               lty = leg_lty, lwd = c(rep(1.8, n_valid), 3.5), cex = 0.65, bg = "white")
      }
    }

    mtext("Opinion Pooling (Linear Opinion Pool \u2014 Equal Weights)",
          outer = TRUE, cex = 1.1, font = 2)
  })

  # ---------------------------------------------------------------------------
  # Stan code card — shown below the plot once code is available
  # ---------------------------------------------------------------------------
  output$pooling_stan_card_ui <- renderUI({
    in_flight <- pooling_stan_in_flight()
    result    <- pooling_stan_result()

    if (!in_flight && is.null(result)) return(NULL)

    header_text <- if (in_flight) {
      tagList(tags$span(class = "expert-spinner"), " Generating Stan model...")
    } else {
      tagList(icon("code"), " Stan Model (Pooled Priors)")
    }

    body_content <- if (!in_flight && !is.null(result)) {
      tags$pre(
        style = paste(
          "background: #f8f9fa; border: none; border-radius: 0;",
          "padding: 16px; font-size: 0.82em; overflow-x: auto;",
          "white-space: pre; margin: 0;"
        ),
        tags$code(extract_stan(result$code))
      )
    } else NULL

    div(
      style = "margin-top: 24px;",
      div(
        class = "card",
        div(class = "card-header bg-dark text-white", header_text),
        if (!is.null(body_content)) div(class = "card-body p-0", body_content)
      )
    )
  })
}

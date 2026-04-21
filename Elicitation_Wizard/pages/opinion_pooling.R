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
    uiOutput("pooling_summary_ui"),
    uiOutput("pooling_plot_container")
  )
}

opinion_pooling_server <- function(input, output, session, parsed_distributions) {

  # Helper: resolve best param label for a group of distributions
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

  # ---------------------------------------------------------------------------
  # Opinion pooling summary table
  # ---------------------------------------------------------------------------
  output$pooling_summary_ui <- renderUI({
    all_dists <- parsed_distributions()
    if (length(all_dists) == 0) {
      return(p("No results yet. Run elicitation first.", class = "text-muted"))
    }

    groups <- unique(lapply(all_dists, function(d) list(family = d$family, param_idx = d$param_idx)))

    table_rows <- list()
    for (grp in groups) {
      grp_dists     <- Filter(function(d) d$family == grp$family && d$param_idx == grp$param_idx, all_dists)
      n_experts_grp <- length(grp_dists)
      fam_label     <- resolve_fam_label(grp$family, grp$param_idx, grp_dists)

      all_param_names <- unique(unlist(lapply(grp_dists, function(d) names(d$params))))
      avg_params <- sapply(all_param_names, function(pname) {
        vals <- sapply(grp_dists, function(d) {
          if (pname %in% names(d$params)) d$params[[pname]] else NA_real_
        })
        mean(vals, na.rm = TRUE)
      })
      avg_param_str <- paste(names(avg_params), "=",
                             format(avg_params, digits = 4, nsmall = 2), collapse = ", ")

      for (d in grp_dists) {
        param_str <- paste(names(d$params), "=",
                           format(d$params, digits = 4, nsmall = 2), collapse = ", ")
        table_rows[[length(table_rows) + 1]] <- tags$tr(
          tags$td(fam_label),
          tags$td(paste0("Expert ", d$expert_num)),
          tags$td(d$model_label),
          tags$td(paste0("1/", n_experts_grp)),
          tags$td(tags$code(style = "font-size: 0.85em;", param_str))
        )
      }
      table_rows[[length(table_rows) + 1]] <- tags$tr(
        style = "background-color: #f0f0f0; font-weight: bold;",
        tags$td(fam_label),
        tags$td(style = "color: #000;", paste0("Pooled (n=", n_experts_grp, ")")),
        tags$td("\u2014"),
        tags$td("Equal"),
        tags$td(tags$code(style = "font-size: 0.85em;", avg_param_str))
      )
    }

    tagList(
      h4("Opinion Pooling \u2014 Linear Combination"),
      p(style = "font-size: 0.9em; color: #555;",
        "The pooled prior is formed by linear opinion pooling: ",
        tags$em("f(\u03b8) = \u03a3 w\u1d62 f\u1d62(\u03b8)"),
        " with equal weights ", tags$em("w\u1d62 = 1/n"),
        " across the n experts who returned each distribution family."),
      tags$table(
        class = "table table-bordered table-sm",
        tags$thead(tags$tr(
          tags$th("Family"), tags$th("Source"), tags$th("Model"),
          tags$th("Weight"), tags$th("Parameters")
        )),
        tags$tbody(table_rows)
      )
    )
  })

  # ---------------------------------------------------------------------------
  # Dynamic plot height container
  # ---------------------------------------------------------------------------
  output$pooling_plot_container <- renderUI({
    all_dists <- parsed_distributions()
    if (length(all_dists) == 0) return(plotOutput("pooling_plot", height = "200px"))

    n_grps <- length(unique(lapply(all_dists, function(d) list(family = d$family, param_idx = d$param_idx))))
    n_col  <- if (n_grps <= 1) 1 else if (n_grps == 2) 2 else 3
    n_row  <- ceiling(n_grps / n_col)
    plotOutput("pooling_plot", height = paste0(max(350, n_row * 320), "px"))
  })

  # ---------------------------------------------------------------------------
  # Opinion pooling density plot
  # ---------------------------------------------------------------------------
  output$pooling_plot <- renderPlot({
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
    n_experts     <- max(sapply(all_dists, `[[`, "expert_idx"))
    expert_colors <- rep_len(base_colors, n_experts)

    plot_groups <- unique(lapply(all_dists, function(d) list(family = d$family, param_idx = d$param_idx)))
    n_fam       <- length(plot_groups)

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
      grp_dists <- Filter(function(d) d$family == fam && d$param_idx == grp$param_idx, all_dists)
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
          if (is.null(yv)) rep(0, length(x_int)) else yv
        })
        pooled_y <- Reduce(`+`, y_vals_list) / n_exp
        max_y <- max(unlist(y_vals_list), pooled_y, na.rm = TRUE)
        if (!is.finite(max_y) || max_y <= 0) max_y <- 1

        plot(NULL, xlim = c(min(x_int) - 0.5, max(x_int) + 0.5),
             ylim = c(0, max_y * 1.15), xlab = "Value", ylab = "Probability",
             main = paste(fam_label, "- Consensus"), xaxt = "n")
        axis(1, at = x_int); grid(ny = NULL, nx = NA)

        offsets <- seq(-0.15, 0.15, length.out = n_exp)
        for (j in seq_along(grp_dists)) {
          d <- grp_dists[[j]]; yv <- y_vals_list[[j]]
          segments(x_int + offsets[j], 0, x_int + offsets[j], yv,
                   col = adjustcolor(expert_colors[d$expert_idx], alpha.f = 0.5), lwd = 2)
          points(x_int + offsets[j], yv,
                 col = adjustcolor(expert_colors[d$expert_idx], alpha.f = 0.5), pch = 16, cex = 0.9)
        }
        segments(x_int, 0, x_int, pooled_y, col = "black", lwd = 4)
        points(x_int, pooled_y, col = "black", pch = 16, cex = 1.4)

        leg_labels <- c(sapply(grp_dists, function(d) paste0("Expert ", d$expert_num, " (", d$model_label, ")")), "Consensus")
        leg_cols   <- c(sapply(grp_dists, function(d) adjustcolor(expert_colors[d$expert_idx], alpha.f = 0.5)), "black")
        legend("topright", legend = leg_labels, col = leg_cols,
               lwd = c(rep(2, n_exp), 4), pch = 16, cex = 0.65, bg = "white")

      } else {
        x <- seq(global_xmin, global_xmax, length.out = 500)
        y_vals_list <- lapply(grp_dists, function(d) {
          yv <- dist_density(fam, d$params, x)
          if (is.null(yv)) rep(0, length(x)) else yv
        })
        pooled_y <- Reduce(`+`, y_vals_list) / n_exp

        all_y_finite <- c(
          unlist(lapply(y_vals_list, function(yv) {
            yv_c <- yv[is.finite(yv)]; if (length(yv_c) == 0) 0 else max(yv_c)
          })),
          max(pooled_y[is.finite(pooled_y)], na.rm = TRUE)
        )
        max_y <- max(all_y_finite, na.rm = TRUE)
        if (!is.finite(max_y) || max_y <= 0) max_y <- 1

        plot(NULL, xlim = c(global_xmin, global_xmax), ylim = c(0, max_y * 1.1),
             xlab = "Parameter value", ylab = "Density",
             main = paste(fam_label, "- Consensus"))
        grid()

        lty_cycle <- c(1, 2, 4, 5)
        for (j in seq_along(grp_dists)) {
          d <- grp_dists[[j]]; yv <- y_vals_list[[j]]
          yv[!is.finite(yv)] <- NA
          lines(x, yv, col = adjustcolor(expert_colors[d$expert_idx], alpha.f = 0.5),
                lwd = 1.8, lty = lty_cycle[((j - 1) %% 4) + 1])
        }
        pooled_y[!is.finite(pooled_y)] <- NA
        lines(x, pooled_y, col = "black", lwd = 3.5, lty = 1)

        leg_labels <- c(sapply(grp_dists, function(d) paste0("Expert ", d$expert_num, " (", d$model_label, ")")), "Consensus")
        leg_cols   <- c(sapply(grp_dists, function(d) adjustcolor(expert_colors[d$expert_idx], alpha.f = 0.5)), "black")
        leg_lty    <- c(lty_cycle[((seq_along(grp_dists) - 1) %% 4) + 1], 1)
        legend("topright", legend = leg_labels, col = leg_cols,
               lty = leg_lty, lwd = c(rep(1.8, n_exp), 3.5), cex = 0.65, bg = "white")
      }
    }

    mtext("Opinion Pooling (Linear Opinion Pool \u2014 Equal Weights)",
          outer = TRUE, cex = 1.1, font = 2)
  })
}

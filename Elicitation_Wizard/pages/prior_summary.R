# =============================================================================
# Page: Prior Summary
# UI and server logic for the "Prior Summary" tab.
# Depends on shared reactive: parsed_distributions()
# =============================================================================

prior_summary_ui <- function() {
  tabPanel(
    "Prior Summary",
    br(),
    uiOutput("prior_summary_ui"),
    uiOutput("prior_plot_container")
  )
}

prior_summary_server <- function(input, output, session, parsed_distributions) {

  # Summary table of all parsed distributions
  output$prior_summary_ui <- renderUI({
    results_available <- length(parsed_distributions()) > 0 ||
                         !is.null(input$elicit_btn) && input$elicit_btn > 0

    all_dists <- parsed_distributions()

    if (length(all_dists) == 0) {
      # Distinguish "not yet run" from "ran but nothing parsed"
      return(p("No results yet. Run elicitation first.", class = "text-muted"))
    }

    rows <- lapply(all_dists, function(d) {
      param_str <- paste(
        names(d$params), "=", format(d$params, digits = 4, nsmall = 2),
        collapse = ", "
      )
      tags$tr(
        tags$td(paste("Expert", d$expert_num)),
        tags$td(d$model_label),
        tags$td(tags$code(d$dist_str)),
        tags$td(d$family),
        tags$td(tags$code(style = "font-size: 0.85em;", param_str))
      )
    })

    tagList(
      h4("Summary of Elicited Priors"),
      tags$table(
        class = "table table-bordered table-striped table-sm",
        tags$thead(tags$tr(
          tags$th("Expert"), tags$th("Model"), tags$th("Distribution"),
          tags$th("Family"), tags$th("Parameters")
        )),
        tags$tbody(rows)
      )
    )
  })

  # Dynamic plot height container
  output$prior_plot_container <- renderUI({
    all_dists <- parsed_distributions()
    if (length(all_dists) == 0) return(plotOutput("prior_plot", height = "200px"))

    families  <- unique(sapply(all_dists, `[[`, "family"))
    n_fam     <- length(families)
    n_col     <- if (n_fam <= 1) 1 else if (n_fam == 2) 2 else 3
    n_row     <- ceiling(n_fam / n_col)
    plot_height <- max(350, n_row * 320)
    plotOutput("prior_plot", height = paste0(plot_height, "px"))
  })

  # Density plot: one subplot per family, overlaid per-expert curves
  output$prior_plot <- renderPlot({
    all_dists <- parsed_distributions()
    if (length(all_dists) == 0) {
      par(mar = c(1, 1, 1, 1)); plot.new()
      text(0.5, 0.5,
           "No distributions could be parsed from the expert responses.\nCheck that the LLM output contains distribution specifications\nlike Beta(2.1, 4.3) or Normal(mean=0.3, SD=0.15).",
           cex = 1.2, col = "grey40", font = 3)
      return(invisible(NULL))
    }

    base_colors   <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                       "#9467bd", "#8c564b", "#e377c2", "#7f7f7f")
    n_experts     <- max(sapply(all_dists, `[[`, "expert_idx"))
    expert_colors <- rep_len(base_colors, n_experts)

    families <- unique(sapply(all_dists, `[[`, "family"))
    n_fam    <- length(families)

    if (n_fam <= 1)      { n_row <- 1; n_col <- 1
    } else if (n_fam == 2) { n_row <- 1; n_col <- 2
    } else if (n_fam <= 4) { n_row <- 2; n_col <- 2
    } else if (n_fam <= 6) { n_row <- 2; n_col <- 3
    } else                 { n_row <- ceiling(n_fam / 3); n_col <- 3 }

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    par(mfrow = c(n_row, n_col), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))

    for (fam in families) {
      fam_dists   <- Filter(function(d) d$family == fam, all_dists)
      is_discrete <- tolower(gsub("-", "", fam)) == "poisson"

      xranges <- lapply(fam_dists, function(d) {
        xr <- dist_xrange(d$family, d$params)
        if (any(!is.finite(xr))) c(-5, 5) else xr
      })
      global_xmin <- min(sapply(xranges, `[`, 1))
      global_xmax <- max(sapply(xranges, `[`, 2))
      if (!is.finite(global_xmin) || !is.finite(global_xmax) ||
          global_xmin >= global_xmax) { global_xmin <- -5; global_xmax <- 5 }

      if (is_discrete) {
        x_int <- seq(max(0, floor(global_xmin)), ceiling(global_xmax))
        y_vals_list <- lapply(fam_dists, function(d) {
          yv <- dist_density(d$family, d$params, x_int)
          if (is.null(yv)) rep(0, length(x_int)) else yv
        })
        max_y <- max(unlist(y_vals_list), na.rm = TRUE)
        if (!is.finite(max_y) || max_y <= 0) max_y <- 1

        plot(NULL, xlim = c(min(x_int) - 0.5, max(x_int) + 0.5),
             ylim = c(0, max_y * 1.1), xlab = "Value", ylab = "Probability",
             main = fam, xaxt = "n")
        axis(1, at = x_int); grid(ny = NULL, nx = NA)

        n_exp   <- length(fam_dists)
        offsets <- seq(-0.15, 0.15, length.out = n_exp)
        for (j in seq_along(fam_dists)) {
          d  <- fam_dists[[j]]; yv <- y_vals_list[[j]]
          segments(x_int + offsets[j], 0, x_int + offsets[j], yv,
                   col = expert_colors[d$expert_idx], lwd = 3)
          points(x_int + offsets[j], yv, col = expert_colors[d$expert_idx], pch = 16, cex = 1.2)
        }
      } else {
        x <- seq(global_xmin, global_xmax, length.out = 500)
        y_vals_list <- lapply(fam_dists, function(d) {
          yv <- dist_density(d$family, d$params, x)
          if (is.null(yv)) rep(0, length(x)) else yv
        })
        max_y <- max(unlist(lapply(y_vals_list, function(yv) {
          yv_c <- yv[is.finite(yv)]; if (length(yv_c) == 0) 0 else max(yv_c)
        })), na.rm = TRUE)
        if (!is.finite(max_y) || max_y <= 0) max_y <- 1

        plot(NULL, xlim = c(global_xmin, global_xmax), ylim = c(0, max_y * 1.08),
             xlab = "Parameter value", ylab = "Density", main = fam)
        grid()

        lty_cycle <- c(1, 2, 4, 5)
        for (j in seq_along(fam_dists)) {
          d  <- fam_dists[[j]]; yv <- y_vals_list[[j]]
          yv[!is.finite(yv)] <- NA
          lines(x, yv, col = expert_colors[d$expert_idx],
                lwd = 2.2, lty = lty_cycle[((j - 1) %% 4) + 1])
        }
      }

      leg_labels <- sapply(fam_dists, function(d) paste0(d$expert_label, ":  ", d$dist_str))
      leg_cols   <- expert_colors[sapply(fam_dists, `[[`, "expert_idx")]
      if (is_discrete) {
        legend("topright", legend = leg_labels, col = leg_cols,
               lwd = 3, pch = 16, cex = 0.7, bg = "white")
      } else {
        lty_vals <- lty_cycle[((seq_along(fam_dists) - 1) %% 4) + 1]
        legend("topright", legend = leg_labels, col = leg_cols,
               lty = lty_vals, lwd = 2.2, cex = 0.7, bg = "white")
      }
    }

    mtext("Parsed Prior Distributions by Expert", outer = TRUE, cex = 1.1, font = 2)
  })
}

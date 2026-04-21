library(shiny)
library(bslib)
library(shinyjs)

fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  useShinyjs(),
  titlePanel("Prior Validation"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # -----------------------------------------------------------------------
      # Shared: dataset upload and column mapping (used by both tabs)
      # -----------------------------------------------------------------------
      h4("Dataset"),
      fileInput("val_file", "Upload CSV:", accept = ".csv"),
      uiOutput("val_col_patient"),
      uiOutput("val_col_site"),
      uiOutput("val_col_ae"),
      uiOutput("val_data_preview"),

      # -----------------------------------------------------------------------
      # Tab 1: Validation-specific inputs
      # -----------------------------------------------------------------------
      conditionalPanel(
        condition = "input.main_tabs == 'Validation'",

        hr(),
        h4("Stan Hyperpriors"),
        p("Enter the Exponential rate parameters for the Gamma hyperpriors.",
          style = "font-size: 0.85em; color: #666;"),
        numericInput("rate_alpha", "rate_alpha:", value = 0.1, min = 0.001, step = 0.01),
        numericInput("rate_beta",  "rate_beta:",  value = 0.1, min = 0.001, step = 0.01),

        hr(),
        accordion(
          id = "advanced_config",
          open = FALSE,
          accordion_panel(
            "Advanced Configuration",

            h5("MCMC Settings"),
            numericInput("mcmc_chains", "Chains:", value = 2, min = 1, max = 8, step = 1),
            numericInput("mcmc_iter", "Iterations:", value = 1000, min = 100, step = 100),
            numericInput("mcmc_warmup", "Warmup:", value = 500, min = 50, step = 50),

            hr(),
            h5("Cross-Validation Settings"),
            numericInput("cv_folds", "CV Folds:", value = 5, min = 2, max = 10, step = 1),

            hr(),
            h5("Sample Efficiency Settings"),
            numericInput("se_n_reps", "SE Replicates:", value = 5, min = 1, max = 50, step = 1),
            checkboxGroupInput("se_subsample_levels", "Subsample levels:",
                               choices  = c("20%" = 0.2, "40%" = 0.4, "60%" = 0.6,
                                            "80%" = 0.8, "100%" = 1.0),
                               selected = c(0.2, 0.4, 0.6, 0.8, 1.0))
          )
        ),
        p("Paper spec: 4 chains, 2000 iter, 1000 warmup, 20 SE reps, levels 20/40/60/80/100%.",
          class = "text-muted", style = "font-size: 0.78em; margin-top: 0.5em;"),

        hr(),
        actionButton("val_run_btn", "Run Validation", class = "btn-success btn-lg w-100")
      ),

      # -----------------------------------------------------------------------
      # Tab 2: Prior Robustness-specific inputs
      # -----------------------------------------------------------------------
      conditionalPanel(
        condition = "input.main_tabs == 'Prior Robustness'",

        hr(),
        h4("Hyperprior Grid"),
        p("Define the grid of Exponential rate parameters to sweep. Enter comma-separated values.",
          style = "font-size: 0.85em; color: #666;"),
        textInput("rob_alpha_grid", "rate_alpha values:",
                  value = "0.01, 0.05, 0.1, 0.5, 1.0"),
        textInput("rob_beta_grid", "rate_beta values:",
                  value = "0.01, 0.05, 0.1, 0.5, 1.0"),
        uiOutput("rob_grid_preview"),

        hr(),
        accordion(
          id = "rob_advanced_config",
          open = FALSE,
          accordion_panel(
            "Advanced Configuration",

            h5("MCMC Settings"),
            numericInput("rob_mcmc_chains", "Chains:", value = 2, min = 1, max = 8, step = 1),
            numericInput("rob_mcmc_iter", "Iterations:", value = 2000, min = 100, step = 100),
            numericInput("rob_mcmc_warmup", "Warmup:", value = 1000, min = 50, step = 50),

            hr(),
            h5("Cross-Validation Settings"),
            numericInput("rob_cv_folds", "CV Folds:", value = 5, min = 2, max = 10, step = 1)
          )
        ),
        p("Each grid point runs a full K-fold CV. Larger grids take proportionally longer.",
          class = "text-muted", style = "font-size: 0.78em; margin-top: 0.5em;"),

        hr(),
        actionButton("rob_run_btn", "Run Robustness Analysis", class = "btn-primary btn-lg w-100")
      )
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",

        # =====================================================================
        # Tab 1: Validation (existing content, unchanged)
        # =====================================================================
        tabPanel("Validation",
          fluidRow(
            column(6,
              h4("Cross-Validation LPD (Table 1)"),
              uiOutput("val_cv_table_ui"),
              plotOutput("val_cv_plot", height = "260px"),
              div(class = "mt-1",
                downloadButton("dl_cv_plot", "Download Plot", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("dl_cv_csv",  "Download CSV",  class = "btn-sm btn-outline-secondary")
              )
            ),
            column(6,
              h4("Sample Efficiency (Table 2)"),
              uiOutput("val_se_table_ui"),
              plotOutput("val_se_plot", height = "260px"),
              div(class = "mt-1",
                downloadButton("dl_se_plot", "Download Plot", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("dl_se_csv",  "Download CSV",  class = "btn-sm btn-outline-secondary")
              )
            )
          )
        ),

        # =====================================================================
        # Tab 2: Prior Robustness (new)
        # =====================================================================
        tabPanel("Prior Robustness",
          h4("Prior Robustness Analysis"),
          p("Sensitivity of LPD to the Exponential hyperprior rates (rate_alpha, rate_beta).",
            class = "text-muted", style = "font-size: 0.9em;"),

          uiOutput("rob_convergence_warning"),

          fluidRow(
            column(7,
              uiOutput("rob_heatmap_ui"),
              div(class = "mt-1",
                downloadButton("dl_rob_plot", "Download Heatmap", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("dl_rob_csv",  "Download CSV",     class = "btn-sm btn-outline-secondary")
              )
            ),
            column(5,
              h5("Results Table"),
              uiOutput("rob_table_ui")
            )
          )
        )
      )
    )
  )
)

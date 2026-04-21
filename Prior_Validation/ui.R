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

      h4("Dataset"),
      fileInput("val_file", "Upload CSV:", accept = ".csv"),
      uiOutput("val_col_patient"),
      uiOutput("val_col_site"),
      uiOutput("val_col_ae"),
      uiOutput("val_data_preview"),

      hr(),
      h4("Stan Hyperpriors"),
      p("Enter the Exponential rate parameters for the Gamma hyperpriors.",
        style = "font-size: 0.85em; color: #666;"),
      numericInput("rate_alpha", "rate_alpha:", value = 0.1, min = 0.001, step = 0.01),
      numericInput("rate_beta",  "rate_beta:",  value = 0.1, min = 0.001, step = 0.01),

      hr(),
      actionButton("val_run_btn", "Run Validation", class = "btn-success btn-lg w-100")
    ),

    mainPanel(
      width = 9,
      fluidRow(
        column(6,
          h4("Cross-Validation LPD (Table 1)"),
          uiOutput("val_cv_table_ui")
        ),
        column(6,
          h4("Sample Efficiency (Table 2)"),
          uiOutput("val_se_table_ui")
        )
      )
    )
  )
)

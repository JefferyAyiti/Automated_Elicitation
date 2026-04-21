fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  useShinyjs(),
  withMathJax(),
  tags$head(tags$style(HTML("
    .expert-spinner {
      display: inline-block;
      width: 1.2rem;
      height: 1.2rem;
      border: 3px solid rgba(0,0,0,.15);
      border-top-color: #2c3e50;
      border-radius: 50%;
      animation: expert-spin 0.8s linear infinite;
      vertical-align: middle;
      margin-right: 8px;
    }
    @keyframes expert-spin {
      to { transform: rotate(360deg); }
    }
    .expert-loading-card .card-body {
      display: flex;
      align-items: center;
      justify-content: center;
      min-height: 150px;
      color: #555;
    }
  "))),
  titlePanel("LLM Prior Elicitation Wizard"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      tags$details(
        tags$summary(h4("API Connection", style = "display:inline; cursor:pointer;")),
        br(),
        textInput("base_url", "Base URL:", placeholder = "https://your-api-endpoint/v1"),
        passwordInput("api_key", "API Key:", placeholder = "sk-..."),
        uiOutput("connection_status")
      ),

      hr(),
      tags$details(
        tags$summary(h4("Clinical Context", style = "display:inline; cursor:pointer;")),
        br(),
        textAreaInput("clinical_context",
          "Disease, population, and setting:",
          value = "Disease: Metastatic hormone-resistant prostate cancer (HRPC)\nTreatment: Control arm (placebo/standard care)\nPopulation: Adult oncology patients\nStudy: Multi-center RCT",
          rows = 3
        )
      ),

      hr(),
      tags$details(
        tags$summary(h4("Data Variables", style = "display:inline; cursor:pointer;")),
        br(),
        textAreaInput("data_variables",
          "Available variables in the dataset:",
          value = "patient_id: unique patient identifier\nsite_id: clinical site identifier\nae_count: cumulative adverse event count per patient",
          rows = 3,
          placeholder = "e.g. patient_id, site_id, ae_count, treatment_arm, ..."
        )
      ),

      hr(),
      h4("Expert Configuration"),
      numericInput("num_experts", "Number of experts:", value = 2, min = 1, max = 4, step = 1),

      uiOutput("expert_selectors"),

      hr(),
      actionButton("elicit_btn", "Elicit Priors", class = "btn-primary btn-lg w-100"),
      uiOutput("elicitation_status"),

      hr(),
      h5("Elicitation Prompt"),
      p(em("LLMs are asked to design a statistical model and specify informative priors based on the clinical context and available data variables."),
        style = "font-size: 0.85em; color: #666;")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        expert_responses_ui(),
        prior_summary_ui(),
        delphi_ui()
      )
    )
  )
)

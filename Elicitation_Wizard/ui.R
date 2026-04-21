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
        tags$summary(h4("Result Expectations", style = "display:inline; cursor:pointer;")),
        br(),
        textAreaInput("parameter_topic",
          "Parameter to elicit prior for:",
          value = "Model:\nEach patient i in site j has AE count:\ny_ij ~ Poisson(lambda_j)\nSite-specific rates: lambda_j ~ Gamma(alpha, beta)\nREQUIRED: alpha ~ Exponential(rate_alpha), beta ~ Exponential(rate_beta)",
          rows = 3
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
      p(em("Each expert will be asked to provide a prior distribution (mean and SD or shape parameters) for the parameter above."),
        style = "font-size: 0.85em; color: #666;")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        expert_responses_ui(),
        prior_summary_ui(),
        opinion_pooling_ui(),
        delphi_ui()
      )
    )
  )
)

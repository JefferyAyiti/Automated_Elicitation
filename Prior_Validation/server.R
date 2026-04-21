library(shiny)
library(shinyjs)
library(rstan)

source("experiment.R")

function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Reactive: uploaded data (raw CSV)
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
  # ---------------------------------------------------------------------------
  val_data <- reactive({
    df <- val_raw_data()
    if (is.null(df)) return(NULL)
    req(input$val_patient_col, input$val_site_col, input$val_ae_col)
    out <- data.frame(
      patient_id = df[[input$val_patient_col]],
      site_id    = as.character(df[[input$val_site_col]]),
      ae_count   = as.integer(df[[input$val_ae_col]]),
      stringsAsFactors = FALSE
    )
    out[!is.na(out$ae_count), ]
  })

  # ---------------------------------------------------------------------------
  # Validation run
  # ---------------------------------------------------------------------------
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
        run_cv(data, rate_alpha, rate_beta, stan_model = sm, chains = 2, iter = 1000, warmup = 500),
        error = function(e) { showNotification(paste("CV error:", e$message), type = "error"); NULL }
      )
      val_cv_results(cv_res)

      incProgress(0.5, message = "Running sample efficiency experiment...")
      se_res <- tryCatch(
        run_sample_efficiency(data, rate_alpha, rate_beta,
                              subsample_levels = c(0.25, 0.5, 0.75, 1.0),
                              n_reps = 5, stan_model = sm, chains = 2, iter = 1000, warmup = 500),
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
      check.names = FALSE
    )
    tagList(
      div(
        class = "alert alert-success p-2 mb-2",
        style = "font-size: 0.85em;",
        sprintf("Overall LPD: %.4f ± %.4f", summary$lpd_mean, summary$lpd_sd)
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
    df_display <- data.frame(
      `Sample %` = summary$subsample_pct,
      `LPD Mean` = round(summary$lpd_mean, 4),
      `LPD SD`   = round(summary$lpd_sd, 4),
      check.names = FALSE
    )
    tags$table(
      class = "table table-sm table-bordered table-striped",
      tags$thead(tags$tr(lapply(names(df_display), tags$th))),
      tags$tbody(apply(df_display, 1, function(row) {
        tags$tr(lapply(row, tags$td))
      }))
    )
  })
}

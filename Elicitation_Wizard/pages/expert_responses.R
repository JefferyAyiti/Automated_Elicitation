# =============================================================================
# Page: Expert Responses
# UI and server logic for the "Expert Responses" tab.
# Depends on shared reactives: expert_results(), expert_status(),
#   parsed_distributions()
# =============================================================================

expert_responses_ui <- function() {
  tabPanel(
    "Expert Responses",
    br(),
    uiOutput("expert_panels"),
    uiOutput("stan_card_ui")
  )
}

expert_responses_server <- function(input, output, session,
                                    expert_results, expert_status,
                                    parsed_distributions,
                                    active_url, active_key) {

  # Single shared Stan result: overwrites on each new generation
  stan_result    <- reactiveVal(NULL)   # list(expert_num, model_label, code) or NULL
  stan_in_flight <- reactiveVal(FALSE)

  # Register a generate-Stan observer for one expert index.
  # Called once per expert when the card is first rendered.
  make_stan_observer <- function(idx) {
    btn_id <- paste0("stan_btn_", idx)
    observeEvent(input[[btn_id]], {
      results <- expert_results()
      r <- Filter(function(x) x$expert_num == idx, results)
      if (length(r) == 0) return()
      r <- r[[1]]

      url     <- active_url()
      api_key <- active_key()
      if (nchar(url) == 0 || nchar(api_key) == 0) {
        showNotification("Please provide a Base URL and API Key.", type = "error")
        return()
      }

      stan_prompt <- paste0(
        "You are an expert Stan programmer. Below is a Bayesian model proposal ",
        "and prior distributions for a clinical trial, written by a biostatistics expert.\n\n",
        "---\n\n", r$response, "\n\n---\n\n",
        "Your task:\n",
        "1. Translate this proposal into a complete, valid Stan model.\n",
        "2. Use exactly the prior distributions the expert specified.\n",
        "3. Include a `generated quantities` block that computes `log_lik` ",
        "(log-likelihood per observation) to enable LOO-CV with the `loo` package.\n",
        "4. Add brief inline comments explaining each block.\n\n",
        "Respond with **only** the Stan model code in a single ```stan code block. ",
        "Do not include any explanation outside the code block."
      )

      stan_result(NULL)
      stan_in_flight(TRUE)
      shinyjs::disable(btn_id)

      fut_url      <- url
      fut_key      <- api_key
      fut_model_id <- stan_coder_model_id
      fut_prompt   <- stan_prompt
      fut_label    <- r$model_label

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
          stan_result(list(expert_num = idx, model_label = fut_label, code = response_text))
          stan_in_flight(FALSE)
          shinyjs::enable(btn_id)
        },
        onRejected = function(err) {
          stan_result(list(expert_num = idx, model_label = fut_label,
                           code = paste("Error:", conditionMessage(err))))
          stan_in_flight(FALSE)
          shinyjs::enable(btn_id)
        }
      )
    }, ignoreInit = TRUE)
  }

  # Track which observers have been registered
  observers_created <- list()

  # Expert response panels (one card per expert, updates incrementally)
  output$expert_panels <- renderUI({
    status  <- expert_status()
    results <- expert_results()

    if (length(status) == 0 && length(results) == 0) {
      return(div(
        class = "text-center text-muted mt-5",
        h4("Configure experts and click 'Elicit Priors' to begin.")
      ))
    }

    results_by_num <- list()
    for (r in results) results_by_num[[as.character(r$expert_num)]] <- r

    n <- length(status)
    if (n == 0) n <- length(results)
    col_width <- max(3, floor(12 / n))

    panels <- lapply(names(status), function(key) {
      st  <- status[[key]]
      idx <- st$expert_num
      model_label <- st$model_label

      if (st$status == "querying") {
        column(col_width,
          div(class = "card h-100 expert-loading-card",
            div(class = "card-header bg-secondary text-white",
              strong(paste("Expert", idx)), br(), tags$small(model_label)),
            div(class = "card-body",
              div(tags$span(class = "expert-spinner"),
                  paste0("Querying ", model_label, "...")))
          )
        )
      } else if (st$status == "error") {
        column(col_width,
          div(class = "card h-100 border-danger",
            div(class = "card-header bg-danger text-white",
              strong(paste("Expert", idx)), br(), tags$small(model_label)),
            div(class = "card-body",
              div(class = "alert alert-danger", style = "font-size: 0.88em;",
                tags$strong("Error: "), st$message))
          )
        )
      } else {
        # Register per-expert Stan observer on first render
        if (is.null(observers_created[[key]])) {
          make_stan_observer(idx)
          observers_created[[key]] <<- TRUE
        }

        r <- results_by_num[[as.character(idx)]]
        response_html <- if (!is.null(r)) render_markdown(r$response) else tags$em("Response missing.")

        btn_label <- if (isTRUE(stan_in_flight())) {
          tagList(tags$span(class = "expert-spinner"))
        } else {
          icon("code")
        }

        column(col_width,
          div(class = "card h-100",
            div(
              class = "card-header bg-primary text-white",
              style = "display: flex; justify-content: space-between; align-items: center;",
              div(strong(paste("Expert", idx)), br(), tags$small(model_label)),
              actionButton(
                paste0("stan_btn_", idx), btn_label,
                class = "btn btn-sm btn-light",
                style = "white-space: nowrap;"
              )
            ),
            div(class = "card-body",
              style = "font-size: 0.88em; overflow-y: auto; max-height: 600px;",
              response_html)
          )
        )
      }
    })

    tagList(
      fluidRow(panels),
      tags$script("if(window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);")
    )
  })

  # Stan code card — shown below all expert panels once code is available
  output$stan_card_ui <- renderUI({
    in_flight <- stan_in_flight()
    result    <- stan_result()

    if (!in_flight && is.null(result)) return(NULL)

    header_text <- if (in_flight) {
      tagList(tags$span(class = "expert-spinner"), " Generating Stan model...")
    } else {
      tagList(
        icon("code"), " Stan Model",
        tags$span(
          style = "font-size: 0.85em; font-weight: normal; margin-left: 10px;",
          paste0("(Expert ", result$expert_num, " — ", result$model_label, ")")
        )
      )
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

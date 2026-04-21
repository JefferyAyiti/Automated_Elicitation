# =============================================================================
# Page: Expert Responses
# UI and server logic for the "Expert Responses" tab.
# Depends on shared reactives: expert_results(), expert_status(),
#   elicitation_context(), parsed_distributions(), active_url(), active_key()
# =============================================================================

expert_responses_ui <- function() {
  tabPanel(
    "Expert Responses",
    br(),
    uiOutput("elicitation_context_box"),
    uiOutput("expert_panels"),
    uiOutput("stan_cards_ui")
  )
}

expert_responses_server <- function(input, output, session,
                                    expert_results, expert_status,
                                    elicitation_context,
                                    parsed_distributions,
                                    active_url, active_key) {

  # ---------------------------------------------------------------------------
  # Per-expert Stan state
  #   stan_states() is a named list keyed by as.character(expert_num):
  #     list(in_flight = FALSE, result = NULL)
  #   result (when set): list(expert_num, model_label, code)
  # ---------------------------------------------------------------------------
  stan_states <- reactiveVal(list())

  # Helper: update a single expert's Stan state without touching others
  update_stan_state <- function(idx, in_flight, result = NULL) {
    key <- as.character(idx)
    st  <- stan_states()
    st[[key]] <- list(in_flight = in_flight, result = result)
    stan_states(st)
  }

  # Track which per-expert observers have been registered.
  # Reset on every new elicitation so stale observers from prior runs don't
  # accumulate.
  observers_created <- list()

  observe({
    status <- expert_status()
    # A new elicitation resets expert_status to an empty list (server.R line 61)
    # before repopulating it. Clear our tracking state at that moment.
    if (length(status) == 0) {
      observers_created <<- list()
      stan_states(list())
    }
  })

  # Register a generate-Stan observer for one expert index.
  # Called once per expert when the card is first rendered (done state).
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

      stan_prompt <- paste0(
        "You are an expert Stan programmer. Below is a Bayesian model specification ",
        "and an expert's prior elicitation response for a clinical trial.\n\n",
        model_spec_section,
        "## Expert Prior Elicitation Response\n\n",
        "---\n\n", r$response, "\n\n---\n\n",
        "Your task:\n",
        "1. Translate this into a complete, valid Stan model.\n",
        "2. Use the model structure from the specification and the prior distributions the expert specified.\n",
        "3. Include a `generated quantities` block that computes `log_lik` ",
        "(log-likelihood per observation) to enable LOO-CV with the `loo` package.\n",
        "4. Add brief inline comments explaining each block.\n\n",
        "Respond with **only** the Stan model code in a single ```stan code block. ",
        "Do not include any explanation outside the code block."
      )

      # Mark this expert as in-flight; disable ALL Stan buttons to prevent
      # concurrent generation requests writing to the same state
      update_stan_state(idx, in_flight = TRUE)
      lapply(names(stan_states()), function(k) shinyjs::disable(paste0("stan_btn_", k)))
      shinyjs::disable(btn_id)  # also cover the just-clicked btn before states update

      fut_url      <- url
      fut_key      <- api_key
      fut_model_id <- stan_coder_model_id
      fut_prompt   <- stan_prompt
      fut_label    <- r$model_label
      fut_idx      <- idx

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
          update_stan_state(fut_idx, in_flight = FALSE,
                            result = list(expert_num = fut_idx,
                                          model_label = fut_label,
                                          code = response_text))
          # Re-enable all Stan buttons
          lapply(names(stan_states()), function(k) shinyjs::enable(paste0("stan_btn_", k)))
        },
        onRejected = function(err) {
          update_stan_state(fut_idx, in_flight = FALSE,
                            result = list(expert_num = fut_idx,
                                          model_label = fut_label,
                                          code = paste("Error:", conditionMessage(err))))
          lapply(names(stan_states()), function(k) shinyjs::enable(paste0("stan_btn_", k)))
        }
      )
      NULL
    }, ignoreInit = TRUE)
  }

  # ---------------------------------------------------------------------------
  # Per-expert Stan button label outputs (spinner vs icon).
  # These are rendered independently from expert_panels, so Stan state changes
  # do NOT trigger a full re-render of the expert response cards.
  # ---------------------------------------------------------------------------
  make_stan_btn_label_output <- function(idx) {
    output[[paste0("stan_btn_label_", idx)]] <- renderUI({
      st <- stan_states()[[as.character(idx)]]
      if (!is.null(st) && isTRUE(st$in_flight)) {
        tags$span(class = "expert-spinner")
      } else {
        icon("code")
      }
    })
  }

  # ---------------------------------------------------------------------------
  # Context summary box: parameter being elicited + parsed distributions list
  # ---------------------------------------------------------------------------
  output$elicitation_context_box <- renderUI({
    results <- expert_results()
    if (length(results) == 0) return(NULL)

    ctx <- elicitation_context()
    if (is.null(ctx)) return(NULL)

    dists <- parsed_distributions()

    sorted_results <- results[order(sapply(results, `[[`, "expert_num"))]
    prior_items <- lapply(sorted_results, function(r) {
      expert_dists <- Filter(function(d) d$expert_num == r$expert_num, dists)
      if (length(expert_dists) == 0) {
        dist_tags <- list(tags$div(tags$em(style = "color:#888; font-size:0.9em;", "Could not parse")))
      } else {
        dist_tags <- lapply(expert_dists, function(d) {
          tags$div(tags$code(style = "font-size: 0.95em;", d$dist_str))
        })
      }
      tags$li(
        style = "font-size: 0.93em; margin-bottom: 6px;",
        strong(paste0("Expert ", r$expert_num)),
        tags$span(style = "color: #555;", paste0(" (", r$model_label, "): ")),
        tagList(dist_tags)
      )
    })

    div(
      style = "margin-bottom: 18px;",
      div(
        class = "card border-info",
        div(
          class = "card-header bg-info text-white",
          style = "padding: 8px 14px;",
          strong("Parameter Being Elicited", style = "font-size: 1.05em;")
        ),
        div(
          class = "card-body",
          style = "padding: 10px 16px;",
          tags$strong(style = "font-size: 0.93em;", "Extracted Prior Distributions:"),
          tags$ol(
            style = "margin-top: 6px; margin-bottom: 0; padding-left: 22px;",
            prior_items
          )
        )
      )
    )
  })

  # ---------------------------------------------------------------------------
  # Expert response panels — NO Stan state read here; button label is a
  # uiOutput placeholder so Stan activity never re-renders these cards.
  # ---------------------------------------------------------------------------
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
      st          <- status[[key]]
      idx         <- st$expert_num
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
        # Register per-expert Stan observer and button label output on first render
        if (is.null(observers_created[[key]])) {
          make_stan_observer(idx)
          make_stan_btn_label_output(idx)
          # Initialise Stan state entry for this expert (in_flight=FALSE, no result)
          cur <- stan_states()
          if (is.null(cur[[as.character(idx)]])) {
            cur[[as.character(idx)]] <- list(in_flight = FALSE, result = NULL)
            stan_states(cur)
          }
          observers_created[[key]] <<- TRUE
        }

        r             <- results_by_num[[as.character(idx)]]
        response_html <- if (!is.null(r)) render_markdown(r$response) else tags$em("Response missing.")

        column(col_width,
          div(class = "card h-100",
            div(
              class = "card-header bg-primary text-white",
              style = "display: flex; justify-content: space-between; align-items: center;",
              div(strong(paste("Expert", idx)), br(), tags$small(model_label)),
              actionButton(
                paste0("stan_btn_", idx),
                # uiOutput placeholder: updates independently without re-rendering this card
                uiOutput(paste0("stan_btn_label_", idx), inline = TRUE),
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

  # ---------------------------------------------------------------------------
  # Per-expert Stan code cards — rendered independently below expert panels.
  # Only visible experts with a completed (or in-flight) Stan state are shown.
  # ---------------------------------------------------------------------------
  output$stan_cards_ui <- renderUI({
    st_all <- stan_states()
    if (length(st_all) == 0) return(NULL)

    # Collect cards for experts that have any Stan activity
    cards <- lapply(sort(as.integer(names(st_all))), function(idx) {
      key <- as.character(idx)
      st  <- st_all[[key]]
      if (is.null(st)) return(NULL)
      if (!isTRUE(st$in_flight) && is.null(st$result)) return(NULL)

      in_flight <- isTRUE(st$in_flight)
      result    <- st$result

      # Retrieve model_label from expert_status for the header
      model_label <- tryCatch(expert_status()[[key]]$model_label, error = function(e) "")

      header_text <- if (in_flight) {
        tagList(tags$span(class = "expert-spinner"),
                paste0(" Generating Stan model for Expert ", idx, "..."))
      } else {
        tagList(
          icon("code"), " Stan Model",
          tags$span(
            style = "font-size: 0.85em; font-weight: normal; margin-left: 10px;",
            paste0("(Expert ", result$expert_num, " \u2014 ", result$model_label, ")")
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

    cards <- Filter(Negate(is.null), cards)
    if (length(cards) == 0) return(NULL)
    tagList(cards)
  })
}

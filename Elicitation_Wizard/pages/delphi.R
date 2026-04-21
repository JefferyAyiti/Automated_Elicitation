# =============================================================================
# Page: Delphi Method
# UI and server logic for the "Delphi Method" tab.
# Implements multi-round Delphi expert elicitation: each round, experts see
# the other experts' prior distributions from the previous round and can revise.
# Up to 3 rounds. Independent from the main elicitation pipeline.
#
# Bug fixes applied:
#   - Bugs 1,2,5: Replaced per-future accumulator with promise_all() for atomic
#     round finalization (eliminates race conditions)
#   - Bug 8: Locked expert config after round 1 starts
#   - Bug 9/3: Notification when peer feedback is partial in round 2+
#   - Bug 10: Cap round counter at MAX_ROUNDS
#   - Bug 11: Require at least 2 experts
#   - Bug 12: Total round failure notification
#   - Bug 13: Disable Reset button during in-flight
#
# Stan code generation (mirrors expert_responses.R):
#   - Per-expert </> button in the converged distributions table
#   - Sends expert's latest-round response to Qwen-Coder for Stan translation
#   - Shared dark card below converged plot displays generated Stan code
# =============================================================================

delphi_ui <- function() {
  tabPanel(
    "Delphi Method",
    br(),

    # -- Control bar ----------------------------------------------------------
    fluidRow(
      column(3,
        actionButton("delphi_run_btn", "Run Delphi",
                      class = "btn-primary btn-lg w-100",
                      icon = icon("play"))
      ),
      column(3,
        div(
          style = "line-height: 3rem; font-size: 1.15em; text-align: center;",
          uiOutput("delphi_round_indicator")
        )
      ),
      column(3,
        actionButton("delphi_reset_btn", "Reset",
                      class = "btn-outline-danger w-100",
                      icon = icon("rotate-left"),
                      style = "height: 48px;")
      ),
      column(3,
        uiOutput("delphi_flight_status")
      )
    ),

    # -- Locked config info alert (shown when expert config is frozen) --------
    uiOutput("delphi_locked_config_alert"),

    hr(),

    # -- Per-round accordion results ------------------------------------------
    uiOutput("delphi_rounds_ui"),

    # -- Converged distributions (after final round or when user stops) -------
    uiOutput("delphi_converged_ui")
  )
}


delphi_server <- function(input, output, session,
                          active_url, active_key,
                          num_experts_input, expert_model_inputs,
                          clinical_context_input,
                          data_variables_input = reactive({ "" })) {

  # -----------------------------------------------------------------------
  # Internal state
  # -----------------------------------------------------------------------
  MAX_ROUNDS <- 3L

  # List of round results. Each element is a list of per-expert entries
  # with structure: list(expert_num, model_label, model_id, response,
  #                      distributions, error)
  delphi_rounds         <- reactiveVal(list())
  delphi_current_round  <- reactiveVal(0L)
  # Bug 1,2,5 fix: delphi_in_flight is now logical (TRUE/FALSE) instead of
  # an integer counter, since promise_all resolves atomically
  delphi_in_flight      <- reactiveVal(FALSE)
  delphi_round_status   <- reactiveVal(list())  # per-expert status for round in progress

  # Bug 8: Locked expert configuration after round 1 starts

  # Stores list(n_experts = integer, expert_models = list of labels) or NULL
  delphi_locked_config  <- reactiveVal(NULL)

  # Per-round warnings for partial peer feedback (Bug 9/3)
  # Named list keyed by round number as character, value is warning string or NULL
  delphi_round_warnings <- reactiveVal(list())

  # Stan code generation state (mirrors expert_responses.R)
  delphi_stan_result         <- reactiveVal(NULL)  # list(expert_num, model_label, code) or NULL
  delphi_stan_in_flight      <- reactiveVal(FALSE)
  delphi_stan_active_expert  <- reactiveVal(NULL)  # expert_num currently generating Stan code

  # Track which Stan observers have been registered (keyed by expert index)
  delphi_stan_observers <- list()

  # -----------------------------------------------------------------------
  # Register a generate-Stan observer for one expert index in the Delphi
  # converged section. Called once per expert when the converged table is
  # first rendered. Uses the expert's response from the latest completed
  # round.
  # -----------------------------------------------------------------------
  make_delphi_stan_observer <- function(idx) {
    btn_id <- paste0("delphi_stan_btn_", idx)
    observeEvent(input[[btn_id]], {
      # Get the expert's response from the latest round
      rounds <- delphi_rounds()
      if (length(rounds) == 0) return()
      latest_round <- rounds[[length(rounds)]]
      r <- Filter(function(x) x$expert_num == idx, latest_round)
      if (length(r) == 0) return()
      r <- r[[1]]

      # Skip if the expert errored
      if (isTRUE(r$error)) {
        showNotification(
          paste0("Expert ", idx, " had an error in the latest round. Cannot generate Stan code."),
          type = "warning"
        )
        return()
      }

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

      delphi_stan_result(NULL)
      delphi_stan_in_flight(TRUE)
      delphi_stan_active_expert(idx)
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
          delphi_stan_result(list(expert_num = idx, model_label = fut_label, code = response_text))
          delphi_stan_in_flight(FALSE)
          delphi_stan_active_expert(NULL)
          shinyjs::enable(btn_id)
        },
        onRejected = function(err) {
          delphi_stan_result(list(expert_num = idx, model_label = fut_label,
                                   code = paste("Error:", conditionMessage(err))))
          delphi_stan_in_flight(FALSE)
          delphi_stan_active_expert(NULL)
          shinyjs::enable(btn_id)
        }
      )
    }, ignoreInit = TRUE)
  }

  # -----------------------------------------------------------------------
  # Round indicator
  # -----------------------------------------------------------------------
  output$delphi_round_indicator <- renderUI({
    round <- delphi_current_round()
    total <- MAX_ROUNDS
    if (round == 0L) {
      tags$span(class = "badge bg-secondary", style = "font-size: 1.1em; padding: 8px 16px;",
                "No rounds completed")
    } else {
      tags$span(class = "badge bg-info", style = "font-size: 1.1em; padding: 8px 16px;",
                paste0("Round ", round, " / ", total))
    }
  })

  # -----------------------------------------------------------------------
  # In-flight status indicator
  # Updated for logical delphi_in_flight
  # -----------------------------------------------------------------------
  output$delphi_flight_status <- renderUI({
    if (isTRUE(delphi_in_flight())) {
      # Read status to get expert count
      rd_status <- delphi_round_status()
      n <- length(rd_status)
      div(class = "alert alert-info p-2 mb-0", style = "font-size: 0.85em; text-align: center;",
          tags$span(class = "expert-spinner"),
          paste0("Querying ", n, " expert", if (n > 1) "s" else "", "..."))
    } else {
      NULL
    }
  })

  # -----------------------------------------------------------------------
  # Disable/enable Run button
  # -----------------------------------------------------------------------
  observe({
    in_flight <- delphi_in_flight()
    round     <- delphi_current_round()
    if (isTRUE(in_flight) || round >= MAX_ROUNDS) {
      shinyjs::disable("delphi_run_btn")
    } else {
      shinyjs::enable("delphi_run_btn")
    }
  })

  # -----------------------------------------------------------------------
  # Bug 13: Disable/enable Reset button during in-flight
  # -----------------------------------------------------------------------
  observe({
    if (isTRUE(delphi_in_flight())) {
      shinyjs::disable("delphi_reset_btn")
    } else {
      shinyjs::enable("delphi_reset_btn")
    }
  })

  # -----------------------------------------------------------------------
  # Bug 8: Locked configuration alert
  # -----------------------------------------------------------------------
  output$delphi_locked_config_alert <- renderUI({
    cfg <- delphi_locked_config()
    if (is.null(cfg)) return(NULL)

    model_str <- paste(
      vapply(seq_along(cfg$expert_models), function(i) {
        paste0("Expert ", i, ": ", cfg$expert_models[[i]])
      }, character(1)),
      collapse = "; "
    )

    div(
      class = "alert alert-info p-2 mt-2 mb-0",
      style = "font-size: 0.85em;",
      icon("lock"),
      tags$strong(" Expert configuration locked for this Delphi session. "),
      tags$span(paste0(cfg$n_experts, " experts: ", model_str, ".")),
      tags$span(" Reset to change configuration.", style = "font-style: italic;")
    )
  })

  # -----------------------------------------------------------------------
  # Reset handler
  # -----------------------------------------------------------------------
  observeEvent(input$delphi_reset_btn, {
    delphi_rounds(list())
    delphi_current_round(0L)
    delphi_in_flight(FALSE)
    delphi_round_status(list())
    delphi_locked_config(NULL)       # Bug 8: unlock config on reset
    delphi_round_warnings(list())    # Bug 9/3: clear warnings
    delphi_stan_result(NULL)         # Clear Stan code generation state
    delphi_stan_in_flight(FALSE)
    delphi_stan_active_expert(NULL)
    # Reset observer tracking so they re-register on next converged render
    delphi_stan_observers <<- list()
    shinyjs::enable("delphi_run_btn")
  })

  # -----------------------------------------------------------------------
  # Build the Delphi prompt for a given expert in round >= 2
  # Returns: list(prompt = character, n_peers_with_data = integer)
  # Bug 9/3: also returns count of peers with parseable data
  # -----------------------------------------------------------------------
  build_delphi_prompt <- function(round_num, expert_idx, prev_round_results,
                                  clinical_context, n_experts,
                                  data_variables = "") {
    base_prompt <- proposal_prompt(clinical_context, data_variables)

    if (round_num == 1L) {
      return(list(prompt = base_prompt, n_peers_with_data = NA_integer_))
    }

    # Collect other experts' distributions from the previous round
    other_dists_lines <- character(0)
    n_peers_total <- 0L
    n_peers_with_data <- 0L

    for (r in prev_round_results) {
      if (r$expert_num == expert_idx) next
      n_peers_total <- n_peers_total + 1L

      dist_strs <- r$distributions
      if (length(dist_strs) == 0 || (length(dist_strs) == 1 && dist_strs[1] == "Could not parse")) next
      n_peers_with_data <- n_peers_with_data + 1L

      label <- paste0("Expert ", r$expert_num, " (", r$model_label, ")")
      for (d in dist_strs) {
        lbl <- names(dist_strs)[which(dist_strs == d)[1]]
        if (!is.null(lbl) && nchar(lbl) > 0) {
          other_dists_lines <- c(other_dists_lines, paste0("  - ", label, ": ", lbl, " ~ ", d))
        } else {
          other_dists_lines <- c(other_dists_lines, paste0("  - ", label, ": ", d))
        }
      }
    }

    if (length(other_dists_lines) == 0) {
      # No parseable distributions from others -- fall back to base prompt
      return(list(prompt = base_prompt, n_peers_with_data = 0L))
    }

    prompt <- paste0(
      base_prompt, "\n\n",
      "## Delphi Round ", round_num, " -- Peer Review\n\n",
      "In the previous round, other experts suggested the following prior distributions:\n\n",
      paste(other_dists_lines, collapse = "\n"), "\n\n",
      "Please review these proposals and reconsider your own prior distributions. ",
      "You may revise your priors in light of the other experts' suggestions, or keep them if you believe they are well-justified. ",
      "Respond with your updated prior distribution(s) in the same format: `DistributionName(param1=value, param2=value)`, one per line."
    )

    list(prompt = prompt, n_peers_with_data = n_peers_with_data)
  }

  # -----------------------------------------------------------------------
  # Run Delphi Round
  # -----------------------------------------------------------------------
  observeEvent(input$delphi_run_btn, {
    # Bug 8: Use locked config if available, otherwise read from sidebar
    locked_cfg <- delphi_locked_config()

    if (!is.null(locked_cfg)) {
      n_experts     <- locked_cfg$n_experts
      expert_models <- locked_cfg$expert_models
    } else {
      n_experts     <- max(1L, min(4L, num_experts_input()))
      expert_models <- expert_model_inputs()
    }

    # Bug 11: Require at least 2 experts for Delphi
    if (n_experts < 2L) {
      showNotification("Delphi method requires at least 2 experts.", type = "warning")
      return()
    }

    clinical_context <- clinical_context_input()
    data_variables   <- data_variables_input()
    url              <- active_url()
    api_key          <- active_key()

    if (nchar(trimws(clinical_context)) == 0) {
      showNotification("Please enter the clinical context in the sidebar.", type = "error")
      return()
    }
    if (nchar(url) == 0 || nchar(api_key) == 0) {
      showNotification("Please provide a Base URL and API Key in the sidebar.", type = "error")
      return()
    }

    current_round <- delphi_current_round()
    if (current_round >= MAX_ROUNDS) {
      showNotification(paste0("Maximum of ", MAX_ROUNDS, " rounds reached. Reset to start over."), type = "warning")
      return()
    }

    new_round <- current_round + 1L

    # Bug 8: Lock config on first round
    if (is.null(locked_cfg)) {
      delphi_locked_config(list(
        n_experts     = n_experts,
        expert_models = expert_models
      ))
    }

    # Get previous round results (for rounds >= 2)
    prev_round_results <- if (new_round > 1L) delphi_rounds()[[new_round - 1L]] else list()

    # Initialize per-expert status for this round
    init_status <- list()
    for (i in seq_len(n_experts)) {
      model_label <- expert_models[[i]]
      init_status[[as.character(i)]] <- list(
        status = "querying", model_label = model_label, expert_num = i
      )
    }
    delphi_round_status(init_status)
    delphi_in_flight(TRUE)
    shinyjs::disable("delphi_run_btn")

    # -------------------------------------------------------------------
    # Bug 1,2,5 fix: Use promise_all instead of per-future accumulation
    #
    # Each individual future catches its own errors and returns a result
    # object (never rejects), so promise_all always resolves.
    # Individual futures can still update per-expert spinner status.
    # Round finalization happens atomically in the promise_all callback.
    # -------------------------------------------------------------------

    # Bug 9/3: Track per-expert peer data counts for partial feedback warning
    peer_data_counts <- list()

    futures_list <- lapply(seq_len(n_experts), function(i) {
      model_label  <- expert_models[[i]]
      model_id     <- available_models[[model_label]]

      # Capture all values needed inside future as plain R values
      fut_url        <- url
      fut_key        <- api_key
      fut_model_id   <- model_id
      fut_sys_prompt <- system_prompt_brief

      # Build per-expert prompt (Bug 9/3: now returns list with peer count)
      prompt_result <- build_delphi_prompt(
        round_num          = new_round,
        expert_idx         = i,
        prev_round_results = prev_round_results,
        clinical_context   = clinical_context,
        n_experts          = n_experts,
        data_variables     = data_variables
      )
      fut_prompt <- prompt_result$prompt
      peer_data_counts[[as.character(i)]] <<- prompt_result$n_peers_with_data

      # Launch future -- returns raw response text
      p <- promises::future_promise({
        library(ellmer)
        chat_obj <- chat_openai_compatible(
          base_url    = fut_url,
          name        = "CHAT-AI",
          credentials = local({ k <- fut_key; function() k }),
          model       = fut_model_id,
          system_prompt = fut_sys_prompt
        )
        chat_obj$chat(fut_prompt)
      }, seed = NULL)

      # Per-future success handler: update spinner status + return result object
      # (this runs in the Shiny session context, so reactive writes are safe)
      p_handled <- p %...>% (function(response_text) {
        # Parse distributions from the response
        distributions <- parse_prior_distribution(response_text)

        # Update per-expert status to "done" (spinner -> checkmark)
        st <- isolate(delphi_round_status())
        st[[as.character(i)]] <- list(
          status = "done", model_label = model_label, expert_num = i
        )
        delphi_round_status(st)

        # Return result object (never throws)
        list(
          expert_num    = i,
          model_label   = model_label,
          model_id      = model_id,
          response      = response_text,
          distributions = distributions,
          error         = FALSE
        )
      })

      # Per-future error handler: update spinner status + return error result
      p_handled <- p_handled %...!% (function(err) {
        err_msg <- conditionMessage(err)

        st <- isolate(delphi_round_status())
        st[[as.character(i)]] <- list(
          status = "error", model_label = model_label,
          expert_num = i, message = err_msg
        )
        delphi_round_status(st)

        # Return error result object (does NOT reject promise)
        list(
          expert_num    = i,
          model_label   = model_label,
          model_id      = model_id,
          response      = paste("Error querying model:", err_msg),
          distributions = "Could not parse",
          error         = TRUE
        )
      })

      p_handled
    })

    # Capture values needed in the finalization callback
    fin_new_round       <- new_round
    fin_max_rounds      <- MAX_ROUNDS
    fin_n_experts       <- n_experts
    fin_peer_data_counts <- peer_data_counts

    # Atomic finalization: all futures resolved (each caught its own errors)
    promises::promise_all(.list = futures_list) %...>% (function(results_list) {
      # results_list is a list of per-expert result objects, one per expert

      # Store this round's results
      rounds <- delphi_rounds()
      rounds[[fin_new_round]] <- results_list
      delphi_rounds(rounds)

      # Bug 10: Cap round counter at MAX_ROUNDS
      delphi_current_round(min(fin_new_round, fin_max_rounds))

      # Clear in-flight state
      delphi_in_flight(FALSE)

      # Re-enable Run button if more rounds remain
      if (fin_new_round < fin_max_rounds) {
        shinyjs::enable("delphi_run_btn")
      }

      # Bug 12: Check if ALL experts errored
      all_errored <- all(vapply(results_list, function(r) isTRUE(r$error), logical(1)))
      if (all_errored) {
        showNotification(
          paste0("Round ", fin_new_round, ": All experts failed. ",
                 "Check your API connection and try again."),
          type = "warning", duration = 8
        )
      }

      # Bug 9/3: Check for partial peer feedback and store per-round warning
      if (fin_new_round > 1L) {
        # Count how many peers had parseable data (use first expert's count
        # as representative -- all experts see the same set of peers minus themselves)
        counts <- unlist(fin_peer_data_counts)
        counts <- counts[!is.na(counts)]
        max_possible_peers <- fin_n_experts - 1L

        if (length(counts) > 0 && any(counts < max_possible_peers)) {
          min_peers <- min(counts)
          warnings <- delphi_round_warnings()
          warnings[[as.character(fin_new_round)]] <- paste0(
            "Note: Only ", min_peers, " of ", max_possible_peers,
            " peers had parseable distributions from the previous round. ",
            "Some experts received incomplete peer feedback."
          )
          delphi_round_warnings(warnings)
        }
      }


    }) %...!% (function(err) {
      # Catch-all for unexpected promise_all failure (should not happen since
      # each individual future catches its own errors)
      delphi_in_flight(FALSE)
      shinyjs::enable("delphi_run_btn")
      showNotification(
        paste0("Unexpected error in Delphi round: ", conditionMessage(err)),
        type = "error", duration = 10
      )
    })
  })

  # -----------------------------------------------------------------------
  # Render per-round accordion panels
  # -----------------------------------------------------------------------
  output$delphi_rounds_ui <- renderUI({
    rounds    <- delphi_rounds()
    rd_status <- delphi_round_status()
    in_flight <- delphi_in_flight()
    warnings  <- delphi_round_warnings()

    if (length(rounds) == 0 && !isTRUE(in_flight)) {
      return(div(
        class = "text-center text-muted mt-4 mb-4",
        h5("Click 'Run Delphi Round' to begin multi-round elicitation."),
        p(style = "font-size: 0.9em;",
          "Each round, experts will see the other experts' distributions from the previous round and can revise their priors.")
      ))
    }

    base_colors   <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                        "#9467bd", "#8c564b", "#e377c2", "#7f7f7f")

    # Build accordion items -- one per completed round
    accordion_items <- lapply(seq_along(rounds), function(round_idx) {
      round_data <- rounds[[round_idx]]
      sorted_data <- round_data[order(sapply(round_data, `[[`, "expert_num"))]

      n <- length(sorted_data)
      col_width <- max(3, floor(12 / max(n, 1)))

      expert_cards <- lapply(sorted_data, function(r) {
        is_error <- isTRUE(r$error) || grepl("^Error querying model:", r$response)
        is_latest <- round_idx == length(rounds)

        if (is_error) {
          column(col_width,
            div(class = "card h-100 border-danger",
              div(class = "card-header bg-danger text-white",
                strong(paste("Expert", r$expert_num)), br(), tags$small(r$model_label)),
              div(class = "card-body",
                div(class = "alert alert-danger", style = "font-size: 0.88em;",
                  tags$strong("Error: "), sub("^Error querying model:\\s*", "", r$response)))
            )
          )
        } else {
          response_html <- render_markdown(r$response)
          # Distribution summary
          dist_strs <- r$distributions
          dist_badge <- if (length(dist_strs) > 0 &&
                            !(length(dist_strs) == 1 && dist_strs[1] == "Could not parse")) {
            div(
              style = "margin-top: 8px; padding: 6px 10px; background: #f8f9fa; border-radius: 4px; font-size: 0.85em;",
              tags$strong("Extracted: "),
              lapply(seq_along(dist_strs), function(k) {
                lbl <- names(dist_strs)[k]
                prefix <- if (!is.null(lbl) && nchar(lbl) > 0) paste0(lbl, " ~ ") else ""
                tags$div(tags$code(paste0(prefix, dist_strs[k])))
              })
            )
          } else {
            div(style = "margin-top: 8px;",
                tags$em(class = "text-muted", style = "font-size: 0.85em;",
                        "Could not parse distribution"))
          }

          # Stan button in header for latest round only
          card_header <- if (is_latest) {
            idx <- r$expert_num
            key <- as.character(idx)
            if (is.null(delphi_stan_observers[[key]])) {
              make_delphi_stan_observer(idx)
              delphi_stan_observers[[key]] <<- TRUE
            }
            active_exp <- delphi_stan_active_expert()
            btn_label <- if (isTRUE(delphi_stan_in_flight()) && identical(active_exp, idx))
              tagList(tags$span(class = "expert-spinner")) else icon("code")
            div(
              class = "card-header bg-primary text-white",
              style = "display: flex; justify-content: space-between; align-items: center;",
              div(strong(paste("Expert", r$expert_num)), br(), tags$small(r$model_label)),
              actionButton(
                paste0("delphi_stan_btn_", idx), btn_label,
                class = "btn btn-sm btn-light",
                style = "white-space: nowrap;"
              )
            )
          } else {
            div(class = "card-header bg-primary text-white",
              strong(paste("Expert", r$expert_num)), br(), tags$small(r$model_label))
          }

          column(col_width,
            div(class = "card h-100",
              card_header,
              div(class = "card-body",
                style = "font-size: 0.88em; overflow-y: auto; max-height: 500px;",
                response_html,
                dist_badge
              )
            )
          )
        }
      })

      # Bug 9/3: Show per-round warning if partial peer feedback
      round_warning <- warnings[[as.character(round_idx)]]
      warning_alert <- if (!is.null(round_warning)) {
        div(
          class = "alert alert-warning p-2 mt-1 mb-2",
          style = "font-size: 0.85em;",
          icon("exclamation-triangle"),
          round_warning
        )
      } else NULL

      # Wrap in a details/summary element to form an accordion-like panel
      round_label <- paste0("Round ", round_idx)
      round_badge <- if (round_idx == length(rounds) && !isTRUE(in_flight)) {
        tags$span(class = "badge bg-success ms-2", "Latest")
      } else NULL

      div(
        style = "margin-bottom: 12px;",
        tags$details(
          open = if (round_idx == length(rounds)) "open" else NULL,
          tags$summary(
            style = "cursor: pointer; padding: 10px 14px; background: #ecf0f1; border-radius: 4px; font-size: 1.05em;",
            tags$strong(round_label), round_badge
          ),
          warning_alert,
          div(style = "padding: 12px 0;", fluidRow(expert_cards))
        )
      )
    })

    # If a round is currently in flight, show a loading panel
    loading_panel <- if (isTRUE(in_flight)) {
      pending_round <- length(rounds) + 1L
      n_status <- length(rd_status)
      col_width <- max(3, floor(12 / max(n_status, 1)))

      loading_cards <- lapply(names(rd_status), function(key) {
        st <- rd_status[[key]]
        if (st$status == "querying") {
          column(col_width,
            div(class = "card h-100 expert-loading-card",
              div(class = "card-header bg-secondary text-white",
                strong(paste("Expert", st$expert_num)), br(), tags$small(st$model_label)),
              div(class = "card-body",
                div(tags$span(class = "expert-spinner"),
                    paste0("Querying ", st$model_label, " (Round ", pending_round, ")...")))
            )
          )
        } else if (st$status == "done") {
          column(col_width,
            div(class = "card h-100 border-success",
              div(class = "card-header bg-success text-white",
                strong(paste("Expert", st$expert_num)), br(), tags$small(st$model_label)),
              div(class = "card-body text-center",
                tags$span(class = "text-success", style = "font-size: 1.3em;", icon("check")),
                " Done"
              )
            )
          )
        } else {
          column(col_width,
            div(class = "card h-100 border-danger",
              div(class = "card-header bg-danger text-white",
                strong(paste("Expert", st$expert_num)), br(), tags$small(st$model_label)),
              div(class = "card-body",
                div(class = "alert alert-danger p-1", style = "font-size: 0.85em;",
                    st$message %||% "Error"))
            )
          )
        }
      })

      div(
        style = "margin-bottom: 12px;",
        tags$details(
          open = "open",
          tags$summary(
            style = "cursor: pointer; padding: 10px 14px; background: #f39c12; color: white; border-radius: 4px; font-size: 1.05em;",
            tags$strong(paste0("Round ", pending_round, " (in progress)"))
          ),
          div(style = "padding: 12px 0;", fluidRow(loading_cards))
        )
      )
    } else NULL

    tagList(
      accordion_items,
      loading_panel,
      tags$script("if(window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);")
    )
  })

  # -----------------------------------------------------------------------
  # Parse all distributions from the latest round (for converged plot)
  # -----------------------------------------------------------------------
  delphi_parsed_distributions <- reactive({
    rounds <- delphi_rounds()
    if (length(rounds) == 0) return(list())

    # Use the latest completed round
    latest_round <- rounds[[length(rounds)]]
    all_dists <- list()

    for (r in latest_round) {
      parsed_strs <- r$distributions
      if (length(parsed_strs) == 1 && parsed_strs[1] == "Could not parse") next

      family_counter <- list()
      for (k in seq_along(parsed_strs)) {
        ds  <- parsed_strs[k]
        lbl <- names(parsed_strs)[k]
        p   <- parse_dist_params(ds)
        if (!is.null(p)) {
          fam <- p$family
          family_counter[[fam]] <- (family_counter[[fam]] %||% 0L) + 1L
          all_dists[[length(all_dists) + 1]] <- list(
            expert_idx   = r$expert_num,
            expert_num   = r$expert_num,
            expert_label = paste0("Expert ", r$expert_num, " - ", r$model_label),
            model_label  = r$model_label,
            family       = fam,
            param_idx    = family_counter[[fam]],
            param_label  = if (!is.null(lbl) && nchar(lbl) > 0) lbl else NULL,
            params       = p$params,
            dist_str     = ds
          )
        }
      }
    }
    all_dists
  })

  # -----------------------------------------------------------------------
  # Converged distributions UI
  # -----------------------------------------------------------------------
  output$delphi_converged_ui <- renderUI({
    rounds <- delphi_rounds()
    in_flight <- delphi_in_flight()
    if (length(rounds) == 0 || isTRUE(in_flight)) return(NULL)

    all_dists <- delphi_parsed_distributions()
    current_round <- delphi_current_round()

    tagList(
      hr(),
      h4(paste0("Converged Distributions (after Round ", current_round, ")")),
      if (current_round < MAX_ROUNDS) {
        p(style = "font-size: 0.9em; color: #555;",
          "You can run more rounds to further converge, or use these distributions as the final result.")
      } else {
        p(style = "font-size: 0.9em; color: #555;",
          "All ", MAX_ROUNDS, " rounds completed. These are the final Delphi-converged distributions.")
      },
      # Summary table (with Stan code generation buttons)
      uiOutput("delphi_converged_table"),
      # Density plot
      uiOutput("delphi_converged_plot_container"),
      # Stan code card (shown when Stan code is generated or in-flight)
      uiOutput("delphi_stan_card_ui")
    )
  })

  # -----------------------------------------------------------------------
  # Converged distributions summary table
  # -----------------------------------------------------------------------
  output$delphi_converged_table <- renderUI({
    all_dists <- delphi_parsed_distributions()
    if (length(all_dists) == 0) {
      return(p("No distributions could be parsed from the latest round.", class = "text-muted"))
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

    tags$table(
      class = "table table-bordered table-striped table-sm",
      tags$thead(tags$tr(
        tags$th("Expert"), tags$th("Model"), tags$th("Distribution"),
        tags$th("Family"), tags$th("Parameters")
      )),
      tags$tbody(rows)
    )
  })

  # -----------------------------------------------------------------------
  # Stan code card -- shown below converged plot once code is available
  # (Mirrors output$stan_card_ui in expert_responses.R)
  # -----------------------------------------------------------------------
  output$delphi_stan_card_ui <- renderUI({
    in_flight <- delphi_stan_in_flight()
    result    <- delphi_stan_result()

    if (!in_flight && is.null(result)) return(NULL)

    header_text <- if (in_flight) {
      tagList(tags$span(class = "expert-spinner"), " Generating Stan model...")
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

  # -----------------------------------------------------------------------
  # Dynamic plot height container
  # -----------------------------------------------------------------------
  output$delphi_converged_plot_container <- renderUI({
    all_dists <- delphi_parsed_distributions()
    if (length(all_dists) == 0) return(plotOutput("delphi_converged_plot", height = "200px"))

    groups <- unique(lapply(all_dists, function(d) list(family = d$family, param_idx = d$param_idx)))
    n_grps <- length(groups)
    n_col  <- if (n_grps <= 1) 1 else if (n_grps == 2) 2 else 3
    n_row  <- ceiling(n_grps / n_col)
    plotOutput("delphi_converged_plot", height = paste0(max(350, n_row * 320), "px"))
  })

  # -----------------------------------------------------------------------
  # Converged distributions density plot (with opinion pooling overlay)
  # -----------------------------------------------------------------------
  output$delphi_converged_plot <- renderPlot({
    all_dists <- delphi_parsed_distributions()
    if (length(all_dists) == 0) {
      par(mar = c(1, 1, 1, 1)); plot.new()
      text(0.5, 0.5,
           "No distributions could be parsed from the latest round.\nRun a Delphi round first.",
           cex = 1.2, col = "grey40", font = 3)
      return(invisible(NULL))
    }

    base_colors   <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                        "#9467bd", "#8c564b", "#e377c2", "#7f7f7f")
    n_experts     <- max(sapply(all_dists, `[[`, "expert_idx"))
    expert_colors <- rep_len(base_colors, n_experts)

    # Helper: resolve best param label for a group
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

    current_round <- delphi_current_round()

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
             main = fam_label, xaxt = "n")
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

        leg_labels <- c(sapply(grp_dists, function(d) paste0("Expert ", d$expert_num, " (", d$model_label, ")")), "Pooled")
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
             xlab = "Parameter value", ylab = "Density", main = fam_label)
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

        leg_labels <- c(sapply(grp_dists, function(d) paste0("Expert ", d$expert_num, " (", d$model_label, ")")), "Pooled")
        leg_cols   <- c(sapply(grp_dists, function(d) adjustcolor(expert_colors[d$expert_idx], alpha.f = 0.5)), "black")
        leg_lty    <- c(lty_cycle[((seq_along(grp_dists) - 1) %% 4) + 1], 1)
        legend("topright", legend = leg_labels, col = leg_cols,
               lty = leg_lty, lwd = c(rep(1.8, n_exp), 3.5), cex = 0.65, bg = "white")
      }
    }

    mtext(paste0("Delphi Converged Distributions (Round ", current_round, ")"),
          outer = TRUE, cex = 1.1, font = 2)
  })
}

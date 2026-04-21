function(input, output, session) {

  # Pre-populate fields from env vars
  observe({
    if (nchar(env_url) > 0) updateTextInput(session, "base_url", value = env_url)
    if (nchar(env_key) > 0) updateTextInput(session, "api_key",  value = env_key)
  })

  active_url <- reactive({ url <- trimws(input$base_url); if (nchar(url) == 0) env_url else url })
  active_key <- reactive({ k   <- trimws(input$api_key);  if (nchar(k)   == 0) env_key else k })

  output$connection_status <- renderUI({
    url_ok <- nchar(active_url()) > 0
    key_ok <- nchar(active_key()) > 0
    if (url_ok && key_ok) {
      div(class = "alert alert-success p-1 mt-1", style = "font-size:0.8em;", "URL and key provided")
    } else {
      missing <- c(if (!url_ok) "Base URL", if (!key_ok) "API Key")
      div(class = "alert alert-warning p-1 mt-1", style = "font-size:0.8em;",
          paste("Missing:", paste(missing, collapse = ", ")))
    }
  })

  output$expert_selectors <- renderUI({
    n <- max(1, min(4, input$num_experts))
    model_names <- names(available_models)
    lapply(seq_len(n), function(i) {
      fluidRow(
        tags$label(paste("Expert", i), class = "col-12 col-form-label pb-0 pt-2"),
        column(8,
          selectInput(paste0("expert_model_", i), NULL,
                      choices = model_names, selected = model_names[min(i, length(model_names))])
        ),
        column(4,
          selectInput(paste0("expert_temp_", i), NULL,
                      choices = c("0.1" = "0.1", "0.5" = "0.5", "1.0" = "1.0"),
                      selected = "0.5")
        )
      )
    })
  })

  # Shared reactive state
  expert_results      <- reactiveVal(list())
  elicitation_context <- reactiveVal(NULL)
  expert_status       <- reactiveVal(list())
  experts_in_flight   <- reactiveVal(0L)

  observeEvent(input$elicit_btn, {
    n                <- max(1, min(4, input$num_experts))
    clinical_context <- input$clinical_context
    topic            <- input$parameter_topic

    if (nchar(trimws(clinical_context)) == 0) { showNotification("Please enter the clinical context.", type = "error"); return() }
    if (nchar(trimws(topic)) == 0)             { showNotification("Please enter the parameter to elicit.", type = "error"); return() }

    url     <- active_url()
    api_key <- active_key()
    if (nchar(url) == 0 || nchar(api_key) == 0) { showNotification("Please provide a Base URL and API Key.", type = "error"); return() }

    expert_results(list())
    elicitation_context(list(clinical_context = clinical_context, parameter_topic = topic))

    prompt <- elicitation_prompt(clinical_context, topic)

    init_status <- list()
    for (i in seq_len(n)) {
      model_label <- input[[paste0("expert_model_", i)]]
      init_status[[as.character(i)]] <- list(status = "querying", model_label = model_label, expert_num = i)
    }
    expert_status(init_status)
    experts_in_flight(n)
    shinyjs::disable("elicit_btn")

    for (i in seq_len(n)) {
      local({
        idx            <- i
        model_label    <- input[[paste0("expert_model_", idx)]]
        model_id       <- available_models[[model_label]]
        temperature    <- as.numeric(input[[paste0("expert_temp_", idx)]] %||% "0.5")
        fut_url        <- url
        fut_key        <- api_key
        fut_prompt     <- prompt
        fut_model_id   <- model_id
        fut_sys_prompt <- system_prompt_brief
        fut_temp       <- temperature

        p <- promises::future_promise({
          library(ellmer)
          chat_obj <- chat_openai_compatible(
            base_url    = fut_url, name = "CHAT-AI",
            credentials = local({ k <- fut_key; function() k }),
            model       = fut_model_id, system_prompt = fut_sys_prompt,
            params      = params(temperature = fut_temp)
          )
          chat_obj$chat(fut_prompt)
        }, seed = NULL)

        promises::then(p,
          onFulfilled = function(response_text) {
            result_entry <- list(expert_num = idx, model_label = model_label,
                                 model_id = model_id, response = response_text)
            current <- expert_results(); current[[length(current) + 1]] <- result_entry; expert_results(current)
            st <- expert_status(); st[[as.character(idx)]] <- list(status = "done", model_label = model_label, expert_num = idx); expert_status(st)
            remaining <- experts_in_flight() - 1L; experts_in_flight(remaining)
            if (remaining <= 0L) shinyjs::enable("elicit_btn")
          },
          onRejected = function(err) {
            err_msg      <- conditionMessage(err)
            result_entry <- list(expert_num = idx, model_label = model_label,
                                 model_id = model_id, response = paste("Error querying model:", err_msg))
            current <- expert_results(); current[[length(current) + 1]] <- result_entry; expert_results(current)
            st <- expert_status(); st[[as.character(idx)]] <- list(status = "error", model_label = model_label, expert_num = idx, message = err_msg); expert_status(st)
            remaining <- experts_in_flight() - 1L; experts_in_flight(remaining)
            if (remaining <= 0L) shinyjs::enable("elicit_btn")
          }
        )
      })
    }
  })

  output$elicitation_status <- renderUI({
    n <- experts_in_flight()
    if (n > 0L)
      div(class = "alert alert-info p-2 mt-2", style = "font-size: 0.85em; text-align: center;",
          tags$span(class = "expert-spinner"),
          paste0("Eliciting from ", n, " expert", if (n > 1) "s" else "", "..."))
    else NULL
  })

  # ---------------------------------------------------------------------------
  # Central shared reactive: parsed distributions
  # ---------------------------------------------------------------------------
  parsed_distributions <- reactive({
    results <- expert_results()
    if (length(results) == 0) return(list())

    all_dists <- list()
    for (i in seq_along(results)) {
      r           <- results[[i]]
      parsed_strs <- parse_prior_distribution(r$response)
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

  # ---------------------------------------------------------------------------
  # Delegate to page server modules
  # ---------------------------------------------------------------------------
  expert_responses_server(input, output, session,
                          expert_results, expert_status,
                          elicitation_context, parsed_distributions,
                          active_url = active_url,
                          active_key = active_key)

  prior_summary_server(input, output, session, parsed_distributions)

  opinion_pooling_server(input, output, session, parsed_distributions,
                         active_url          = active_url,
                         active_key          = active_key,
                         elicitation_context = elicitation_context)

  # Reactive wrappers for sidebar inputs consumed by the Delphi tab
  # Bug 15: NULL guards via req() to prevent passing NULL during startup
  delphi_num_experts <- reactive({
    req(input$num_experts)
    max(1L, min(4L, input$num_experts))
  })
  delphi_expert_models <- reactive({
    n <- delphi_num_experts()
    models <- list()
    for (i in seq_len(n)) {
      val <- input[[paste0("expert_model_", i)]]
      req(val)
      models[[i]] <- val
    }
    models
  })
  delphi_expert_temps <- reactive({
    n <- delphi_num_experts()
    temps <- list()
    for (i in seq_len(n)) {
      val <- input[[paste0("expert_temp_", i)]] %||% "0.5"
      temps[[i]] <- as.numeric(val)
    }
    temps
  })
  delphi_clinical_context <- reactive({
    req(input$clinical_context)
    input$clinical_context
  })
  delphi_parameter_topic <- reactive({
    req(input$parameter_topic)
    input$parameter_topic
  })

  delphi_server(input, output, session,
                active_url             = active_url,
                active_key             = active_key,
                num_experts_input      = delphi_num_experts,
                expert_model_inputs    = delphi_expert_models,
                expert_temp_inputs     = delphi_expert_temps,
                clinical_context_input = delphi_clinical_context,
                parameter_topic_input  = delphi_parameter_topic)
}

available_models <-list(
  "MedGemma-27b-it"="medgemma-27b-it",
  "Gemma-3-27b-it"="gemma-3-27b-it",
  "Deepseek-R1-L-70b"="deepseek-r1-distill-llama-70b",
  "Llama-3.3-70b" = "llama-3.3-70b-instruct",
  "Qwen-3-I-30b" = "qwen3-30b-a3b-instruct-2507",
  "Mistral-3-675B" = "mistral-large-3-675b-instruct-2512"
)

system_prompt_brief <- "You are a biostatistics expert specializing in clinical trials and Bayesian analysis."


coding_models <- list(
  "Qwen-Coder"       = "qwen3-coder-30b-a3b-instruct",
  "Devstral-2-123b"  = "devstral-2-123b-instruct-2512"
)

stan_coder_model_id <- coding_models[["Qwen-Coder"]]
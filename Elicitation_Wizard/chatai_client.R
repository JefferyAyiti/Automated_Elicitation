library(ellmer)
chatai_url <-Sys.getenv("BASE_URL")
key <- function(){Sys.getenv("API_KEY")}
chatai_model <- "meta-llama-3.1-8b-instruct"

system_prompt <- "You are a Zombie brought to life"

chat<-chat_openai_compatible(base_url = chatai_url, name = "CHAT-AI", credentials = key, model = chatai_model, system_prompt = system_prompt)
chat$chat("Checking your heartbeat")
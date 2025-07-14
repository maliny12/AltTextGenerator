library(tidyverse)
library(BrailleR)
library(ellmer)
library(usethis)



# Set up ---------------------------------
sysprompt <- readLines("prompt.txt")
readRenviron(".Renviron")
chat <- chat_openai(api_key = Sys.getenv("OPENAI_API_KEY"))

# Programmatic Chat -------------------

client_responses <- function(brailleR, image_path = NULL){
  chat <- chat_openai(
    model = "gpt-4.1",
    system_prompt = sysprompt
  )

  # Token limit : 30000
  if (sum(nchar(capture.output(VI(brailleR)))) >= 7500) {
    client_input = " "
  } else {
    client_input = brailleR
  }


  # tryCatch(
  #   {
  #     if (is.null(image)) {
  #       chat$chat(paste0(sysprompt,client_input))
  #     } else {
  #       chat$chat(paste0(sysprompt,client_input),content_image_file(image_path))
  #       chat$get_tokens()
  #     }
  #   },
  #   error = function(cond) {
  #     message(paste("Prompt casue a warning"))
  #     chat$get_tokens()
  #   }
  # )

  if (is.null(image)) {
    chat$chat(paste0(sysprompt,client_input))
  } else {
    chat$chat(paste0(sysprompt,client_input),content_image_file(image_path))
  }


}

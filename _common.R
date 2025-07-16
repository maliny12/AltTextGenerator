library(tidyverse)
library(BrailleR)
library(ellmer)
library(usethis)



# Set up ---------------------------------
sysprompt <- readLines("prompt.txt")
readRenviron(".Renviron")
chat <- chat_openai(api_key = Sys.getenv("OPENAI_API_KEY"))

# Programmatic Chat -------------------

client_responses <- function(brailleR, image_path = NULL){ # nolint # nolint

  chat <- chat_openai(
    model = "gpt-4.1",
    system_prompt = sysprompt
  )


  client_input <- " "
  # Token limit : 30000 / 7500 char
  # System prompt: 763 char
  tryCatch(
    {
      if (sum(nchar(capture.output(VI(brailleR)))) >= 6700) {
        client_input <- " "
      } else {
        client_input <- brailleR
      }
    },
    error = function(e){
      warning("Issue with BrailleR")
      class(client_input) <- "error"
    }
  )


  # In case BrailleR throws an error
  if(inherits(client_input, "error")){
    client_input <- " "
  }


  # tryCatch(
  #   {
  #     if (is.null(image)) {
  #       chat$chat(paste0(sysprompt,client_input)) # nolint
  #     } else {
  #       chat$chat(paste0(sysprompt,client_input),content_image_file(image_path)) # nolint # nolint
  #       chat$get_tokens() # nolint
  #     }
  #   },
  #   error = function(cond) {
  #     message(paste("Prompt casue a warning")) # nolint # nolint
  #     chat$get_tokens() # nolint # nolint
  #   }
  # )

  if (is.null(image_path)) {
    chat$chat(paste0(sysprompt,client_input)) # nolint
  } else {
    chat$chat(paste0(sysprompt,client_input),content_image_file(image_path)) # nolint
  }


}


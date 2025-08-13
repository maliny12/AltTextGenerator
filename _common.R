library(tidyverse)
library(BrailleR)
library(ellmer)
library(usethis)



# Set up ---------------------------------
sysprompt <- readLines("prompt.txt")

# Programmatic Chat -------------------

client_responses <- function(body_list){


  chat <- chat_openai(
    model = body_list$model,
    api_key = body_list$api_key,
    system_prompt = paste0(body_list$sysinstruct,sysprompt),
    params = params(
                temperature = body_list$temp,
                top_p = body_list$top_p,
                max_tokens = body_list$max_token
    )
  )


    if (nzchar(body_list$input_code)) {

    client_input <- " "
    # Token limit : 30000 / 7500 char
    # System prompt: 763 char
    tryCatch(
      {
        if (sum(nchar(capture.output(VI(body_list$input_code)))) >= body_list$max_token) {
          client_input <- " "
        } else {
          client_input <- VI(body_list$input_code)
        }
      },
      error = function(e){
        showModel(modelDialog(
          title = "Request failed",
          paste("Error:", e$message),
          easyClose = TRUE
        ))
        class(client_input) <- "error"
      }
    )

    # In case BrailleR throws an error
    if(inherits(client_input, "error")){
      client_input <- paste0("Interpret this code and use the interpreation to generate alt-text", body_list$input_code)
    }

    } else {
      client_input <- " "
  }

  if (nzchar(body_list$user_message)) {
    client_input <- paste0(body_list$user_message, client_input)
  }


  if (nzchar(body_list$input_caption)) {
    client_input <- paste0(client_input,"Use the caption to help interpret the image/R code, ensuring the alt-text differs from it.", body_list$input_caption)
  }

  inFile <- body_list[["input_image"]]

  if (!is.null(inFile)) {
    chat$chat(paste0(sysprompt,client_input), content_image_file(inFile$datapath))
  } else if (!is.null(body_list$rendered_image)) {
    chat$chat(paste0(sysprompt,client_input), content_image_file(body_list$rendered_image))
  } else {
    chat$chat(paste0(sysprompt,client_input))
  }

}



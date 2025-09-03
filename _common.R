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
        vi_expression <-  paste0("VI({\n", paste(body_list$input_code, collapse = "\n"), "\n})")
        brailleR_output <- eval(parse(text = vi_expression))


        if (sum(nchar(brailleR_output$text)) >= body_list$max_token) {
          client_input <- " "
        } else {
          client_input <- brailleR_output$text
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
    client_input <- paste0("BrailleR input: ", client_input, collapse = "")
    response <- chat$chat(paste0(sysprompt,client_input), content_image_file(inFile$datapath))

    usage <- paste0("<span style='color: #2c9373;'> Image and BrailleR", "</span> &nbsp;  &nbsp;",
                "Cost: <span style='color: #2c9373;'>", round(chat$get_cost()[1], 3),  "</span> &nbsp;  &nbsp; ",
                "Token Usage: <span style='color: #2c9373;'>", sum(chat$get_tokens()[3])[1] , "</span> &nbsp;  &nbsp; "
    )
    
    c(response, usage)
  # } else if (!is.null(body_list$rendered_image)) {
  #   chat$chat(paste0(sysprompt,client_input), content_image_file(body_list$rendered_image))
  # } else {
  } else {
    client_input <- paste0("BrailleR input: ", client_input, collapse = "")
    response <- chat$chat(paste0(sysprompt, client_input))
    usage <- paste0("<span style='color: #2c9373;'> BrailleR", "</span> &nbsp;  &nbsp;",
                "Cost: <span style='color: #2c9373;'>", round(chat$get_cost()[1], 3),  "</span> &nbsp;  &nbsp; ",
                "Token Usage: <span style='color: #2c9373;'>", sum(chat$get_tokens()[3])[1] , "</span> &nbsp;  &nbsp; "
    )

    c(response, usage)
  }

}



library(shiny)
library(httr)
library(jsonlite)
library(shinyjs)
library(magrittr)
library(base64enc)
library(shinycssloaders)
source("_common.R")

# can just add css file in a folder
ui <- fluidPage(
  useShinyjs(),
  tags$style(HTML("
        #spinner { display: none; margin: 10px auto; }

        html, body {
        margin: 0;
        padding: 0;
        height: 100vh;
        width: 100vw;
        overflow: hidden;
        background-color: #1e1e1e;
        font-family: 'Segoe UI', sans-serif;
        color: #e0e0e0;
        }

        .col-sm-3,.col-sm-9 {
        height: 100%;
       display: flex;
       flex-direction: column;
       padding: 0;
        }

        .shiny-html-output {
        min-height: 76vh !important;
        }

        .shiny-input-container:not(.shiny-input-container-inline) {
         width: 100% !important;
        }

        .row {
        margin-right: 0;
        margin-left: 0;
        }


        #settings_panel {
        position: absolute;
        right: 0;
        background-color: #2c2c2c;
        border: 1px solid #444;
        padding: 15px;
        color: white;
        z-index: 9999;
        display: none;
        box-shadow: 0px 4px 8px rgba(0,0,0,0.5);
        border-radius: 10px !important;
        transform: translate(0, 40px);
        }

        #code_input.shiny-input-textarea {
        height: 22em !important;
        }

      .setting-summary {
        font-family: monospace;
        font-size: 14px;
        margin-top: 10px;
        margin-bottom: 10px;
      }

      .model_dropdown {
        left: 0;
      }

      .model_config {
        position: relative;
        displays: flex;
      }

      .control-panel {
        background-color: #1e1e1e;
        padding: 20px;
        color: white;
        font-family: Arial;
      }

      .main-chat-panel {
       height: 80%;
      }

      .shiny-html-output {
      overflow-y: auto;
      min-height: 10vh;
      max-height: 65vh;
      /*text-align: center;*/
      }

      #chat_history {
  display: flex;
  flex-direction: column;
  margin: 0 5rem;
}

.user-chat {
  /* margin-left: 20%; */
    padding: 1em;
  border: 1px solid white;
  background-color: #323232d9;
  border-radius: 10px;
  width: fit-content;
  align-self: flex-end;
  max-width: 80%;
  margin-top: 1rem;
}

.ai-chat {
  /* margin-right: 20%; */
    padding: 1em;
  border: 1px solid white;
  background-color: steelblue;
  border-radius: 10px;
  margin-top: 1rem;
  max-width: 80%;
  width: fit-content;
  align-self: flex-start;
}

  ")),
  tags$head(
    tags$script(src = "script.js")
  ),
  fluidRow(
      column(
        width = 3,
        div(
          class = "control-panel",
          tags$hr(style = "margin-top: 10px; margin-bottom: 20px;"),
          fluidRow(
            h5("OpenAI Api Key"),
            textInput("api_key", label = NULL, placeholder = "Enter OpenAI API keys")
          ),
          fluidRow(
            h5("Model"),
            div(
              class = "model_dropdown",
              fluidRow(
                style = "display: flex",
                column(
                  width = 9,
                  div(
                    selectInput(
                      "modelselection", NULL,
                      choices = list(
                        "gpt-4.1" = "gpt-4.1",
                        "gpt-4.1-mini" = "gpt-4.1-mini",
                        "gpt-4.1-nano" = "gpt-4.1-nano"
                      )
                    )
                  )
                ),
                column(
                  width = 1,
                  div()
                ),
                column(
                  width = 2,
                  class = "model-config",
                  style = "float: right; padding-right:0px",
                  actionButton("show_setting", icon = icon("sliders"), label = NULL, style = "width: unset; float: right;"),
                  div(
                    style="min-width: 300px",
                    id = "settings_panel",
                    h5("Temperature"),
                    sliderInput("temperature", label = NULL, min = 0.00, max = 2.00, value = 1.00),
                    h5("Max Tokens"),
                    sliderInput("max_tokens", label = NULL, min = 1, max = 32000, value = 2048),
                    h5("Top P"),
                    sliderInput("top_p", label = NULL, min = 0.00, max = 1.00, value = 1.00)
                  )
                )
              )
            )
          ),
          fluidRow(
            h5("System Instruction"),
            textAreaInput("sysinstruct", NULL, height = "80px", placeholder = "Talk like a pirate...")
          ),
          fluidRow(
            h5("Choose Image"),
            fileInput("image_input", label = NULL, accept ="image/png")
          ),
          fluidRow(
            h5("R Code"),
            textAreaInput("code_input", NULL, height = "80px",
                          placeholder = "ggplot(data = mpg) +
    geom_point(
    mapping = aes(x = displ,y = hwy)) +
    hello ()
                  ")
          ),
        )
      ),
      column(
        width = 9,
        fluidRow(
          div(
            class = "main-chat-panel",
            h3("Alt-Text Generator"),
            withSpinner(uiOutput("chat_history")),
            div(id = "spinner", class = "spinner-border", role = "status"),
            tags$hr(),
          )
        ),
        fluidRow(
          column(11, textAreaInput("user_message",label = NULL,  placeholder = "Chat with your prompt...", width = "100%")),
          column(1, actionButton("send_request",label = NULL , icon = icon("paper-plane"), class = "button"))
        )
      )
    )
  )






server <- function(input, output, session) {

  # output[["image"]] <- renderUI({
  #   if(!is.null(base64())){
  #     tags$div(
  #       tags$img(src= base64(), width="100%"),
  #       style = "width: 400px;"
  #     )
  #   }
  # })


  # Set up a reactive data frame
  chat_data <- reactiveVal(data.frame(source = character(0),
                                      message = character(0),
                                      stringsAsFactors = FALSE))

  observeEvent(input$send_request, {
    req(input$api_key)

    new_data <- data.frame(source = "User",
                           message = input$user_message, stringsAsFactors = FALSE)

    chat_data(rbind(chat_data(), new_data))
    disable("send_request")
    runjs("document.getElementById('spinner').style.display = 'block';")

    messages <- list()
    if (nzchar(input$sysinstruct)) {
      messages <- append(messages, list(list(role = "system", content = input$sysinstruct)))
    }

    messages <- append(messages, list(list(role = "user", content = input$user_message)))

    body_list <- list(
      model = input$modelselection,
      user_message = input$user_message,
      sysinstruct = input$sysinstruct,
      input_code = input$code_input,
      input_image = input$image_input,
      top_p = input$top_p,
      temp = input$temperature,
      max_token = input$max_tokens,
      api_key = input$api_key

    )

    # Send HTTP request to OpenAI
    response <- client_responses(body_list)

    model_response <- data.frame(source = "Assistant",
                                 message = response,
                                 stringsAsFactors = FALSE)
    chat_data(rbind(chat_data(), model_response))

    enable("send_request")
    updateTextAreaInput(session, "user_message", value = "")
    runjs("document.getElementById('spinner').style.display = 'none';")
    runjs("window.scrollTo(0,document.body.scrollHeight);")

  })

  # Convert image to a data URI
  observeEvent(input$image_input, {
    inFile <- input[["image_input"]]
    if (!is.null(inFile)) {
      img_data <- dataURI(file = inFile$datapath, mime = "image/png")
      chat_data(rbind(chat_data(), data.frame(
        source = "User",
        message = img_data,
        stringsAsFactors = FALSE
      )))
    }
  })

  # Render chat history
  output$chat_history <- renderUI({

    if(nrow(chat_data()) == 0){
      return(div("Your conversation will appear here", style="position: absolute; left: 50%; top: 50%; transform: translate(-50%, -50%);"))
    }

    lapply(1:nrow(chat_data()), function(i) {
      source <- chat_data()[i, "source"]
      message <- chat_data()[i, "message"]

      if (source == "User" && (is.na(message) || trimws(message) == "")) {
        return(NULL)
      }

      formatted_msg <- gsub("(\\b[1-3])\\.", "<br>\\1.", message)

      chat_bubble_class <- ifelse(source == "User", "user-chat", "ai-chat")

      # Check if message looks like an image data URI
      if (grepl("^data:image/", message)) {
        div(
          strong(source), ":",
          br(),
          tags$img(src = HTML(formatted_msg), style = "max-width: 200px; max-height: 200px;"),
          br(),
          class = chat_bubble_class
        )
      } else {
        div(
          strong(source), ": ", HTML(formatted_msg),
          br(),
          class = chat_bubble_class
        )
      }
    })

  })

  # Render hidden tab
  observeEvent(input$show_setting, {
    toggle(id = "settings_panel", anim = TRUE)
  })

}

shinyApp(ui = ui, server = server)







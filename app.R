library(shiny)
library(httr)
library(jsonlite)
library(shinyjs)
library(magrittr)


ui <- fluidPage(
  useShinyjs(),
  tags$head(),
  fluidRow(
    column(
      width = 3,
      div(
        class = "sidebar-panel",
        h3("Alt-Text Generator"),
        tags$hr(style = "margin-top: 10px; margin-bottom: 20px;"),
        textInput("api_key", label = NULL, placeholder = "Enter OpenAI API keys"),

        h5("System Prompt"),
        textAreaInput("sysprompt", NULL, height = "80px", placeholder = "Based on this information what can you learn about the plot. Give me only the overall summary as alt-text")
      )
    ),
    column(
      width = 9,
      div(
        class = "main-chat-panel",
        h3("Chat"),
        uiOutput("chat_history"),
        div(id = "spinner", class = "spinner-border", role = "status"),
        tags$hr(),
        fluidRow(
          column(11, textAreaInput("user_message",label = NULL,  placeholder = "Your R code goes here...", width = "100%")),
          column(1, actionButton("send_message",label = NULL , icon = icon("paper-plane"), class = "button"))
        )
      )
    )
  )

)


server <- function(input, output) {

  chat_data <- reactiveVal(data.frame(source = character(0),
                                      message = character(0),
                                      stringsAsFactors = FALSE))

  observeEvent(input$send_message, {
    req(input$user_message, input$api_key)

    new_data <- data.frame(source = "User",
                           message = input$user_message, stringsAsFactors = FALSE)

    chat_data(rbind(chat_data(), new_data))
    disable("send_message")
    runjs("document.getElementById('spinner').style.display = 'block';")

    messages <- list()
    if (nzchar(input$sysprompt)) {
      messages <- append(messages, list(list(role = "system", content = input$sysprompt)))
    }
    messages <- append(messages, list(list(role = "user", content = input$user_message)))

    body_list <- list(
      model = "gpt-4o",
      messages = messages,
      top_p = 1
    )

    # Process the r code

    # Send HTTP to OpenAI



  })


}

shinyApp(ui = ui, server = server)

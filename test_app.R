library(shiny)

# Dummy function
parse_qmd_file <- function(file_path) {
  data.frame(
    chunk_label = c("plot1", "plot2"),
    code = c("ggplot(data, aes(x, y)) + geom_point()",
             "ggplot(data, aes(x, y)) + geom_line()"),
    alt_text = c("A scatter plot of x vs y",
                 "A line plot showing trend of y over x"),
    stringsAsFactors = FALSE
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .center-content {
        height: 80vh;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        text-align: center;
      }
    "))
  ),
  titlePanel("Auto-generated Alt-Text"),
  uiOutput("user_manual"),
  uiOutput("button_ui"),
  uiOutput("results_ui")

)

server <- function(input, output, session) {

  parsed_data <- reactiveVal(NULL)
  file_uploaded <- reactiveVal(FALSE) # Track if the file is submitted

  # Post-submit button rendering
  output$button_ui <- renderUI({
    if (file_uploaded()) {
      div(
        style = "position: fixed; bottom: 30px; left: 0; right: 0; text-align: center; z-index: 1000;",
        actionButton("show_modal", "Upload Another QMD File")
      )
    }
  })

  output$user_manual <- renderUI({
    if (!file_uploaded()) {
      div(
        class = "center-content",
        p("This app automatically generates alt-text for images to improve accessibility."),
        actionButton("show_modal", "Upload QMD File")
      )
    }
  })


  observeEvent(input$show_modal, {
    showModal(modalDialog(
      h5("OpenAI Api Key "),
      passwordInput("api_key", label = NULL, value = "", placeholder = "Enter OpenAI API keys"),
      h5("Model "),
      selectInput("model_dropdown1", NULL,
                  choices = list(
                    "gpt-4.1" = "gpt-4.1",
                    "gpt-4.1-mini" = "gpt-4.1-mini",
                    "gpt-4.1-nano" = "gpt-4.1-nano"
                  )),
      h5("System Instruction"),
      textInput("text_input", NULL,
                placeholder = "Describe desired model behavior (keept it concise, include the context ... )"),
      h5("Upload a .qmd file"),
      fileInput("file_upload", NULL, accept = ".qmd"),

      footer = tagList(
        actionButton("submit_modal", "Generate Alt-Text"),
        modalButton("Cancel")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$submit_modal, {
    req(input$file_upload)
    removeModal()

    result <- parse_qmd_file(input$file_upload$datapath)
    parsed_data(result)
    file_uploaded(TRUE)
  })


  output$results_ui <- renderUI({
    req(parsed_data())
    results <- parsed_data()

    output_list <- lapply(1:nrow(results), function(i) {
      fluidRow(
        column(6,
               tags$pre(code(results$code[i]))
        ),
        column(6,
               tags$p(strong("Alt-text: "), results$alt_text[i])
        ),
        tags$hr()
      )
    })

    do.call(tagList, output_list)
  })
}

shinyApp(ui, server)




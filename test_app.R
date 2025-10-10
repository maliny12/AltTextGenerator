library(shiny)

# Dummy function to simulate your real parser
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
  titlePanel("Auto-generated Alt-Text Viewer"),

  # Dynamic placement of the button
  uiOutput("button_ui"),

  # Spacer
  br(), br(),

  # Output container for results
  uiOutput("results_ui")
)

server <- function(input, output, session) {

  parsed_data <- reactiveVal(NULL)
  file_uploaded <- reactiveVal(FALSE)  # Track if file has been submitted

  # Initial and post-submit button rendering
  output$button_ui <- renderUI({
    if (file_uploaded()) {
      # After submission: button at the bottom and centered
      div(
        style = "position: fixed; bottom: 30px; left: 0; right: 0; text-align: center; z-index: 1000;",
        actionButton("show_modal", "Upload Another QMD File")
      )
    } else {
      # Before submission: button at top
      div(
        style = "text-align: left;",
        actionButton("show_modal", "Upload QMD File")
      )
    }
  })


  # Show modal on button click
  observeEvent(input$show_modal, {
    showModal(modalDialog(
      title = "Upload and Options",

      selectInput("dropdown", "Choose an option:",
                  choices = c("Option A", "Option B", "Option C")),

      textInput("text_input", "Enter some text:"),

      fileInput("file_upload", "Upload a .qmd file:", accept = ".qmd"),

      actionButton("submit_modal", "Submit"),

      footer = modalButton("Cancel"),
      easyClose = TRUE
    ))
  })

  # When user submits the modal
  observeEvent(input$submit_modal, {
    req(input$file_upload)

    # Close modal
    removeModal()

    # Simulate processing uploaded file
    result <- parse_qmd_file(input$file_upload$datapath)
    parsed_data(result)
    file_uploaded(TRUE)  # Mark as submitted
  })

  # Render results
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




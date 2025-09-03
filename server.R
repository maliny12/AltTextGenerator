

server <- function(input, output, session) {

  runjs("$('#file').parent().parent().siblings().removeClass('form-control').attr('hidden','true');")

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
      input_caption = input$caption,
      top_p = input$top_p,
      temp = input$temperature,
      max_token = input$max_tokens,
      api_key = input$api_key
    )

    # Send HTTP request to OpenAI with Ellmer

    response <- client_responses(body_list)

    model_response <- data.frame(source = "Assistant",
                                 message = response[1],
                                 stringsAsFactors = FALSE)

    model_usage <- data.frame(source = "Usage",
                              message = response[2],
                              stringsAsFactors = FALSE)
    chat_data(rbind(chat_data(), model_response))
    chat_data(rbind(chat_data(), model_usage))

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

  # Delete chat history
  observeEvent(input$clear_history, {
    chat_data(data.frame(source = character(0), message = character(0), stringsAsFactors = FALSE))
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


      chat_bubble_class <- case_when(source == "User" ~ "user-chat",
                                     source == "Assistant" ~ "ai-chat",
                                     source == "Usage" ~ "usage-chat")
      
      label <- if (source == "Usage") {source} else {strong(source)}

      # Check if message looks like an image data URI
      if (grepl("^data:image/", message)) {
        div(
          strong(source), ":",
          br(),
          tags$a(
            href = HTML(formatted_msg),
            `data-lightbox` = "mygallerly",
            tags$img(src = HTML(formatted_msg), style = "max-width: 200px; max-height: 200px;"),
          ),
          br(),
          class = chat_bubble_class
        )
      } else {
        div(
          label, ": ", HTML(formatted_msg),
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

  # Render model description
  output$system_description <- renderUI({

    HTML(paste0("temp:  <span style='color: #2c9373;'>", input$temperature, "</span> &nbsp;  &nbsp;",
                "tokens: <span style='color: #2c9373;'>", input$max_tokens,  "</span> &nbsp;  &nbsp; ",
                "top_p: <span style='color: #2c9373;'>", input$top_p , "</span> &nbsp;  &nbsp; "
    ))
  })

  # Download chat history
  output$download_chat <- downloadHandler(
    filename = function() {"chat_history.txt"},
    content = function(file) {
      chat_history <- paste(chat_data()$source, chat_data()$message, sep = ": ", collapse = "\n")
      writeLines(chat_history, file)
    }
  )

}








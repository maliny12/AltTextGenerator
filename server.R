

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

    rendered_path <- NULL # Initalise rendered image path
    # Evaluate the R code
    if (nchar(input$code_input)) {
      tmp_file <- tempfile(fileext = ".png")
      # Attempt to render the plot
      p <- tryCatch({
        print(input$code_input)
        eval(parse(text = input$code_input))
      }, error = function(x) {
        #showNotification("Error in rendering the plot: the R code is not self contained.", type = "error")
        NULL
      })

      # Only works with `ggplot` object
      if (inherits(p, "ggplot")) {
        ggsave(tmp_file, plot = p)
        rendered_path <- dirname(tmp_file)

        # If the code is self-contained and input image is not provided
        inFile <- input[["image_input"]]
        if (is.null(inFile)) {
          img_data <- dataURI(file = tmp_file, mime = "image/png")
          chat_data(rbind(chat_data(), data.frame(
            source = "User",
            message = img_data,
            stringsAsFactors = FALSE
          )))
        }
      }
    }


    body_list <- list(
      model = input$modelselection,
      user_message = input$user_message,
      sysinstruct = input$sysinstruct,
      input_code = input$code_input,
      rendered_image = rendered_path,
      input_image = input$image_input,
      input_caption = input$caption,
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

  # Render model description
  output$system_description <- renderUI({

    HTML(paste0("temp:  <span style='color: #2c9373;'>", input$temperature, "</span> &nbsp;  &nbsp;",
                "tokens: <span style='color: #2c9373;'>", input$max_tokens,  "</span> &nbsp;  &nbsp; ",
                "top_p: <span style='color: #2c9373;'>", input$top_p , "</span> &nbsp;  &nbsp; "
    ))
  })




}








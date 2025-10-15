
ui <- navbarPage("Alt-Text Generator",
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

        .shiny-input-container:not(.shiny-input-container-inline) {
         width: 100% !important;
        }

        .row {
        margin-right: 0;
        margin-left: 0;
        }

        .form-control {
        height: auto;
        font-family: monospace;
        font-size: 1.2rem;
        }

        .input-group-btn {
        font-family: monospace;
        font-size: 1.2rem;
        }

        #api_key {
        font-family: monospace;
        font-size: 1.2rem;
        }

        #input-group-btn {
        font-size = 1.2rem;
        }

        #system_description {
        color: grey;
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
        height: 22vh !important;
        }

        .tooltips>h5 {
         position: relative;
         display: inline-block;
         cursor: help;
         padding-bottom: 2px;
         border-bottom: 2px solid transparent;

        }

        .selectize-dropdown .selected {
         background-color: #2c9373;
        }

        .tooltiptext {
         visibility: hidden;
         width: fit-content;
         background-color: #555;
         color: #fff;
         text-align: center;
         padding: 5px;
         border-radius: 10px;

         position: absolute;
         z-index: 999;

        }

        .col-sm-2 .tooltiptext {
         margin-left: 10px;
         left: 100%;
         width: 200px;
        }

        .tooltips:hover .tooltiptext {
         visibility: visible;
         opacity: 0.99;
         display: block;
        }

        .tooltips:hover h5 {
         border-bottom: 2px dotted #2c9373;
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

      .btn.btn-default.btn-file{
      font-size: 1.2rem;
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
      max-height: 65vh;
      /*text-align: center;*/
      }

      #chat_history {
      display: flex;
      flex-direction: column;
      margin: 0 5rem;
      min-height: 76vh ;
      }

    .shiny-spinner-output-container {
      background-color: #7575751f;
      border-radius: 10px;
    }

    .form-control {
    background-color:  #7575751f !important;
    border: 1px solid #9b9898bd;
    }

    .selectize-input.full {
    background-color: #d9d9d8 !important;
    }

    .selectize-dropdown {
    background-color: #d9d9d8 !important;
    }


    .form-control:focus {
    border-color: #2c9373 !important;
    }

    .btn-default {
    background-color: #d9d9d8 !important;
    }

    #send_request {
    background-color: #27a786 !important;
    border: transparent !important;
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

    .usage-chat {
      /* margin-right: 20%; */
      padding: 1em;
      border: transparent;
      background-color: transparent;
      border-radius: 10px;
      max-width: 80%;
      width: fit-content;
      align-self: flex-start;
    }

    /* Second Tab */
    .center-content {
        height: 80vh;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        text-align: center;
      }


  ")),
  tags$head(
    tags$script(src = "scripts/index.js"),
    #tags$script(type = "module", src = "scripts/script.js"),
    includeCSS("www/styles/lightbox.min.css"),
    includeCSS("www/styles/lightbox.css"),
    includeScript("www/scripts/lightbox.js"),
    tags$script(src = "https://webr.r-wasm.org/latest/webr.mjs", type = "module")
  ),
  tabPanel("Manual",
             fluidRow(
               column(
                 width = 3,
                 div(
                   class = "control-panel",
                   tags$hr(style = "margin-top: 10px; margin-bottom: 20px;"),
                   fluidRow(
                     tags$div(
                       class = "tooltips",
                       h5("OpenAI Api Key "),
                       tags$span(class = "tooltiptext", "This key is required to generate alt-text.")
                     ),
                     passwordInput("api_key", label = NULL, value = "", placeholder = "Enter OpenAI API keys")
                   ),
                   fluidRow(
                     tags$div(
                       class = "tooltips",
                       h5("Model "),
                       tags$span(class = "tooltiptext", "Choose the OpenAI model to interpret the uploaded image and R code. Different models may produce slightly different outputs.")
                     ),
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
                             tags$div(
                               class = "tooltips",
                               h5("Temperature "),
                               tags$span(class = "tooltiptext", "Controls the randomness of the output. Lower values make the model more focused and deterministic, producing more predictable results. Higher values make the output more creative and diverse. ")
                             ),
                             sliderInput("temperature", label = NULL, min = 0.00, max = 2.00, value = 1.00, step = 0.1),
                             tags$div(
                               class = "tooltips",
                               h5("Max Tokens "),
                               tags$span(class = "tooltiptext", "Specifies the maximum number of tokens allowed in the combined input and output. A token can be as short as one character or as long as one word. This limits the length of the model’s response.")
                             ),
                             sliderInput("max_tokens", label = NULL, min = 1, max = 32000, value = 2048),
                             tags$div(
                               class = "tooltips",
                               h5("Top P "),
                               tags$span(class = "tooltiptext", "Controls the diversity of the output by limiting choices to the top probability mass. Lower values narrow the focus, leading to more deterministic responses.")
                             ),
                             sliderInput("top_p", label = NULL, min = 0.00, max = 1.00, value = 1.00)
                           )
                         )
                       )
                     ),
                     uiOutput("system_description")
                   ),
                   fluidRow(
                     tags$div(
                       class = "tooltips",
                       h5("System Instruction "),
                       tags$span(class = "tooltiptext", HTML(paste0("Optional prompt to guide the model's behavior (e.g., tone, focus). This will be appended to the full prompt sent to the model.", "View the full prompt <a href='https://github.com/maliny12/AltTextGenerator/blob/main/prompt.txt' target='_blank'>here</a>.")))
                     ),
                     textAreaInput("sysinstruct", NULL, height = "80px", placeholder = "Describe desired model behavior (keept it concise, include the context ... )")
                   ),
                   fluidRow(
                     div(style = "display: flex; justify-content: center; align-item: center; gap: 10px; flex-wrap: wrap;",
                         actionButton("clear_history", NULL, icon = icon("trash"), class = "btn-custom", title = "Clear Chat"),
                         downloadButton("download_chat", "Save", class = "btn-custom", title = "Download Chat")
                         #fileInputOnlyButton(label =  "Upload", inputId = "upload_chat")
                         # fileInput("file", label = NULL),
                         # tag$button("Upload File", id = "custom_upload_btn")
                     )
                   ),
                   fluidRow(
                     class = "image-input-row",
                     tags$div(
                       class = "tooltips",
                       h5("Choose Image "),
                       tags$span(class = "tooltiptext", "Upload an image. The app will use the model's vision capabilities to describe its contents.")
                     ),
                     fileInput("image_input", label = NULL, accept ="image/png")
                   ),
                   fluidRow(
                     tags$div(
                       class = "tooltips",
                       h5("Caption"),
                       tags$span(class = "tooltiptext", "Provide the caption to the figure. This will be used to help with the alt-text generation, but its content will not be reused" )
                     ),
                     textAreaInput("caption", NULL, height = "80px", placeholder = "This figure illustrate ... ")
                   ),
                   fluidRow(
                     tags$div(
                       class = "tooltips",
                       h5("R Code "),
                       tags$span(class = "tooltiptext", "Provide the R code used to create the image. It helps the model interpret the plot, and BrailleR is used to generate an accessible graphical summary.")
                     ),
                     textAreaInput("code_input", NULL, height = "80px",
                                   placeholder = "aus_temp |>
  ggplot(aes(
    x_major = long, y_major = lat,
    x_minor = month, ymin_minor = tmin,
    ymax_minor = tmax)) +
  geom_sf(
    data = ozmaps::abs_ste,
    inherit.aes = FALSE) +
  add_glyph_boxes() +
  add_ref_lines() +
  geom_glyph_ribbon()")
                   )
                 )
               ),
               column(
                 width = 9,
                 fluidRow(
                   div(
                     class = "main-chat-panel",
                     #h3("Alt-Text Generator"),
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
            ),
    tabPanel("Automatic",
             uiOutput("user_manual"),
             uiOutput("button_ui"),
             uiOutput("results_ui")
             )

)




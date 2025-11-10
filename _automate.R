generate_alt_text <- function(file_path = NULL,api = NULL, userinstruct = ""){

  if (is.null(file_path)){
    stop("Missing file path")
  }

  if (is.null(api)){
    stop("Missing Open-AI API key. Information on how to obtain an API key can be found here https://help.openai.com/en/collections/3675931-api")
  }

  if (!is.character(api)) {
    stop("API needs to be in a character string.  Information on how to obtain an API key can be found here https://help.openai.com/en/collections/3675931-api")
  }


  content <- extract_ggplot_code(file_path)

  body_list <- list(
    model = "gpt-4.1",
    api_key = api,
    userinstruct = userinstruct,
    max_token = 2048
  )


  result <- client_responses_auto(body_list , content)



  alt_text <- tibble(chunk_label = result$chunk_label,
                     response = result$response,
                     provided_alttext = result$provided_alttext,
                     usage = result$usage,
                     code = result$code,
                     start_line = result$start_line,
                     end_line = result$end_line,
                     newline_style = result$newline_style)


  return(alt_text)
}


client_responses_auto <- function(body_list , content) {



  sysprompt <- "You are a researcher tasked with generating one concise variations of alt-text for graphs based on R code, BrailleR output, reference text, figure caption and alternative text. Your role involves analyzing textual descriptions (such as BrailleR output), statistical summaries, and context from the reference text to produce clear and informative alt-text. If provided, please use the figure caption and alt text as context but do not repeat information. You should naturally describe the chart type, variables on the axes, axis ranges, data mappings (such as color or shape), and any patterns, relationships, or clusters. Include your interpretation of the data where relevant. Do not begin alt-text with phrases like 'Alt-text:' or use labels such as 'Iteration.' If the prompt lacks detail, make reasonable assumptions and note them. Don't provide seperate interpreation for provided R code, reference text or brailleR output."

  chat <- ellmer::chat_openai(
    model = body_list$model,
    api_key = body_list$api_key,
    system_prompt = paste0(body_list$userinstruct,sysprompt)
  )


  output <- data.frame(
    chunk_label = character(0),
    response = character(0),
    provided_alttext = character(0),
    usage = character(0),
    code = character(0),
    start_line = numeric(0),
    end_line = numeric(0),
    newline_style = logical(0)
  )


  for (i in seq_along(content)) {
    if (nzchar(content[[i]]$chunk_code)) {

      client_input <- " "

      # Token limit : 30000 / 7500 char
      # System prompt: 763 char
      tryCatch(
        {

          user_expr <- parse(text = paste(content[[i]]$chunk_code, collapse = "\n"))
          # vi_expression <-  paste0("VI({\n", paste(body_list$input_code, collapse = "\n"), "\n})")
          # brailleR_output <- eval(parse(text = vi_expression))
          plot_obj <- eval(user_expr)

          brailleR_output <- VI(plot_obj)


          if (sum(nchar(brailleR_output$text)) >= body_list$max_token) {
            client_input <- ""
          } else {
            client_input <- brailleR_output$text
          }
        },
        error = function(e){
          class(client_input) <- "error"
        }
      )



      # In case BrailleR throws an error
      if(inherits(client_input, "error") || !nzchar(client_input)) {
        client_input <- paste0("Interpret this code and use the interpreation to generate alt-text", content[[i]]$chunk_code)
      }

    } else {
      client_input <- " "
    }

    client_input <- paste0("BrailleR input: ", client_input, collapse = "")

    # Prepare reference text, fig caption and alt text

    if (!is.null(content[[i]]$reference_paragraph)) {
      reference_text <- paste0("Reference text: ", content[[i]]$reference_paragraph, collapse = "")
    } else {
      reference_text <- ""
    }

    if (!is.na(content[[i]]$alt_text)) {
      alt_text <- paste0("Alt text: ", content[[i]]$alt_text, collapse = "")
    } else {
      alt_text <- ""
    }

    if (!is.na(content[[i]]$fig_cap)) {
      fig_cap <- paste0("Figure caption: ", content[[i]]$fig_cap, collapse = "")
    } else {
      fig_cap <- ""
    }

    # HTTP request
    print(paste0("Evaluating ", content[[i]]$chunk_label, "..."))

    response <- chat$chat(paste0(client_input, reference_text, alt_text, fig_cap))
    usage <- paste0("BrailleR",
                    ", Cummulated cost: ", round(chat$get_cost()[1], 3),
                    ", Cummulated token usage: ", sum(chat$get_tokens()[3])[1]
    )

    output[nrow(output) + 1,] <-  list(chunk_label = content[[i]]$chunk_label,
                                       response = response,
                                       provided_alttext = content[[i]]$alt_text,
                                       usage = usage,
                                       code = content[[i]]$chunk_code,
                                       start_line = content[[i]]$start_line,
                                       end_line = content[[i]]$end_line,
                                       newline_style = content[[i]]$newline_style
                                       )


  }


  return(output)

}


extract_ggplot_code <- function(file_path) {
  # Return: List of a list for each chunk info (chunk label, code, and reference paragraph)


  content <- readLines(file_path)
  temp_file <- tempfile(fileext = ".txt")
  knitr::purl(file_path, output = temp_file, documentation = 1) # no documentation just code boundary


  # Grab all code chunks from the .txt file
  all_code_chunks <-   parse_code(temp_file)

  results <- list()


  # Check if the chunk generate ggplot objects
  for (i in seq_along(all_code_chunks)) {
    chunk <- all_code_chunks[[i]]

    if (stringr::str_detect(chunk$code, "ggplot|geom") & stringr::str_detect(chunk$code, "aes")) {
      # If so, find location of the code chunk in the qmd file and
      original_location <- find_chunk_location(content, chunk$chunk_label, chunk$code)
      # Grab the paragraph that references the chunk label (@fig-)
      reference_paragraph <- find_reference_text(content, chunk$chunk_label,
                                                 original_location$start,
                                                 original_location$end)



      chunk_info <- list(
        chunk_label = chunk$chunk_label,
        alt_text = chunk$alt_text,
        fig_cap = chunk$fig_cap,
        chunk_code = chunk$code,
        reference_paragraph = reference_paragraph,
        start_line = original_location$start,
        end_line = original_location$end,
        newline_style = chunk$newline_style
      )

      results[[length(results) + 1]] <- chunk_info
    }

  }


  unlink(temp_file)
  return(results)

}



parse_code <- function(purled_content) {

  chunks <- list()
  current_chunk <- NULL
  purled_lines <- readLines(purled_content)

  for (i in seq_along(purled_lines)) {
    line <- purled_lines[i]

    chunk_start_pattern <- "^## ----"

    if (stringr::str_detect(line, chunk_start_pattern)){

      if (stringr::str_detect(line, "## ---------------------------------------------------------------------------------------------")) {
        # QMD style with #| label: instead of inline

        #chunk_label <- stringr::str_extract(line, "(?<=#\\| label: ).*")
        #alt_text <- stringr::str_extract(line, "(?<=#\\| fig-alt: ).*")
        #fig_cap <- stringr::str_extract(line, "(?<=#\\| fig-cap: ).*")

        chunk_label <- NA # Will be extracted from code
        alt_text <- NA  # Will be extracted from code
        fig_cap <- NA  # Will be extracted from code
        newline_style <- TRUE

      } else {

        # RMD style
        chunk_label <- stringr::str_extract(line, "(?<=^## ----).*?(?=--|,)") # starts with ## ---- and ends with either , or --
        alt_text <- stringr::str_match(line, 'fig\\.alt="([^"]*)"')[,2]
        fig_cap <- stringr::str_match(line, 'fig\\.cap="([^"]*)"')[,2]
        newline_style <- FALSE

      }

      # Chunk label: either chunk_i or the provided chunk label
      if (is.na(chunk_label)){
        chunk_label <- paste0("chunk_",length(chunks) +1) # chunk_i
      }
      chunk_label <- stringr::str_trim(chunk_label) # Else: chunk_label

      alt_text <- altText_figCap_processing(alt_text, fig_cap)[1]
      fig_cap <- altText_figCap_processing(alt_text, fig_cap)[2]


      if (!is.null(current_chunk)) {
        chunks[[length(chunks) + 1]] <- current_chunk
      }

      # Initialize current chunk
      current_chunk <- list(
        chunk_label = chunk_label,
        alt_text = alt_text,
        fig_cap = fig_cap,
        newline_style = newline_style,
        code = character()
      )
    } else if (!is.null(current_chunk) && !stringr::str_detect(line, "^##")) {
      # Add all codes until the next chunk starts
      current_chunk$code <- c(current_chunk$code, line)
    }

  }

  if (!is.null(current_chunk)) {
    chunks[[length(chunks) + 1]] <- current_chunk
  }

  for (i in seq_along(chunks)) {
    chunks[[i]]$code <- stringr::str_trim(paste0(chunks[[i]]$code, collapse = "\n"))

    # Extract chunk_label, alt-text and fig-cap for QMD style
    if (chunks[[i]]$newline_style) {

      # Chunk_label is either chunk_i or the given chunk_label
      chunk_label <- str_match(chunks[[i]]$code, "#\\|\\s*label:\\s*([^\\n]+)")[, 2]
      if (is.na(chunk_label)) {
        chunk_label <- paste0("chunk_", i) # chunk_i
      }

      alt_text <- stringr::str_match(chunks[[i]]$code, "#\\|\\s*fig-alt:\\s*([^\\n]+)")[, 2]
      fig_cap <- stringr::str_match(chunks[[i]]$code, "#\\|\\s*fig-cap:\\s*([^\\n]+)")[, 2]

      # Remove chunk-label, fig.cap and alt-text from the code (everything that starts with #| and ends with \n)
      chunks[[i]]$code <- stringr::str_remove_all(chunks[[i]]$code, regex("^#\\|.*\\n", multiline = TRUE))


      # ------------
      chunks[[i]]$chunk_label <- chunk_label
      chunks[[i]]$alt_text <- altText_figCap_processing(alt_text, fig_cap)[1]
      chunks[[i]]$fig_cap <- altText_figCap_processing(alt_text, fig_cap)[2]

    }

  }


  return(chunks)

}


find_chunk_location <- function(content, chunk_label, code) {
  # content: readLines(".qmd")
  # chunk_label: a single label name
  # code: all syntax in a single code chunk
  # Return: Start and end position of each code chunk. If it is an empty chunk, start and end position are set to NA.


  start_pattern <- paste0("^```\\{r.*", chunk_label, "|", "^#\\| label: ", chunk_label)
  potential_chunk_start <- which(stringr::str_detect(content, start_pattern))


  # if there is no chunk label
  if (length(potential_chunk_start) == 0) {
    all_syntax <- stringr::str_split(code, "\n")[[1]]
    if (length(all_syntax) > 0){
      first_line <- stringr::str_trim(all_syntax[1])

      # Iterate through the entire document and find the first line
      if (nchar(first_line) > 0) {
        for (i in seq_along(content)){
          if (stringr::str_detect(stringr::str_trim(content[i]), stringr::fixed(first_line))) { #fixed() to compare literal bytes

            # chunk_start is where "^{r" is
            chunk_start <- i
            while (chunk_start > 1 && !stringr::str_detect(content[chunk_start - 1], "^```\\{r")) {
              chunk_start <- chunk_start - 1
            }
            chunk_start <- chunk_start - 1

            # chunk_end is where "^```" is
            chunk_end <- i
            while (chunk_end > 1 && !stringr::str_detect(content[chunk_end - 1], "^```$")) {
              chunk_end <- chunk_end + 1
            }
            chunk_end <- chunk_end + 1

            return(list(start = chunk_start, end = chunk_end))
          }
        }
      }
    }
    # if there is no syntax in the code chunk
    return(list(start = NA, end = NA))
  }

  # if there is a chunk label
  start_line <- potential_chunk_start[1] - 1
  chunk_ends <- which(stringr::str_detect(content, "^```$"))
  end_line <- chunk_ends[chunk_ends > start_line][1] # grab the first one

  if (is.na(end_line)) {
    end_line <- length(content)
  }

  return(list(start = start_line, end = end_line))

}


find_reference_text <- function(content, chunk_label, chunk_start, chunk_end) {
  # content: readLines(".qmd")
  # chunk_label: a single label name
  # chunk_start, chunk_end: line where the boundary of the code begins/end. Output from find_chunk_location()
  # Return: Start and end position of each code chunk. If it is an empty chunk, start and end position are set to NA.


  paragraph <- NULL

  reference_pattern <- c(
    paste0("@fig-", chunk_label),
    paste0("@fig:", chunk_label),
    paste0("\\{#fig", chunk_label, "\\}"),
    paste0("Figure.*", chunk_label),
    paste0("figure.*", chunk_label)
    # "figure.*above", "figure.*below",
    # "plot.*above","plot.*below",
    # "following.*figure", "following.*plot",
    # "graph.*above", "graph.*below"
  )


  for (i in seq_along(content)) {


    if (i >= chunk_start && i <= chunk_end) next # Skip code chunks

    line <- content[i]

    for (p in reference_pattern) {


      if (stringr::str_detect(line, p)) {

        # Not all paragraphs are written in one long line. So a paragraph is defined by white space at both end.
        paragraph <- extract_paragraph(content, i)
        break
      }
    }

  }

  return(paragraph)

}


extract_paragraph <- function(content, line_number) {

  start_line <- line_number
  end_line <- line_number

  # go backwards to find paragraph start: stop when we hit white space
  while (start_line > 1 &&
         !stringr::str_detect(content[start_line - 1], "^\\s*$") &&
         !stringr::str_detect(content[start_line - 1], "^#+\\s")) {
    start_line <- start_line - 1
  }

  # go forwards to find paragraph end: stop when we hit white space or beginning of a new code chunk
  while (end_line < length(content) &&
         !stringr::str_detect(content[end_line + 1], "^\\s*$") &&
         !stringr::str_detect(content[end_line + 1], "^#+\\s") &&
         !stringr::str_detect(content[end_line + 1], "^```")) {
    end_line <- end_line + 1
  }

  paragraph <- paste(content[start_line:end_line], collapse = " ")
  paragraph <- stringr::str_trim(stringr::str_squish(paragraph))

  return(paragraph)
}


altText_figCap_processing <- function(alt_text, fig_cap) {
  # Alt text: either NA or the given alt-text (minimum length is 40 chars long)
  if (is.na(alt_text) || nchar(alt_text) < 40){
    alt_text <- NA
  }
  alt_text <- stringr::str_trim(alt_text) # Else: the given alt-text

  # Fig caption: either NA or the given fig caption (minimum length is 40 chars long)
  if (is.na(fig_cap) || nchar(fig_cap) < 40) {
    fig_cap <- NA
  }
  fig_cap <- stringr::str_trim(fig_cap) # Else: the given fig.cap

  return(list(alt_text, fig_cap))
}


insert_altText <- function(file_path, result, name_to) {

  lines <- readLines(file_path)
  adjust_position <- 0

  for (i in seq_len(nrow(result))) {

    start <- result$start_line[i] + adjust_position
    end <- result$end_line[i] + adjust_position
    alt_text <- result$response[i]
    provided_alttext <- result$provided_alttext[i]
    newline_style <- result$newline_style[i]


    # Always favor the suggest alt-text (since the provided alt-text is used to generate the suggest alt-text)
    if (newline_style) {

      alt_line <- grep("^#\\|\\s*fig-alt:", lines[start:end])
      if (length(alt_line) > 0) {
        # Update existing alt-text
        lines[start + alt_line - 1] <- paste0("#| fig-alt: ", alt_text)
      } else {
        lines <- append(lines, paste0("#| fig-alt: ", alt_text), after = start)
        # Since we are appending a new line, we need to adjust the start/end position of the new chunk by 1
        adjust_position <- adjust_position + 1
      }
    }  else {
      # Inline style
      header <- lines[start + 1] # Start position does not contain the inline for some reason
      if (grepl("fig\\.alt\\s*=", header)) {
        # Update existing fig.alt
        header <- sub('fig\\.alt\\s*=\\s*"[^"]*"', paste0('fig.alt="', alt_text, '"'), header)
      } else {
        # Insert fig.alt before closing }
        header <- sub("\\}$", paste0(', fig.alt="', alt_text, '"}'), header)
      }
      lines[start + 1] <- header

    }
  }

  writeLines(lines, name_to)

}











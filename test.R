

extract_ggplot <- function(file_path) {

  # Read the QMD file in as a temporary file
  temp_file <- tempfile(fileext = ".txt")
  knitr::purl(file_path, output = temp_file)
  purled_content <- read_lines(temp_file)

  chunk_info <- parse_purled_content(purled_content)


}

parse_purled_content <- function(purled_content) {
  # Parse purled content
  chunks <- list()
  current_chunk <- NULL

  for (i in seq_along(purled_content)) {
    line <- purled_content[i]

    if (str_detect(line, "^## ----")) {
      if(!is.null(current_chunk)) {
        chunks[[length(chunks) + 1]] <- current_chunk
      }

      # Will return NA when there is no chunk label
      chunk_label <- str_trim(str_extract(line, "(?<=## ----)[^-]+?(?=,|$)"))
      if (is.na(chunk_label)) {
        # Manually label the chunk as chunk_i
        chunk_label <- paste0("chunk_", length(chunks) + 1)
      }

      current_chunk <- list(
        chunk_label = chunk_label,
        code = character()
      )

    } else if (!is.null(current_chunk) && !str_detect(line, "^##")) {
      # add code to the current chunk (without comments)
      current_chunk$code <- c(current_chunks$code, line)
    }

  }

  if (is.null(current_chunk)) {
    chunks[[length(chunk) + 1]] <- current_chunk
  }


  # convert code vectors into string
  for (i in seq_along(chunks)) {
    chunks[[i]]$code <- paste(chunks[[i]]$code, collapse = "\n")
    chunks[[i]]$code <- str_trim(chunks[[i]]$code)
  }

  return(chunks)

}



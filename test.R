library(tidyverse)
library(BrailleR)
library(ellmer)

x = rnorm(1000)



brailleR <- capture.output(VI(hist(rnorm(1000))))
writeLines(brailleR,"breilleR_output.txt" )

a <-readLines("breilleR_output.txt")


chat <- chat_openai()
chat$chat(paste0("based on this information what can you learn about the plot. Give me only the overall summary as alt-text",brailleR) )

tryCatch({
  output_text <- capture.output({
    result <- eval(parse(text = input$user_message))
    print(result)
  })

})

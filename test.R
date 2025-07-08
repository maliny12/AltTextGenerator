library(tidyverse)
library(BrailleR)
library(ellmer)
library(ggplot2)
library(gridExtra)
library(palmerpenguins)

sysprompt <- "You are a researcher looking to write alt text for your plot. Based on the available information, what you can learn from the plot? State any relationship, clusters or any interesting thing about the data.Do not include the phrase 'Alt text:' at the beginning. Be concise and sound natural"

# Simple data ------------------

x = rnorm(1000)

example1 <- capture.output(VI(hist(x)))
# writeLines(brailleR,"breilleR_output.txt" )
# a <-readLines("breilleR_output.txt")


chat <- chat_openai()
chat$chat(paste0(sysprompt,example1))

# Grid plot ---------------------


example2 <- capture.output(
  VI(
    ggplot(penguins, aes(bill_depth_mm, bill_length_mm, color = species)) +
    geom_point() +
    labs(
      title = "Bill length and depth",
      subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
      x = "Bill depth in mm", y = "Bill length in mm",
      color = "species",
      caption = "Source: Palmer Station LTER / palmerpenguins package"
    ) +
    scale_color_viridis_d()
  )
)

chat$chat(paste0(sysprompt,example2),
          content_image_file("example2.png"))



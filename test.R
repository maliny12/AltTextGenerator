library(tidyverse)
library(BrailleR)
library(ellmer)
library(ggplot2)
library(gridExtra)
library(palmerpenguins)

sysprompt <- "based on this information what can you learn about the plot. Give me only the overall summary as alt-text"

# Simple data ------------------

x = rnorm(1000)

example1 <- capture.output(VI(hist(x)))
# writeLines(brailleR,"breilleR_output.txt" )
# a <-readLines("breilleR_output.txt")


chat <- chat_openai()
chat$chat(paste0(sysprompt,example1))

# Grid plot ---------------------


capture.output(
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

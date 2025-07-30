library(tidyverse)
library(BrailleR)
library(ellmer)
library(ggplot2)
library(gridExtra)
library(palmerpenguins)
library(usethis)


# Set up ---------------------------------
sysprompt <- readLines("prompt.txt")
readRenviron(".Renviron")
chat <- chat_openai(api_key = Sys.getenv("OPENAI_API_KEY"))

# All examples are from Dataviz- Claus Wilke
# Link: https://github.com/clauswilke/dataviz
#

x = rnorm(1000)

example1 <- capture.output(VI(hist(x)))
#writeLines(example1,"breilleR_example1.txt" )

readRenviron(".Renviron")
chat <- chat_openai(api_key = Sys.getenv("OPENAI_API_KEY"))
chat$chat(paste0(sysprompt,example1))




# Example 2----------------------------------

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
#writeLines(example2,"breilleR_example2.txt" )

chat$chat(paste0(sysprompt,example2),
          content_image_file("files/example2.png"))


# --------------------------------------------

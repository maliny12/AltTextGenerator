library(tidyverse)
library(BrailleR)
library(ellmer)
library(ggplot2)
library(gridExtra)
library(palmerpenguins)
library(usethis)

sysprompt <- readLines("prompt.txt")

# Example 1 ---------------------------------

x = rnorm(1000)

example1 <- capture.output(VI(hist(x)))
#writeLines(example1,"breilleR_example1.txt" )

usethis::edit_r_environ()
chat <- chat_openai(api_key = OPENAI_API_KEY)
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
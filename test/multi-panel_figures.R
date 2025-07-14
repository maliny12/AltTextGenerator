
# All examples are from Dataviz- Claus Wilke
# Source code is the same as the original, with some modifications
# Link: https://github.com/clauswilke/dataviz

library(forcats)
library(stringr)
library(ggridges)
library(tidyverse)
library(ggplot2movies)
library(scales)

# Run set up file
source("_common.R")

# Example 1----------------------------------

ex1 <- as.data.frame(Titanic) %>% mutate(surv = ifelse(Survived == "No", "died", "survived")) %>%
  ggplot(aes(x= Sex, y = Freq, fill = Sex)) + geom_col() +
  facet_grid(Class ~ surv, scales = "free_x") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(limits = c(0, 195), expand = c(0, 0)) +
  scale_fill_manual(values = c("#D55E00D0", "#0072B2D0"), guide = "none") +
  theme(
    axis.line = element_blank(),
    axis.ticks.length = grid::unit(0, "pt"),
    axis.ticks = element_blank(),
    axis.text.x = element_text(margin = margin(7, 0, 0, 0)),
    strip.text = element_text(margin = margin(3.5, 3.5, 3.5, 3.5)),
    strip.background  = element_rect(
      fill = "grey85", colour = "grey85",
      linetype = 1, linewidth = 0.25
    ),
    panel.border = element_rect(
      colour = "grey85", fill = NA, linetype = 1,
      linewidth = 1.)
  )

client_responses(VI(ex1), "files/ex1.png")

# --------------------------------------------

ex2 <- ggplot(filter(movies, year > 1905), aes(y = rating, x = votes)) +
  geom_point(color = "#0072B250", size = 0.1) +
  geom_smooth(
    method = 'lm', se = FALSE, size = 1.25, color = '#D55E00',
    fullrange = TRUE
  ) +
  scale_x_log10(labels = label_log(base = 10), name = "number of votes", breaks = c(10, 1000, 100000)) +
  scale_y_continuous(
    limits = c(0, 10), expand = c(0, 0),
    breaks = c(0, 5, 10), name = "average rating"
  ) +
  facet_wrap(~year, ncol = 10) +
  theme(
    axis.title = element_text(size = 14),
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0, "pt"),
    strip.text = element_text(margin = margin(3.5, 3.5, 3.5, 3.5)),
    panel.border = element_rect(
      colour = "grey80", fill = NA, linetype = 1, size = 1.
    ),
    plot.margin = margin(3, 5, 3, 1.5)
  )

#ggsave("files/ex2.png")

client_responses(VI(ex2), "files/ex2.png")




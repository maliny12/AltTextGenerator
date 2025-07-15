
# All examples are from Dataviz- Claus Wilke
# Source code is the same as the original, with some modifications
# Link: https://github.com/clauswilke/dataviz

library(forcats)
library(stringr)
library(ggridges)
library(tidyverse)
library(ggplot2movies)
library(scales)
library(cowplot)

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


# Issue: input from BreilleR is too large
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


# ------------------------------------------

male_sport <- unique(filter(Aus_athletes, sex=="m")$sport)
female_sport <- unique(filter(Aus_athletes, sex=="f")$sport)
both_sport <- male_sport[male_sport %in% female_sport]
athletes_df <- filter(Aus_athletes, sport %in% both_sport) %>%
  mutate(
    sport = case_when(
      sport == "track (400m)" ~ "track",
      sport == "track (sprint)" ~ "track",
      TRUE ~ sport
    ),
    sex = factor(sex, levels = c("f", "m"))
  )

p1 <- ggplot(athletes_df, aes(x = sex)) +
  geom_bar(fill = "#56B4E9E0") +
  scale_y_continuous(limits = c(0, 95), expand = c(0, 0), name = "number") +
  scale_x_discrete(name = NULL, labels = c("female", "male")) +
  theme(
    axis.ticks.x = element_blank(),
    #axis.ticks.length = grid::unit(0, "pt"),
    plot.margin = margin(3, 0, 0, 0)
  )

p2 <- ggplot(athletes_df, aes(x = rcc, y = wcc, shape = sex, color = sex, fill = sex)) +
  geom_point(size = 2.5) +
  scale_x_continuous(limits = c(3.8, 6.75), name = NULL) +
  scale_y_continuous(limits = c(2.2, 11.), expand = c(0, 0), name = "WBC count") +
  scale_shape_manual(
    values = c(21, 22),
    labels = c("female   ", "male"), name = NULL,
    guide = guide_legend(direction = "horizontal")
  ) +
  scale_color_manual(
    values = c("#CC79A7", "#56B4E9"), name = NULL,
    labels = c("female   ", "male"),
    guide = guide_legend(direction = "horizontal")
  ) +
  scale_fill_manual(
    values = c("#CC79A780", "#56B4E980"), name = NULL,
    labels = c("female   ", "male"),
    guide = guide_legend(direction = "horizontal")
  ) +
  theme(
    legend.position = c(1, .1),
    legend.justification = "right",
    legend.box.background = element_rect(fill = "white", color = "white"),
    plot.margin = margin(3, 0, 0, 0)
  )

p_row <- plot_grid(
  p1, NULL, p2,
  labels = c("a", "", "b"),
  align = 'h',
  nrow = 1,
  rel_widths = c(0.7, 0.02, 1)
) +
  draw_text(
    "RBC count", x = 1, y = 0.01, size = 12, hjust = 1, vjust = 0
  )

p3 <- ggplot(
  athletes_df,
  aes(
    x = sport, y = pcBfat, color = fct_relevel(sex, "m"),
    fill = fct_relevel(sex, "m")
  )
) +
  geom_boxplot(width = 0.5) +
  scale_color_manual(
    values = c("#009E73", "#56B4E9"), name = NULL,
    labels = c("male", "female")
  ) +
  scale_fill_manual(
    values = c("#009E7340", "#56B4E940"), name = NULL,
    labels = c("male", "female")
  ) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "% body fat") +
  theme(
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()
    #axis.ticks.length = grid::unit(0, "pt")
  )

ex3 <- plot_grid(
    p_row, NULL, p3,
    ncol = 1,
    rel_heights = c(1, .04, 1),
    labels = c("", "", "c")
  ) +
    theme(plot.margin = margin(6, 6, 3, 1.5))

client_responses(VI(ex3), "files/ex3.png")




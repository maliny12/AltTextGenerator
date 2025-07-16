
# All examples are from Dataviz- Claus Wilke
# Source code is the same as the original, with some modifications
# Link: https://github.com/clauswilke/dataviz

library(forcats)
library(stringr)
library(ggridges)
library(tidyverse)
library(ggforce)
library(treemapify)

# Run set up file
source("_common.R")

# Set up data ----------------------------------

bridge_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/bridges/bridges.data.version2"
bridge_col_names <- c("ID", "River", "Location", "Erected", "Purpose", "Length",
                      "Lanes", "Clear", "T-or-D", "Material", "Span", "Rel-L", "Type")

bridges <- read.csv(bridge_url, header = FALSE, sep = ",", col.names = bridge_col_names)

select(bridges, Material, Erected) %>%
  table() %>%
  reshape2::melt() %>%
  rename(material = Material, erected = Erected, count = value) %>%
  mutate(
    material = case_when(
      material == "IRON" ~ "iron",
      material == "STEEL" ~ "steel",
      material == "WOOD" ~ "wood"
    ),
    erected = case_when(
      erected == "CRAFTS" ~ "crafts",
      erected == "EMERGING" ~ "emerging",
      erected == "MATURE" ~ "mature",
      erected == "MODERN" ~ "modern"
    )
  ) %>%
  group_by(erected) %>%
  mutate(group_count = sum(count)) -> bridges_tidy


n_total <- sum(bridges_tidy$count)

bridges_erected <- filter(bridges_tidy, erected %in% c("crafts", "modern")) %>%
  group_by(erected) %>%
  summarize(
    count = sum(count),
    percent = round(100*count/n_total, 1)
  ) %>%
  rename(type = erected)

bridges_material <- group_by(bridges_tidy, material) %>%
  summarize(
    count = sum(count),
    percent = round(100*count/n_total, 1)
  ) %>%
  rename(type = material)

bridges_material_erected <- rbind(bridges_material, bridges_erected) %>%
  mutate(
    type = factor(type, levels = c("steel", "wood", "iron", "modern", "crafts"))
  ) %>%
  arrange(type)

bridges_pie <- bridges_material_erected %>%
  mutate(
    count_total = sum(count),
    end_angle = 2*pi*cumsum(count)/count_total,   # ending angle for each pie slice
    start_angle = lag(end_angle, default = 0),   # starting angle for each pie slice
    mid_angle = 0.5*(start_angle + end_angle),   # middle of each pie slice, for the text label
    hjust = ifelse(mid_angle>pi, 1, 0),
    vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1)
  )

rpie = 1
rlabel = 1.05 * rpie

# Pie chart ------------------------------------------------------

ex4 <- ggplot(bridges_pie) +
  geom_arc_bar(
    aes(
      x0 = 0, y0 = 0, r0 = 0, r = rpie,
      start = start_angle, end = end_angle, fill = type
    ),
    color = "white", size = 0.5
  ) +
  geom_text(
    aes(
      x = rlabel*sin(mid_angle),
      y = rlabel*cos(mid_angle),
      label = type,
      hjust = hjust, vjust = vjust
    ),
    size = 14/.pt
  ) +
  geom_text(
    aes(
      x = 0.6*sin(mid_angle),
      y = 0.6*cos(mid_angle),
      label = paste0(percent, "%")
    ),
    size = 12/.pt,
    color = c("white", "white", "white", "black", "black", "black")
  ) +
  coord_fixed(clip = "off") +
  scale_x_continuous(
    limits = c(-1.5, 1.5), expand = c(0, 0), name = "", breaks = NULL, labels = NULL
  ) +
  scale_y_continuous(
    limits = c(-1.15, 1.15), expand = c(0, 0), name = "", breaks = NULL, labels = NULL
  ) +
  scale_fill_manual(
    values = c(iron = "#D55E00D0", wood = "#009E73D0", steel = "#0072B2D0",
               crafts = "#F0E442D0", modern = "#56B4E9D0")
  ) +
  theme(legend.position = "none")

# ggsave("files/ex4.png")

client_responses(VI(ex4), "files/ex4.png")


# Mosiac plot ------------------------------------------------

labels_df <- group_by(bridges_tidy, erected) %>%
  filter(count != 0) %>%
  arrange(desc(material)) %>%
  mutate(
    y = (cumsum(count) - 0.5*count)/group_count,
    y = ifelse(
      erected == "mature" & material == "wood", NA, y
    )
  )

ex5 <-  ggplot(bridges_tidy) +
  aes(x = erected, y = count, width = group_count, fill = material) +
  geom_bar(stat = "identity", position = "fill", colour = "white", size = 1) +
  geom_text(
    data = labels_df,
    aes(y = y, label = count, color = material),
    na.rm = TRUE,
    size = 12/.pt
  ) +
  facet_grid(~erected, scales = "free_x", space = "free_x") +
  scale_y_continuous(
    name = NULL,
    #breaks = NULL,
    expand = c(0, 0),
    breaks = filter(labels_df, erected == "crafts")$y,
    labels = filter(labels_df, erected == "crafts")$material,
    sec.axis = dup_axis(
      breaks = filter(labels_df, erected == "modern")$y,
      labels = filter(labels_df, erected == "modern")$material
    )
  ) +
  scale_x_discrete(
    name = NULL
  ) +
  scale_fill_manual(
    values = c("#D55E00D0", "#0072B2D0", "#009E73D0"),
    guide = "none"
  ) +
  scale_color_manual(
    values = c(iron = "white", wood = "white", steel = "white"),
    guide = "none"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    line = element_blank(),
    strip.text = element_blank(),
    axis.ticks.length = unit(0, "pt"),
    panel.spacing.x = unit(0, "pt")
  )

# ggsave("files/ex5.png")


client_responses(VI(ex5), "files/ex5.png")


# Parallel sets --------------------------------------



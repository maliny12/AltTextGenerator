# All examples are from Dataviz- Claus Wilke
# Source code is the same as the original, with some modifications
# Link: https://github.com/clauswilke/dataviz

library(forcats)
library(stringr)
library(ggridges)
library(tidyverse)
library(lubridate)
library(nycflights13)
library(colorspace)
library(Stat2Data)
library(ggrepel)

# Run set up file
source("_common.R")
# Simple but overlap ----------------------------------------------
ex8 <- ggplot(mpg, aes(y = cty, x = displ, color = drv, fill = drv)) +
  geom_point(size = 3, shape = 21) +
  ylab("fuel economy (mpg)") +
  xlab("displacement (l)") +
  scale_color_manual(values=c("#202020", "#E69F00", "#56B4E9"),
                     name="drive train",
                     breaks=c("f", "r", "4"),
                     labels=c("FWD", "RWD", "4WD")) +
  scale_fill_manual(values=c("#20202080", "#E69F0080", "#56B4E980"),
                    name="drive train",
                    breaks=c("f", "r", "4"),
                    labels=c("FWD", "RWD", "4WD")) +
  theme(legend.position = c(.7, .8),
        plot.margin = margin(3, 7, 3, 1.5))

ggsave("files/ex8.png")

client_responses(VI(ex8), "files/ex8.png")

# 2D bins ------------------------------------------

breaks_x <- c("0:00", "6:00", "12:00", "18:00", "24:00")

p_flights_base <- ggplot(flights, aes(dep_time*35,dep_delay)) +
  geom_abline(slope = 0, intercept = 0, color="grey70") +
  scale_x_time(
    name = "departure time",
    breaks = hm(breaks_x),
    labels = breaks_x
  ) +
  scale_y_continuous(
    name = "departure delay (minutes)"
  ) +
  theme(plot.margin = margin(3, 7, 3, 1.5))

p_flights_scatter <- p_flights_base + geom_point(alpha = 0.1, stroke = 0, size = 2)

ex9 <- p_flights_base +
  geom_bin2d(bins=50) +
  #scale_fill_continuous_sequential(palette = "Blue-Yellow", l2 = 90, c2 = 20) +
  scale_fill_continuous_sequential(
    h1 = -83, h2 = 20, c1 = 30, cmax = 40, c2 = 0, l1 = 20, l2 = 94, p1 = 1, p2 = 1.2,
    rev = TRUE,
    begin = 0.2,
    name = "departures"
  ) +
  theme(legend.position = c(0.85, .85))

client_responses(capture.output(VI(ex9), "files/ex9.png"))


# Contour lines ---------------------------------


ex10 <- ggplot(diamonds, aes(carat, price)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", color = darken("#0072B2", .2), size = .3, binwidth = 0.8) +
  #geom_density2d(color = darken("#0072B2", .2), size = .3, binwidth = 0.8) +
  scale_fill_gradient(low = desaturate(lighten("#0072B2", .9), .6), high = desaturate(lighten("#0072B2", .6), .6), guide = "none") +
  scale_x_continuous(
    limits = c(-1, 5.1)
  ) +
  scale_y_log10(
    name = "price (USD)",
    breaks = c(300, 1000, 3000, 10000),
    labels = c("$300", "$1,000", "$3,000", "$10,000")
  ) +
  coord_cartesian(xlim = c(-.1, 2.3), ylim = c(200, 25000), expand = FALSE) +
  facet_wrap(~cut, scales = "free_x", labeller = labeller(cut = tolower)) +
  panel_border() +
  theme(
    legend.title = element_text(hjust = 0.5),
    legend.position = c(.95, .05),
    legend.justification = c(1, 0),
    axis.ticks.length = unit(0, "pt")
  )

client_responses(capture.output(VI(ex10)), "files/ex10.png")


# Redundance coding ---------------------------------------------

iris_dens <- group_by(iris, Species) %>%
  do(ggplot2:::compute_density(.$Sepal.Length, NULL)) %>%
  rename(Sepal.Length = x)

# compute densities for sepal lengths
iris_dens2 <- group_by(iris, Species) %>%
  do(ggplot2:::compute_density(.$Sepal.Width, NULL)) %>%
  rename(Sepal.Width = x)

dens_limit <- max(iris_dens$density, iris_dens2$density) * 1.05 # upper limit of density curves


iris_max <- filter(iris_dens, density == max(density)) %>%
  ungroup() %>%
  mutate(
    hjust = c(0, 0.4, 0),
    vjust = c(1, 0, 1),
    nudge_x = c(0.11, 0, 0.24),
    nudge_y = c(-0.02, 0.02, -0.02),
    label = paste0("Iris ", Species)
  )
# we need different hjust and nudge values here
iris_max <-
  iris_max %>%
  mutate(
    hjust = c(1, 0.4, 0),
    vjust = c(1, 0, 1),
    nudge_x = c(-0.18, 0, 0.47),
    nudge_y = c(-0.01, 0.06, 0.03),
    label = paste0("Iris ", Species)
  )


breaks = c("setosa", "virginica", "versicolor")
labels = paste0("Iris ", breaks)

iris_scatter2_base <- ggplot(
  iris, aes(x = Sepal.Length, y = Sepal.Width, shape = Species, fill = Species, color = Species)
) +
  scale_shape_manual(
    values = c(21, 22, 23),
    breaks = breaks,
    labels = labels,
    name = NULL
  ) +
  scale_color_manual(
    values = darken(c("#56B4E9", "#E69F00", "#009E73"), 0.3),
    breaks = breaks,
    labels = labels,
    name = NULL
  ) +
  scale_fill_manual(
    values = c("#56B4E980", "#E69F0080", "#009E7380"),
    breaks = breaks,
    labels = labels,
    name = NULL
  ) +
  scale_x_continuous(
    limits = c(3.95, 8.2), expand = c(0, 0),
    labels = c("4.0", "5.0", "6.0", "7.0", "8.0"),
    name = "sepal length"
  ) +
  scale_y_continuous(
    limits = c(1.9, 4.6), expand = c(0, 0),
    name = "sepal width"
  )

iris_scatter2 <- iris_scatter2_base +
  geom_point(
    size=2.5, stroke = 0.5,
    position = position_jitter(
      width = 0.01 * diff(range(iris$Sepal.Length)),
      height = 0.01 * diff(range(iris$Sepal.Width)),
      seed = 3942)
  ) +
  theme(
    legend.title.align = 0.5,
    legend.text = element_text(face = "italic"),
    legend.spacing.y = unit(3.5, "pt"),
    plot.margin = margin(7, 7, 3, 1.5)
  )


xdens <- axis_canvas(iris_scatter2, axis = "x") +
  geom_density_line(
    data=iris_dens,
    aes(x = Sepal.Length, y = density, fill = Species, color = Species),
    stat = "identity", size = .2
  ) +
  geom_text(
    data = iris_max,
    aes(
      label = label, hjust = hjust, vjust = vjust, color = Species,
      x = Sepal.Length + nudge_x,
      y = density + nudge_y
    ),
    size = 12/.pt,
    #color = "black", inherit.aes = FALSE,
    fontface = "italic"
  ) +
  scale_color_manual(
    values = darken(c("#56B4E9", "#E69F00", "#009E73"), 0.3),
    breaks = c("virginica", "versicolor", "setosa"),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c("#56B4E950", "#E69F0050", "#009E7350"),
    breaks = c("virginica", "versicolor", "setosa"),
    guide = "none"
  ) +
  scale_y_continuous(limits = c(0, dens_limit), expand = c(0, 0))

ydens <- axis_canvas(iris_scatter2, axis = "y", coord_flip = TRUE) +
  geom_density_line(
    data = iris_dens2,
    aes(x = Sepal.Width, y = density, fill = Species, color = Species),
    stat = "identity", size = .2
  )  +
  scale_color_manual(
    values = darken(c("#56B4E9", "#E69F00", "#009E73"), 0.3),
    breaks = c("virginica", "versicolor", "setosa"),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c("#56B4E950", "#E69F0050", "#009E7350"),
    breaks = c("virginica", "versicolor", "setosa"),
    guide = "none"
  ) +
  scale_y_continuous(limits = c(0, dens_limit), expand = c(0, 0)) +
  coord_flip()

p1 <- insert_xaxis_grob(
  iris_scatter2 + theme(legend.position = "none"),
  xdens,
  grid::unit(3*14, "pt"), position = "top"
)
p2 <- insert_yaxis_grob(p1, ydens, grid::unit(3*14, "pt"), position = "right")


ex11 <- ggdraw(p2)

client_responses(capture.output(VI(ex11)), "files/ex11.png")






# All examples are from Dataviz- Claus Wilke
# Source code is the same as the original, with some modifications
# Link: https://github.com/clauswilke/dataviz


library(forcats)
library(stringr)
library(ggridges)
library(tidyverse)
library(plot3D)
library(cowplot)
library(tinter)

# Run set up file
source("_common.R")

# Multi plane 3D scatter plot ------------------------------------------------


colors <- c("#0072B2", "#CC79A7", "#E69F00")

cyls <- data.frame(cyl = factor(c(4, 6, 8)))

p <- ggplot(cyls, aes(cyl, cyl, color = cyl)) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = colors,
    name = "cylinders ",
    labels = c("4  ", "6  ", "8")
  ) +
  #theme_dviz_open(font_size = 14, rel_small = 1, font_family = dviz_font_family) + # nolint
  theme(
    legend.position = "top",
    legend.justification = "right",
    legend.key.width = unit(4, "pt")
  )
legend <- get_legend(p)

pfun <- function(theta = 30, phi = 20) {
  function() {
    par(
      xpd = NA,
      bg = "transparent",
      mai = c(0, 0.1, 0, 0)
      #family = dviz_font_family_condensed # nolint
    )
    scatter3D(
      mtcars$disp, mtcars$hp, mtcars$mpg, colvar = mtcars$cyl,
      col = colors,
      pch = 19, bty ="b2", theta = theta, phi = phi, colkey = FALSE, # nolint
      xlab = "displacement (cu. in.)",
      ylab ="power (hp)", # nolint
      zlab = "efficiency (mpg)",
      cex.lab = 1.17
    )
  }
}

ex6 <- plot_grid(pfun(30, 20), pfun(-30, 20),
          NULL, legend, # nolint
          pfun(30, 40), pfun(-30, 40),
          rel_heights = c(1, 0.1, 1), ncol = 2,
          labels = c("a", "b", "", "", "c", "d"),
          label_fontface = "plain")


client_responses(VI(ex6), "files/ex6.png")

# Protein 3D ------------------------------------------------

# Make legend via ggplot2
df <- data.frame(x = 1:10,
                 fill = runif(10))

p <- ggplot(df, aes(x, y = 1, fill = fill)) + geom_tile() +
  scale_fill_gradient2(low = darken("#A6522B", .07), mid = darken("#FFFF00", .05), # nolint # nolint
                       high = darken("#FFFFFF", .02),
                       midpoint = .5,
                       limits = c(0, 1),
                       breaks = c(0, 1),
                       labels = c("highly\nconserved", "highly\nvariable"),
                       name = "sequence conservation",
                       guide = guide_colorbar(direction = "horizontal",
                                              label.position = "bottom",
                                              title.position = "top",
                                              ticks = FALSE,
                                              barwidth = grid::unit(3.5, "in"),
                                              barheight = grid::unit(0.2, "in"))) + # nolint
  theme(legend.title.align = 0.5,
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.justification = "center")

legend <- get_legend(p)

ex7 <- plot_grid(ggdraw() +  draw_image("files/1AKO-cropped.png"),
         legend, ncol = 1, rel_heights = c(16, 4)) # nolint # nolint

client_responses(VI(ex7), "files/ex7.png")




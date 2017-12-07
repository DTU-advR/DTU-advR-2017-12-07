# share a legend
# https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs

library(dplyr)
library(ggplot2)
librry(grid)
library(gridExtra)

set.seed(1410)
dat <- diamonds[sample(nrow(diamonds), 900), ]
p1 <- ggplot(dat, aes(x = clarity, y = carat, fill = cut)) +
  geom_boxplot(color = "black")

p2 <- ggplot(dat, aes(x = clarity, fill = cut)) +
  geom_bar(color = "black", position = "dodge")
p1
p2

grid.arrange(p1, p2, nrow = 1)
# more on that: https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html

grid.arrange(p1 + guides(fill = FALSE), p2, nrow = 1)

grid.arrange(p1 + guides(fill = FALSE), p2 + theme(legend.position = "bottom"), nrow = 1)

get_legend <- function(gg_plot) {
  grob_table <- ggplotGrob(gg_plot)
  # gtable from gtable package
  # names(grob_table[["grobs"]]) does not work!
  grob_table[["grobs"]][[which(sapply(grob_table[["grobs"]], function(x) x[["name"]]) == "guide-box")]]
}

fill_legend <- get_legend(p2)
# grid.draw(fill_legend)

grid.arrange(p1 + guides(fill = FALSE), 
             p2 + guides(fill = FALSE), 
             fill_legend, 
             nrow = 1)

grid.arrange(p1 + guides(fill = FALSE), 
             p2 + guides(fill = FALSE), 
             fill_legend, 
             nrow = 1, widths = c(0.4, 0.4, 0.2))

lm <- rbind(c(1, 2),
            c(3, 3))

fill_legendh <- get_legend(p2 + theme(legend.position = "bottom"))

grob_list <- list(grobTree(ggplotGrob(p1 + guides(fill = FALSE))), 
                  grobTree(ggplotGrob(p2 + guides(fill = FALSE))), 
                  grobTree(fill_legendh))

grid.arrange(grobs = grob_list, layout_matrix = lm, nrow = 2, 
             heights = c(0.9, 0.1))

lm2 <- rbind(c(1, 2, 3, 4),
             c(5, 5, 5, 5))

grob_list2 <- list(grobTree(textGrob("A", y = unit(0.9, "npc"))),
                   grobTree(ggplotGrob(p1 + guides(fill = FALSE))), 
                   grobTree(textGrob("B", y = unit(0.9, "npc"))),
                   grobTree(ggplotGrob(p2 + guides(fill = FALSE))), 
                   grobTree(fill_legendh))

grid.arrange(grobs = grob_list2, layout_matrix = lm2, nrow = 2, 
             heights = c(0.9, 0.1), widths = c(0.05, 0.45, 0.05, 0.45))

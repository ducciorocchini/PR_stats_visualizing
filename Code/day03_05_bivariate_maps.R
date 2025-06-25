# https://cran.r-project.org/web/packages/biscale/vignettes/biscale.html

# install.packages("biscale")
# install.packages(c("cowplot", "sf"))

library(biscale)
library(ggplot2)
library(cowplot)  # for the ggdraw function

# create classes
data <- bi_class(stl_race_income, x = pctWhite, y = medInc, style = "quantile", dim = 3)

# create map
map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(
    title = "Income in St. Louis, MO",
    subtitle = "Gray Pink (GrPink) Palette"
  ) +
  bi_theme()

# legend
legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Higher Education possibilities",
                    ylab = "Higher Income ",
                    size = 8)

# combine map with legend
ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.2, .65, 0.2, 0.2)

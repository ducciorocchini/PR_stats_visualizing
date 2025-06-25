# Code for multitemporal analysis

library(terra)
library(imageRy)
library(ggplot2)
library(patchwork)
library(viridis)
library(RStoolbox)
# install.packages("colorist")
library(colorist)

im.list()

EN_01 <- im.import("EN_01.png")
EN_01 <- flip(EN_01)
EN_13 <- im.import("EN_13.png")
EN_13 <- flip(EN_13)

p1 <- ggRGB(EN_01, 1, 2, 3)
p2 <- ggRGB(EN_13, 1, 2, 3)

p1 + p2

difEN <- EN_01[[1]] - EN_13[[1]]
plot(difEN)

# Greenland ice melt
gr <- im.import("greenland")
difgr <- gr[[1]] - gr[[4]]
plot(difgr, col=mako(100))

im.plotRGB(gr, 1, 2, 4)

# colorist package for overlapping colors

data("fiespa_occ")

# First map with single distributions
m1 <- metrics_pull(fiespa_occ)
p1 <- palette_timecycle(fiespa_occ)
map_multiples(m1, p1, ncol=4, labels=names(fiespa_occ))

# Common map
m1_distill <- metrics_distill(fiespa_occ)
map_single(m1_distill, p1)

# Ridgeline plots
im.ridgeline(gr, scale=1)
im.ridgeline(gr, scale=2)
im.ridgeline(gr, scale=2, palette="inferno")

# NDVI phehnology
ndvi <- im.import("Sentinel2_NDVI_2020")
im.plotRGB(ndvi, 1, 2, 3)

im.ridgeline(ndvi, scale=2)

names(ndvi) <- c("02_Feb", "05_May", "08_Aug", "11_Nov")
im.ridgeline(ndvi, scale=1)
im.ridgeline(ndvi, scale=2)
im.ridgeline(ndvi, scale=2, palette="magma")


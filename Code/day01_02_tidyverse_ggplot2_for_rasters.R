# Code for plotting satellite data with ggplot2

library(terra)
library(imageRy)
library(RStoolbox)
library(patchwork)

# Import the data
im.list()

mato1992 <- im.import("matogrosso_l5_1992219_lrg.jpg")
mato2006 <- im.import("matogrosso_ast_2006209_lrg.jpg")

# Single layers
p1992 <- im.ggplot(mato1992[[1]])
p2006 <- im.ggplot(mato2006[[1]])

p1992 + p2006

# ggplot2 for RGB space: the ggRGB function
# 1 NIR
# 2 red
# green

# from RStoolbox
p1 <- ggRGB(mato1992, r=1, g=2, b=3)
p2 <- ggRGB(mato2006, r=1, g=2, b=3)

p1 + p2

p3 <- ggRGB(mato1992, r=2, g=1, b=3)
p4 <- ggRGB(mato2006, r=2, g=1, b=3)
p5 <- ggRGB(mato1992, r=2, g=3, b=1)
p6 <- ggRGB(mato2006, r=2, g=3, b=1)

(p1 | p2) /
(p3 | p4) /
(p5 | p6) 

p1 + p2 + p3 + p4 + p5 + p6

# Stretching
ggRGB(mato2006, r=1, g=2, b=3)
p7 <- ggRGB(mato2006, r=1, g=2, b=3, stretch="lin")

p2 + p7

p8 <- ggRGB(mato2006, r=1, g=2, b=3, stretch="hist")

p2 + p7 + p8

p9 <- ggRGB(mato2006, r=1, g=2, b=3, stretch="sqrt")

p2 + p7 + p9

p10 <- ggRGB(mato2006, r=1, g=2, b=3, stretch="log")

# ggRGB() will plot RGB layers instead of single bands
# The argument stretch is used to stretch images. The lin stretch is the same of that of im.ploRGB from imageRy

p1 <- ggRGB(mato1992, r=1, g=2, b=3, stretch="lin")
p2 <- ggRGB(mato2006, r=1, g=2, b=3, stretch="lin")
p3 <- ggRGB(mato1992, r=2, g=1, b=3, stretch="lin")
p4 <- ggRGB(mato2006, r=2, g=1, b=3, stretch="lin")
p5 <- ggRGB(mato1992, r=2, g=3, b=1, stretch="lin")
p6 <- ggRGB(mato2006, r=2, g=3, b=1, stretch="lin")

p1 + p2 + p3 + p4 + p5 + p6

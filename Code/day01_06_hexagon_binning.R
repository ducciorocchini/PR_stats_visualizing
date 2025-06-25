# Plotting data in scatterplots with hexagon binning
library(terra)
library(imageRy)
# install.packages("hexbin")
library(hexbin)
library(ggplot2)
library(viridis)

im.list()

sentdol <- im.import("sentinel.dolomites")

plot(sentdol[[1]], sentdol[[2]])

# NDVI example
ndvi <- im.import("_NDVI_20")
plot(ndvi[[2]], ndvi[[3]], xlim=c(-0.4,0.8), ylim=c(-0.4,0.8), xlab="May", ylab="August")
# y=bx+a
# y=x
# intercept a = 0
# slope b = 1
abline(0, 1, col="red")

par(mfrow=c(1,3))
plot(ndvi[[2]])
plot(ndvi[[3]])
plot(ndvi[[2]], ndvi[[3]], xlim=c(-0.4,0.8), ylim=c(-0.4,0.8), xlab="May", ylab="August")
abline(0, 1, col="red")

library(hexbin)
may <- as.data.frame(ndvi[[2]])
aug <- as.data.frame(ndvi[[3]])

hb <- hexbin(may[[1]], aug[[1]], xbins=40)
plot(hb)

hb <- hexbin(may[[1]], aug[[1]], xbins=100)
plot(hb)

hb <- hexbin(may[[1]], aug[[1]], xbins=10)
plot(hb)

# ggplot2
ggplot(data.frame(x = rnorm(10000), y=rnorm(10000)), aes(x=x, y=y)) +
  geom_hex() + coord_fixed()

ggplot(data.frame(x=may[[1]], y=aug[[1]]), aes(x=x, y=y)) + 
  geom_hex() +
  scale_fill_viridis()

setwd("~/Desktop")
pdf("hex.pdf")
ggplot(data.frame(x=may[[1]], y=aug[[1]]), aes(x=x, y=y)) + 
  geom_hex() +
  scale_fill_viridis()
dev.off()

setwd("~/Desktop")
jpeg("hex.jpg")
ggplot(data.frame(x=may[[1]], y=aug[[1]]), aes(x=x, y=y)) + 
  geom_hex() +
  scale_fill_viridis()
dev.off()

ggplot(data.frame(x=may[[1]], y=aug[[1]]), aes(x=x, y=y)) + 
  geom_hex() +
  scale_fill_viridis(option="mako")


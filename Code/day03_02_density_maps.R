# Code related to population ecology

# A package is needed for point pattern analysis
# install.packages("spatstat")
library(spatstat)
library(viridis)

# let's use the bei data:
# data description:
# https://CRAN.R-project.org/package=spatstat

bei

# plotting the data
plot(bei)

# changing dimension - cex
plot(bei, cex=.2)

# changing the symbol - pch
plot(bei, cex=.2, pch=19)

# additional datasets
bei.extra
plot(bei.extra)
plot(bei.extra, col=viridis(100))

# let's use only part of the dataset: elev
plot(bei.extra[[1]], col=viridis(100))
elevation <- bei.extra[[1]]
plot(elevation, col=viridis(100))

# passing from points to a countinuous surface
densitymap <- density(bei)
plot(densitymap, col=magma(100))
points(bei, cex=.2, col="yellow")

# multiframe
par(mfrow=c(1,3))
plot(bei)
plot(densitymap)
plot(elevation)














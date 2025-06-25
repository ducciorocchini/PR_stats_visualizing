# https://jtr13.github.io/spring19/Community_Contribution_Group16.html
# change with ecological data from vegan

library(gclus)
library(GGally)

data("Seatbelts")
Seatbelts.df <- as.data.frame(Seatbelts)

# Part 1: Plotting the pure scatterplot matrix
# pairs from Base R
pairs(Seatbelts)

# Coloring points by different level of variable "law"
pairs(Seatbelts.df[,1:7], col=ifelse(Seatbelts.df$law==0, "black", "red"))

# changing shape of scatterplots
pairs(Seatbelts.df[,1:7],pch=ifelse(Seatbelts.df$law==0, 1,3), upper.panel = NULL)

## Loading required package: cluster
sb.cor <- cor(Seatbelts.df)
# assign color
sb.color <- dmat.color(sb.cor)
# assign order
sb.o <- order.hclust(sb.cor)
cpairs(Seatbelts, order= sb.o,panel.color= sb.color,upper.panel=NULL)

# Part 2: The GG family
iris$Long.Petal<-as.factor(ifelse(iris$Petal.Length>median(iris$Petal.Length), "long", "short"))

ggpairs(iris)

# function ggcorr in "ggplot2"
library(ggplot2)
ggcorr(iris)



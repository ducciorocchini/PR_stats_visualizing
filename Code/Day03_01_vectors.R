# Code for scatterplot matrices with vector (or raster) data

library(terra)
library(sf)
# https://www.youtube.com/watch?v=2d8YaVu1uzs
library(ggplot2)
library(viridis)
library(patchwork)
library(GGally)
library(imageRy)

nc <- st_read(system.file("shape/nc.shp", package="sf"))

plot(nc)

ggplot() +
    geom_sf(data = nc) +
    ggtitle("Counties of NC")

nc$areakm2 <- st_area(nc) / 1000000

# Order and then take the first 10

nc <- nc[order(nc$areakm2, decreasing = TRUE), ]

ggplot() + 
    geom_sf(data = nc[1:10, ]) +
    ggtitle("Counties with highest area")

nc <- nc[order(nc$areakm2, decreasing = FALSE), ]

ggplot() + 
    geom_sf(data = nc[1:10, ]) +
    ggtitle("Counties with smallest area")

p1 = ggplot(nc) +
  geom_sf(aes(fill = AREA), color="white") +
  scale_fill_viridis_c(option = "cividis", direction = -1) 

p2 = ggplot(nc) +
  geom_sf(aes(fill = AREA), color="white") +
  scale_fill_viridis_c(option = "magma") 

p1 / p2

p3 = ggplot(nc) +
  geom_sf(aes(fill = PERIMETER), color="white") +
  scale_fill_viridis_c(option = "viridis", direction = -1) 

p1 / p3

nc$COMPLEXITY = nc$PERIMETER / nc$AREA

p4 = ggplot(nc) +
    geom_sf(aes(fill = COMPLEXITY), color="white") +
    scale_fill_viridis_c(option = "magma", direction = -1) 
  
p1 / p3 / p4

p5 = ggplot(nc) +
  geom_sf(aes(fill = PERIMETER), color="white") +
  scale_fill_viridis_c(option = "viridis") 

p6 = ggplot(nc) +
    geom_sf(aes(fill = COMPLEXITY), color="white") +
    scale_fill_viridis_c(option = "viridis") 

p2 / p5 / p6

nc$COMPLEXITYlog = log(nc$COMPLEXITY)

p7 = ggplot(nc) +
    geom_sf(aes(fill = COMPLEXITYlog), color="white") +
    scale_fill_viridis_c(option = "viridis") 

p2 / p5 / p6 / p7

nc$COMPLEXITYsqrt = sqrt(nc$COMPLEXITY)

p8 = ggplot(nc) +
    geom_sf(aes(fill = COMPLEXITYsqrt), color="white") +
    scale_fill_viridis_c(option = "viridis") 

p6 / p7 / p8

pdf("~/Desktop/complex.pdf")
ggplot(nc) +
    geom_sf(aes(fill = COMPLEXITYsqrt), color="white") +
    scale_fill_viridis_c(option = "viridis") +
    labs(
        title = "North Carolina Counties",
        subtitle = "Complexity of counties",
        fill = "Perimeter / Area"
       ) 
dev.off()

nc_df = st_drop_geometry(nc)
names(nc_df)

summary(nc_df)

head(nc_df)

# Select columns
cols = c("AREA", "PERIMETER", "COMPLEXITY", "COMPLEXITYlog")
nc_df_subset = nc_df[, cols]

ns = nc_df_subset

ggplot(ns, aes(x = AREA)) +
  geom_histogram(
    aes(y = ..density.., fill = ..count..),
    bins = 20,
    color = "light blue"
  ) 

p9 = ggplot(ns, aes(x = AREA)) +
  geom_histogram(
    aes(y = ..density.., fill = ..count..),
    bins = 20,
    color = "black"
  ) +
  geom_density(color="dark blue", size=1.2)  +
  scale_fill_viridis_c(option = "viridis") +
  labs(
        fill = "AREA"
       ) 

p10 = ggplot(ns, aes(x = PERIMETER)) +
  geom_histogram(
    aes(y = ..density.., fill = ..count..),
    bins = 20,
    color = "black"
  ) +
  geom_density(color="dark blue", size=1.2)  +
  scale_fill_viridis_c(option = "viridis") +
  labs(
        fill = "PERIMETER"
       ) 


p11 = ggplot(ns, aes(x = COMPLEXITY)) +
  geom_histogram(
    aes(y = ..density.., fill = ..count..),
    bins = 20,
    color = "black"
  ) +
  geom_density(color="dark blue", size=1.2)  +
  scale_fill_viridis_c(option = "viridis") +
  labs(
        fill = "COMPLEXITY"
       ) 

p12 = ggplot(ns, aes(x = COMPLEXITYlog)) +
  geom_histogram(
    aes(y = ..density.., fill = ..count..),
    bins = 20,
    color = "black"
  ) +
  geom_density(color="dark blue", size=1.2)  +
  scale_fill_viridis_c(option = "viridis") +
  labs(
        fill = "COMPLEXITYlog"
       ) 

pairs(ns)

p9 / p10 / p11 / p12

ggpairs(ns)

# Plotting area and perimeter per county as a factor

ggplot(nc_df, aes(x = PERIMETER, y = AREA, color = as.factor(CNTY_))) +
  geom_point(size = 4) +
  scale_color_viridis_d(option = "viridis") 


## Ratsreizing vector data

# Convert to SpatVector (terra format)
nc_vect = vect(nc)

# Create an empty raster template based on the extent of the vecto
r_template = rast(nc_vect, resolution = 0.1)  # Adjust resolution as needed

# Rasterize a specific attribute (e.g., AREA)
r_area = rasterize(nc_vect, r_template, field = "AREA")

plot(r_area, col=viridis(100))

im.ridgeline(r_area, scale=1)




# rocio: https://github.com/b-cubed-eu/biospace25-demo/blob/main/README_Rocio.md
# paper: https://www.ecography.org/sites/ecography.org/files/appendix/ecog-01388.pdf
# https://borisleroy.com/virtualspecies_tutorial/index.html

library(geodata)

# Import data
worldclim <- worldclim_global(var = "bio", res = 10,
                              path = tempdir())
# wait for it, it might require some time.

# Rename bioclimatic variables for simplicity
names(worldclim) <- paste0("bio", 1:19)

# Virtual species
library(virtualspecies)

my.parameters <- formatFunctions(bio1 = c(fun = 'dnorm', mean = 25, sd = 5),
                                 bio12 = c(fun = 'dnorm', mean = 4000,
                                 sd = 2000))

my.first.species <- generateSpFromFun(raster.stack = worldclim[[c("bio1", "bio12")]],
                                      parameters = my.parameters,
                                      plot = TRUE)

my.first.species

## Virtual species generated from 2 variables:
##  bio1, bio12
## 
## - Approach used: Responses to each variable
## - Response functions:
##    .bio1  [min=-54.72435; max=30.98764] : dnorm   (mean=25; sd=5)
##    .bio12  [min=0; max=11191] : dnorm   (mean=4000; sd=2000)
## - Each response function was rescaled between 0 and 1
## - Environmental suitability formula = bio1 * bio12
## - Environmental suitability was rescaled between 0 and 1


# changing parameters

my.parameters <- formatFunctions(bio1 = c(fun = 'dnorm', mean = 25, sd = 5),
                                 bio12 = c(fun = 'dnorm', mean = 4000, sd = 2000))

# Generation of the virtual species
new.species <- generateSpFromFun(raster.stack = worldclim[[c("bio1", "bio12")]],
                                 parameters = my.parameters,
                                 formula = "3.1 * bio1^2 - 1.4 * sqrt(bio12) * bio1",
                                 plot = TRUE)

# Additional species
linear.function <- function(x, a, b)
{
  a*x + b
}

my.responses <- formatFunctions(bio1 = c(fun = "linear.function", a = 1, b = 0),
                                bio12 = c(fun = "dnorm", mean = 1000, sd = 1000))

generateSpFromFun(raster.stack = worldclim[[c("bio1", "bio12")]],
                  parameters = my.responses, plot = TRUE)


my.responses <- formatFunctions(bio1 = c(fun = "linear.function", a = 1, b = 0),
                                bio12 = c(fun = "dnorm", mean = 1000, sd = 1000))

generateSpFromFun(raster.stack = worldclim[[c("bio1", "bio12")]],
                  parameters = my.responses, plot = TRUE)


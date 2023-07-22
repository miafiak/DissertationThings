setwd("~/dissertation/DissertationThings")
library(terra)
library(mclust)
library(sf)
#library(tidyverse)
library(cowplot)
library(spocc)
library(rgdal)
library(fpc)
library(maptools)
library(mapview)
#library(spatialEco)
library(biclust)
#library(SpatialPack)
library(ggmap)
library(kohonen)
library(terra)
library(rassta)
#library("GeoInterpolation")
#additional comment to commit to GitHub


#using the terra package to load in the raster datasets
eleva.tif <- rast("Data/Elevation.tif")
pop.tif <- rast("Data/ken_general_2020.tif")

lc.forest <- rast("Data/fractional_cover_lc100/AllForestTypes.tif")
lc.bare <- rast("Data/fractional_cover_lc100/Bare-SparseVegetation.tif")
lc.crops <- rast("Data/fractional_cover_lc100/Cropland.tif")
lc.herb.veg <- rast("Data/fractional_cover_lc100/HerbaceousVegetation.tif")
lc.herb.wet <- rast("Data/fractional_cover_lc100/HerbaceousWetland.tif")
lc.shrubs <- rast("Data/fractional_cover_lc100/Shrubs.tif")
lc.urban <- rast("Data/fractional_cover_lc100/UrbanBuiltUp.tif")
lc.water <- rast("Data/fractional_cover_lc100/Water.tif")

#Stack all landcovers into one:
lc.stack <- c(lc.forest, lc.bare, lc.crops, lc.herb.veg, lc.herb.wet, lc.shrubs, lc.urban, lc.water)

#read in the rectangle
rectangle <-st_read("Data/rectangle.shp")

#crop to extent of the rectangle (to test out the code so far):
eleva.rec.tif <- terra::crop(eleva.tif, rectangle)
pop.rec.tif <- crop(pop.tif, rectangle)

#Create new NA data, assuming that the population in the areas with No Data is 0 (bold assumption but otherwise hard)
new_value <- 0 
pop.rec.tif[is.na(pop.rec.tif)] <- new_value
resamp.pop <- resample(pop.rec.tif, eleva.rec.tif)

#normalize population and elevation here
popnx <- minmax(resamp.pop)
norm.pop <- (resamp.pop- popnx [1,]) / (popnx [2,] - popnx [1,])

elnx <- minmax(eleva.rec.tif)
norm.ele <- (eleva.rec.tif - elnx [1,])/ (elnx[2,] - elnx [1,])
#crop the stack to the extent of the rectangle:

lc.rec <- terra::crop(lc.stack, rectangle)




#resample data so I can combine them (this leads to aggregation issues!)
#resamp.pop<- resample(norm.pop, eleva.rec.tif)
resamp.lc <- resample(lc.rec, eleva.rec.tif)

#omit NAs by creating a inbetween thing of them
foc.ele <- focal(norm.ele)

#Renaming the layer so it becomes identifiable again
#names(foc.ele) <- ("elevation")

#Trying to do some stratification before so I can use it in the SOM
strat.units <- strata(cu.rast = resamp.lc)

stack1 <- (c(resamp.pop, resamp.lc))
som_grid <- somgrid(xdim = 6, ydim= 6, topo = "hexagonal")
#supersom with Rassta pray for me
set.seed(999)
#som1 <- som(stack1, grid = somgrid(5,5, topo = "hexagonal"))
gapsom <- som_gap(stack1, xdim = 5, ydim = 5, rlen = 100, K.max = 20)

pamsom <- som_pam(ref.rast = stack1, kohsom = gapsom$SOM, k = gapsom$Kopt)

if(interactive()){plot(pamsom$sompam.rast)}




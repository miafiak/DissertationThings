setwd("M:/dissertation/DissertationThings")
library(terra)
library(mclust)
library(sf)
library(tidyverse)
library(cowplot)
library(spocc)
library(rgdal)
library(fpc)
library(maptools)
library(mapview)
#library(spatialEco)ss
library(biclust)
#library(SpatialPack)
library(ggmap)
library(kohonen)
library(terra)
library(rassta)

#using the terra package to load in the raster datasets
eleva.tif <- rast("data/elevation.tif")
pop.tif <- rast("data/ken_general_2020.tif")

#Different Landcover Binary Values
lc.bare.tif <- rast("data/fractional_cover_lc100/Bare-SparseVegetation.tif")
lc.forst <- rast("data/fractional_cover_lc100/AllForestTypes.tif")
lc.crop <- rast("data/fractional_cover_lc100/Cropland.tif")
lc.urban <- rast("data/fractional_cover_lc100/UrbanBuiltUp.tif")
lc.water <- rast("data/fractional_cover_lc100/Water.tif")
lc.shrubs <- rast("data/fractional_cover_lc100/Shrubs.tif")
lc.herb.veg <- rast("data/fractional_cover_lc100/HerbaceousVegetation.tif")
lc.herb.wet <- rast("data/fractional_cover_lc100/HerbaceousWetland.tif")

#Combine the datasets to a stack with different layers
lc.stack <- (c(lc.bare.tif, lc.forst, lc.crop, lc.urban, lc.water, lc.shrubs, lc.herb.veg, lc.herb.wet))

#read in the rectangle
rectangle <-st_read("data/rectangle.shp")

#crop to extent of the rectangle (to test out the code so far):
eleva.rec.tif <- terra::crop(eleva.tif, rectangle)
pop.rec.tif <- crop(pop.tif, rectangle)

#Crop the stack to the extent of the rectangle
lc.rec.stack <- crop(lc.stack, rectangle)

#Create new NA data (assuming the population in unknown is 0)
new_value <- 0 
pop.rec.tif[is.na(pop.rec.tif)] <- new_value

#resample data so I can combine them (this leads to aggregation issues!) CHECK AGAIN WHETHER THAT IS NEEDED
resamp.pop<- resample(pop.rec.tif, eleva.rec.tif)
resamp.lc <- resample(lc.rec.tif, eleva.rec.tif)
resamp.lc.bare <- resample(lc.bare.rec.tif, eleva.rec.tif)

#omit NAs (Also check whether still needed, except for elevation)
#foc.lc <- focal(resamp.lc)
#foc.pop<- focal(resamp.pop)
#foc.bare.lc <- focal(resamp.lc.bare)
foc.ele <- focal(eleva.rec.tif)

#library(raster)
stack1 <- (c(foc.ele, pop.rec.tif, lc.rec.stack))

#rename the coloumns so that I can identify them, need to check how to rename them again, and where it went wrong
#names(stack1) <- c("elevation", "population", "bare", "landcover")

#supersom with Rassta pray for me
set.seed(9)
gapsom <- som_gap(stack1, K.max = 25)


pamsom <- som_pam(ref.rast = stack1, kohsom = gapsom$SOM, k = gapsom$Kopt)

if(interactive()){plot(pamsom$sompam.rast)}


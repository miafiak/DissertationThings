setwd("M:/dissertation/RAllTries")
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
#library(spatialEco)
library(biclust)
#library(SpatialPack)
library(ggmap)
library(kohonen)
library(terra)
library(rassta)
#additional comment to commit to GitHub


#using the terra package to load in the raster datasets
eleva.tif <- rast("data/elevation.tif")
pop.tif <- rast("data/ken_general_2020.tif")
lc.tif <- rast("data/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif")
lc.bare.tif <- rast("data/fractional_cover_lc100/PROBAV_LC100_global_v3.0.1_2019-nrt_Bare-CoverFraction-layer_EPSG-4326.tif")

#Create new NA data

#read in the rectangle
rectangle <-st_read("data/rectangle.shp")

#crop to extent of the rectangle (to test out the code so far):
eleva.rec.tif <- terra::crop(eleva.tif, rectangle)
#eleva.rec.tif <- mask(eleva.tif2, rectangle)

pop.rec.tif <- crop(pop.tif, rectangle)
#pop.rec.tif <- mask(pop.tif2, rectangle)

lc.rec.tif <- crop(lc.tif, rectangle)
lc.bare.rec.tif <- crop(lc.bare.tif, rectangle)

new_value <- 999 
pop.rec.tif[is.na(pop.rec.tif)] <- new_value
lc.rec.tif[is.na(lc.rec.tif)] <- new_value
lc.bare.rec.tif[is.na(lc.bare.rec.tif)] <- new_value

#resample data so I can combine them (this leads to aggregation issues!)
resamp.pop<- resample(pop.rec.tif, eleva.rec.tif)
resamp.lc <- resample(lc.rec.tif, eleva.rec.tif)
resamp.lc.bare <- resample(lc.bare.rec.tif, eleva.rec.tif)

#omit NAs
foc.lc <- focal(resamp.lc)
foc.pop<- focal(resamp.pop)
foc.bare.lc <- focal(resamp.lc.bare)
foc.ele <- focal(eleva.rec.tif)

#library(raster)
stack1 <- (c(foc.ele, foc.pop, foc.bare.lc, foc.lc))
#rename the coloumns so that I can identify them
names(stack1) <- c("elevation", "population", "bare", "landcover")

#supersom with Rassta pray for me
set.seed(9)
gapsom <- som_gap(stack1, K.max = 25)


pamsom <- som_pam(ref.rast = stack1, kohsom = gapsom$SOM, k = gapsom$Kopt)

if(interactive()){plot(pamsom$sompam.rast)}


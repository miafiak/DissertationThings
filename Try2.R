setwd("M:/dissertation/RAllTries")
library(raster)
library(sf)
library(tidyverse)
library(cowplot)
library(spocc)
library(rgdal)
library(fpc)
library(maptools)
library(mapview)
library(spatialEco)
library(biclust)
library(terra)
#library(SpatialPack)

#using the terra package to load in the raster datasets
eleva.tif <- raster("data/elevation.tif")
pop.tif <- raster("data/ken_general_2020.tif")
lc.tif <- raster("data/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif")
lc.bare.tif <- raster("data/fractional_cover_lc100/PROBAV_LC100_global_v3.0.1_2019-nrt_Bare-CoverFraction-layer_EPSG-4326.tif")

#read in the rectangle
rectangle <-st_read("data/rectangle.shp")

#crop to extent of the rectangle (to test out the code so far):
eleva.tif2 <- crop(eleva.tif, extent(rectangle))
eleva.rec.tif <- mask(eleva.tif2, rectangle)

pop.tif2 <- crop(pop.tif, extent(rectangle))
pop.rec.tif <- mask(pop.tif2, rectangle)

lc.rec.tif <- crop(lc.tif, extent(eleva.rec.tif))
lc.bare.rec.tif <- crop(lc.bare.tif, extent(eleva.rec.tif))

#resample data so I can combine them (this leads to aggregation issues!)
resampled<- resample(pop.rec.tif, eleva.rec.tif)

stack1 <- stack(resampled, eleva.rec.tif)

k-clust <- kmeans(getValues(stack1), centers = getValues(random.raster()))

#create matrix
#resamp.mtrx <- getValues(resampled)
#add all of them together into one to create a cluster

#k-clust <- kmeans(resamp.mtrx)

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
#library(osmar)
library(osmdata)

#using the raster package to load in the raster datasets
eleva.tif <- raster("data/elevation.tif")
pop.tif <- raster("data/ken_general_2020.tif")
lc.tif <- raster("data/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif")
lc.bare.tif <- raster("data/fractional_cover_lc100/PROBAV_LC100_global_v3.0.1_2019-nrt_Bare-CoverFraction-layer_EPSG-4326.tif")

#read in shp files (for now only extent of Garissa)
GarissaOut <- st_read("data/Garissa_Outline_ken_admn_bound.shp")

#crop the rasters to Garissa outlines
eleva.tif2 <- crop(eleva.tif, extent(GarissaOut))
eleva.Gar.tif <- mask(eleva.tif2, GarissaOut)

pop.tif2 <- crop(pop.tif, extent(GarissaOut))
pop.Gar.tif <- mask(pop.tif2, GarissaOut)

#assign 0s if value is NA
eleva.Gar.tif[is.na(eleva.Gar.tif)] <- 0 #mean(eleva.tif)

#plot the data on a basemap and visualize it
#If I maybe want to see a better resolution, add: ', maxpixels= Inf'

#mapview(eleva.tif) + mapview(pop.tif) + mapview(eleva.Gar.tif) + mapview(pop.Gar.tif) + mapview(lc.tif)
mapview(lc.bare.tif)

#create matrices for elevation and population
eleva.mtrx <- as.matrix(eleva.Gar.tif2)

#The package used for the clustering:
library(mclust)

#eleva.clstr <- Mclust(eleva.mtrx)
#mapview(eleva.clstr)






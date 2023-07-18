setwd("~/dissertation/DissertationThings")
library(beepr)
library(terra)
library(kohonen)
library(ggplot2)
#library(rgdal)
library(gridExtra)
library(grid)
library(viridis)
library(dplyr)
library(sf)

#read in of landcover
lc.gen <- rast("Data/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif")

#read in the AEZs so that they can be used:
CoasLow.shp <- st_read("Data/AEZ/CoastalLowland.shp")
AllAEZ.shp <- st_read("Data/AEZ/Kenya_Agro-Ecological_Zones_Data.shp")

#rasterize it:
AEZ.tif <- rasterize(CoasLow.shp, lc.gen, "AEZ_Code", background=0)

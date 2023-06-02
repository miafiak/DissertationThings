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
library(spatialEco)
library(biclust)
#library(SpatialPack)
library(kohonen)
library(terra)
library(rassta)

#using the terra package to load in the raster datasets
eleva.tif <- rast("data/elevation.tif")
pop.tif <- rast("data/ken_general_2020.tif")
lc.tif <- rast("data/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif")
lc.bare.tif <- rast("data/fractional_cover_lc100/PROBAV_LC100_global_v3.0.1_2019-nrt_Bare-CoverFraction-layer_EPSG-4326.tif")


#read in the rectangle
rectangle <-st_read("data/smallrectangle.shp")

#crop to extent of the rectangle (to test out the code so far):
eleva.rec.tif <- terra::crop(eleva.tif, rectangle)
#eleva.rec.tif <- mask(eleva.tif2, rectangle)

pop.rec.tif <- crop(pop.tif, rectangle)
#pop.rec.tif <- mask(pop.tif2, rectangle)

lc.rec.tif <- crop(lc.tif, rectangle)
lc.bare.rec.tif <- crop(lc.bare.tif, rectangle)

#Create new NA data
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

#create df
dfstack1 <- terra::as.matrix(stack1)
#dfstack2 <- terra::as.data.frame(stack1, xy= TRUE) (this doesn't work because it needs to be a matrix not df, so no)

#Creating the SOM grid
som_grid <- somgrid(xdim = 5, ydim= 5, topo = "hexagonal")
#grid2 <- somgrid(topo = "hexagonal")

#Create the Som Object
som_model <- supersom(data= dfstack1, grid = som_grid, keep.data = TRUE)
#som2 <- supersom(data = dfstack2, grid = grid2)


#this makes a self and super-organising map, with supersom and from kohonen
#som <- supersom(stack1)

plot(som_model)

pamsomter <- som_pam(ref.rast = eleva.rec.tif, kohsom = som_model$SOM, k = 25)

#trying to put it onto the map:
#Get the SOM codebook matrix, which contains the trained SOM weights:
#codebook <- som_model$codes[]

#put into df
#code_df <- as.data.frame(codebook)

#prepare spatial information to assign the SOM

reference_raster <- rast(foc.ele)

#create new Raster with same extent and resolution as reference raster
output_raster <- rast(reference_raster, nlyr = ncol(code_df))

#reshaped_codebook <- matrix(code_df, nrow = nrow(reference_raster), ncol = ncol(reference_raster), byrow = TRUE)
#assign SOM results to spatial information

#values(output_raster) <- code_df

#for (i in 1:ncol(code_df)) {
#  values(output_raster)[[i]] <- matrix(code_df[[i]], nrow = nrow(reference_raster), ncol = ncol(reference_raster), byrow = TRUE)
#}


#https://github.com/MariekeDirk/GeoInterpolation/


#clust <- kmeans(stack1, centers = 5)


# need to make matrix to create a cluster
#val <- values(stack1)
#do the cluster
#k.clust <- kmeans(na.omit(val), centers = 10)

#create raster with same dimensions as stack1
#krast <- stack1
#values(krast) <- k.clust$cluster




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

#Stack all landcovers into one (they do not need any normalisation, they are only 1 or 0:
lc.stack <- c(lc.forest, lc.bare, lc.crops, lc.herb.veg, lc.herb.wet, lc.shrubs, lc.urban, lc.water)

#read in the rectangle
rectangle <-st_read("Data/rectangle.shp")

#crop to extent of the rectangle (to test out the code so far):
eleva.rec.tif <- terra::crop(eleva.tif, rectangle)
pop.rec.tif <- crop(pop.tif, rectangle)

#normalize population and elevation here
popnx <- minmax(pop.rec.tif)
norm.pop <- (pop.rec.tif - popnx [1,]) / (popnx [2,] - popnx [1,])

elnx <- minmax(eleva.rec.tif)
norm.ele <- (eleva.rec.tif - elnx [1,])/ (elnx[2,] - elnx [1,])

#crop the stack to the extent of the rectangle:
lc.rec <- terra::crop(lc.stack, rectangle)


#Create new NA data, assuming that the population in the areas with No Data is 0 (bold assumption but otherwise hard)
#new_value <- 0 
#pop.rec.tif[is.na(pop.rec.tif)] <- new_value

#resample data so I can combine them (this leads to aggregation issues!)
resamp.pop<- resample(norm.pop, eleva.rec.tif)
resamp.lc <- resample(lc.rec, eleva.rec.tif)

#omit NAs by creating a inbetween thing of them
#foc.ele <- focal(eleva.rec.tif)

#Renaming the layer so it becomes identifiable again
#names(foc.ele) <- ("elevation")

#library(raster)
stack1 <- (c(norm.ele, resamp.pop, resamp.lc))

#supersom with Rassta pray for me

dfstack1 <- terra::as.matrix(stack1)
#dfstack2 <- terra::as.data.frame(stack1, xy= TRUE) (this doesn't work because it needs to be a matrix not df, so no)

#Creating the SOM grid
som_grid <- somgrid(xdim = 6, ydim= 6, topo = "hexagonal")
#grid2 <- somgrid(topo = "hexagonal")

#Create the Som Object
som_model <- supersom(data= dfstack1, grid = som_grid, keep.data = TRUE)

plot(som_model)

#Getting the WCSS metric for k means, this inidicates every datapoints distance to nodes
#Using this to indicate the ideal number of clusters
codes <- getCodes(som_model)
wcss <- (nrow(codes)-1)* sum(apply(codes, 2, var))
for (i in 2:15) wcss[i] <- sum(kmeans(codes, centers = i)$withinss)

plot(1:15, wcss, type = "b", xlab = "Number of Clusters",
     ylab= "Within groups of squares", main= "WCSS")




#pamsomter <- som_pam(ref.rast = eleva.rec.tif, kohsom = som_model$SOM, k = 25)

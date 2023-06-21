setwd("~/dissertation/DissertationThings")
library(terra)
library(kohonen)
library(ggplot2)
#library(rgdal)
library(gridExtra)
library(grid)
library(viridis)
library(dplyr)
library(sf)


#read in the admin boundary

adm2.sp <- st_read("Data/ken_admbnda_adm2_iebc_20191031.shp")

#using the terra package to load in the raster datasets
eleva.tif <- rast("Data/Elevation.tif")
pop.tif <- rast("Data/ken_general_2020.tif")
#Landcover read in
lc.gen <- rast("Data/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif")

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

lc.df = data.frame()

(classes <- sort(terra::unique(lc.stack)[,1]))

adm <- vect("Data/ken_admbnda_adm2_iebc_20191031.shp")

levels(lc.stack) <- data.frame(id= 1:8, name= c("forest", "bare", "crops", "herbaceous vegetation", "herb wet", "shrubs", "urban", "water"))

e <- extract(lc.stack, adm, fun= "table", na.rm=TRUE, exact = FALSE)

zone <- rasterize(adm, lc.gen, "ADM2_EN")

zone
df <- data.frame(Name_2=adm$NAME_2[e[,1]], e[,-1])
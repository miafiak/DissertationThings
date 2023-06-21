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

#Transform the zones so that it becomes a raster 
zone <- rasterize(adm2.sp, lc.gen, "ADM2_EN")
#Extract all the zone pixels for each singular lc:
pix.for <- expanse(lc.forest, byValue = TRUE, zones=zone, wide=TRUE)
pix.bar <- expanse(lc.bare, byValue = TRUE, zones=zone, wide=TRUE)
pix.crop <- expanse(lc.crops, byValue = TRUE, zones=zone, wide=TRUE)
pix.hv <- expanse(lc.herb.veg, byValue = TRUE, zones=zone, wide=TRUE)
pix.hw <- expanse(lc.herb.wet, byValue = TRUE, zones=zone, wide=TRUE)
pix.shrb <- expanse(lc.shrubs, byValue = TRUE, zones=zone, wide=TRUE)
pix.urbn <- expanse(lc.urban, byValue = TRUE, zones=zone, wide=TRUE)
pix.wtr <- expanse(lc.water, byValue = TRUE, zones=zone, wide=TRUE)

#Calculate percentage of lc for each district
pix.for$for.percent <- (pix.for$`1`/(pix.for$`0`+pix.for$`1`))
pix.bar$bar.percent <- (pix.bar$`1`/(pix.bar$`0`+pix.bar$`1`))
pix.crop$crop.percent <- (pix.crop$`1`/(pix.crop$`0`+pix.crop$`1`))
pix.hv$hv.percent <- (pix.hv$`1`/(pix.hv$`0`+pix.hv$`1`))
pix.hw$hw.percent <- (pix.hw$`1`/(pix.hw$`0`+pix.hw$`1`))
pix.shrb$shrb.percent <- (pix.shrb$`1`/(pix.shrb$`0`+pix.shrb$`1`))
pix.urbn$urbn.percent <- (pix.urbn$`1`/(pix.urbn$`0`+pix.urbn$`1`))
pix.wtr$wtr.percent <- (pix.wtr$`1`/(pix.wtr$`0`+pix.wtr$`1`))

#Create a df with all the percentages of landcover:
df <- data.frame(adm2.sp$ADM2_EN, pix.for$for.percent, pix.bar$bar.percent, pix.crop$crop.percent,
                 pix.hv$hv.percent, pix.hw$hw.percent, pix.shrb$shrb.percent,
                 pix.urbn$urbn.percent, pix.wtr$wtr.percent)
names(df) <- c("ADM2_EN", "Forest", "Bare", "Crop", "Herbaceous Vegetation",
               "Herbaceous Wetland", "Shrubs", "Urban", "Water")
#Get rid of the names:
data_train <- select(df, 2:9)
#check whether I need the scale or not, what difference does it make
data_train_mtrx <- as.matrix.data.frame(scale(data_train))
names(data_train_mtrx) <- names(df)

som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")

som_model <- som(data_train_mtrx, grid = som_grid, rlen = 500, keep.data = TRUE)


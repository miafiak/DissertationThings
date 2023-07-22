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
library(cluster)
library(fpc)
#library(factoextra)
library(cluster)
library(cuRe)

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

#read in livestock data
cattle <- rast("Data/LiveStock/cattle.tif")
chicken <- rast("Data/LiveStock/chicken.tif")
ducks <- rast("Data/LiveStock/ducks.tif")
goats <- rast("Data/LiveStock/goats.tif")
horses <- rast("Data/LiveStock/horse.tif")
pigs <- rast("Data/LiveStock/pig.tif")
sheeps <- rast("Data/LiveStock/sheep.tif")

#add in travel time to market
travel <- rast("Data/traveltimetomarket_ssa_020k.tif")

#resample livestock data
res.cat <- resample(cattle, lc.gen)
res.chi <- resample(chicken, lc.gen)
res.duc <- resample(ducks, lc.gen)
res.gts <- resample(goats, lc.gen)
res.hrs <- resample(horses, lc.gen)
res.pgs <- resample(pigs, lc.gen)
res.shee <- resample(sheeps, lc.gen)

#resample travel
res.trvl <- resample(travel, lc.gen)

#create a stack
livstck <- c(res.cat, res.chi, res.duc, res.gts, res.hrs, res.pgs,
             res.shee)

#read in other data
eleva.tif <- rast("Data/Elevation.tif")
pop.tif <- rast("Data/ken_general_2020.tif")
#resample them so they fit the same resolution of the others
resa.ele <- resample(eleva.tif, lc.gen)
resa.pop <- resample(pop.tif, lc.gen)

#Transform the zones so that it becomes a raster 
zone <- terra::rasterize(adm2.sp, lc.gen, fun= "sum", overwrite= TRUE, "ADM2_PCODE")
   #Extract the number of zone pixels for each singular data:
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

#Extract the average values per zone for elevation, population and livestock:
ele_zone <- zonal(resa.ele, zone, fun = mean, na.rm = TRUE)
pop_zone <- zonal(resa.pop, zone, fun = mean, na.rm = TRUE)
ls_zone <- zonal(livstck, zone, fun = mean, na.rm = TRUE)
trvl_zone <- zonal(res.trvl, zone, fun = mean, na.rm = TRUE)
names(ls_zone) <- c("ADM2_PCode", "Cattle", "Chicken", "Ducks",
                    "Goats", "Horses", "Pigs", "Sheep")

#Question do I need to put the elevation and population on a 0-1 scale?
#Create a df with all the percentages of landcover:
df <- data.frame(adm2.sp$ADM2_PCODE, pix.for$for.percent, pix.bar$bar.percent, pix.crop$crop.percent,
                 pix.hv$hv.percent, pix.hw$hw.percent, pix.shrb$shrb.percent,
                 pix.urbn$urbn.percent, pix.wtr$wtr.percent, ele_zone$Elevation,
                 pop_zone$ken_general_2020, trvl_zone$traveltimetomarket_ssa_020k, ls_zone$Cattle, ls_zone$Chicken,
                 ls_zone$Ducks, ls_zone$Goats, ls_zone$Horses, ls_zone$Pigs, 
                 ls_zone$Sheep)
names(df) <- c("ADM2_EN", "Forest", "Bare", "Crop", "Herbaceous Vegetation",
               "Herbaceous Wetland", "Shrubs", "Urban", "Water", "Elevation",
               "Population", "Travel Time", "Cattle", "Chicken", "Ducks",
               "Goats", "Horses", "Pigs", "Sheep")




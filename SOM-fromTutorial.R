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
library(ggplot2)
library(ggspatial)



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
zone <- rasterize(adm2.sp, lc.gen, "ADM2_PCODE")
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
#create a new coloumn with only the numbers of the references:
df$Code <- gsub("[A-Za-z]{2}", "", df$ADM2_EN)
df$Code <- as.numeric(df$Code)

# Select all numeric columns that aren't normalized yet

columns_to_normalize <- 10:19

# Normalize the selected columns
df_norm <- df
df_norm[, columns_to_normalize] <- apply(df_norm[, columns_to_normalize], 2, function(x) x / max(x))


#Get rid of the codes:
data_train <- select(df_norm, 2:19)
#check whether I neezoned the scale or not, what difference does it make
data_train_mtrx <- as.matrix.data.frame(scale(data_train))
colnames(data_train_mtrx) <- (c("Forest", "Bare", "Crop", "Herbaceous Vegetation",
                                  "Herbaceous Wetland", "Shrubs", "Urban",
                                "Water", "Elevation", "Population", "Travel Time", "Cattle", "Chicken", "Ducks",
                                "Goats", "Horses", "Pigs", "Sheep" ))

som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")

som_model <- supersom(data_train_mtrx, grid = som_grid, rlen = 10000, keep.data = TRUE)

#show the WCSS metric for different clustering sizes
# Can be used as a rough indicator of the ideal number of clusters

mydata <- getCodes(som_model)
#wss <- (nrow(mydata)-1)*sum(apply(mydata, 2, var))
#for (i in 2:10) wss [i] <- sum(kmeans(mydata, centers = i)$withinss)

#form clusters on grid
##use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(getCodes(som_model))), 25)
plot(som_model, type= "codes", bgcol= som_cluster, main= "Clusters")

cluster_details <- data.frame(ADM2_EN=df$ADM2_EN, 
                              cluster= som_cluster[som_model$unit.classif])
#mappoints <- merge(zone, cluster_details, by= "ADM2_PCODE")

final = zone
cluster_det_srt <- arrange(cluster_details,(cluster_details$ADM2_PCODE))
#export it so I can bring them together in Arc:
write.csv2(cluster_det_srt, file = "Data/clustersadm2.csv")
# Create a spatial join of the clusters and the map
spatialcluster <- merge (adm2.sp, cluster_det_srt, 
                                by.x= "ADM2_PCODE", by.y= "ADM2_EN")
#for easier mapping excluse not needed layers:
filtercluster <- spatialcluster['cluster']

#register_google(key = "", write = TRUE)
#get basemap
bbox <- st_bbox(filtercluster)
##tiles <- OpenStreetMap::openmap(bbox = bbox, type = "osm")

#map <- ggmap(basemap) +
 # geom_sf(data = filtercluster) +
  #annotation_scale(location = "bl") +
  #annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering)

#print(map)


#plot it
plot(spatialcluster['cluster'])

#Aftr defining a color palette for each variable by using the RColorBrewer package, we are running a loop to plot the mean value of the different
#ariables for each cluster.
library(RColorBrewer)

cols <- c(brewer.pal(18, "Spectral"), brewer.pal(11, "BrBG"))

#for (i in 1:som_model$codes){
 # DF <- as.data.frame(colMeans(som_model[som_model$codes ==i, 1:length(som_model[1,])-1]))
  #DF <- data.frame(rownames(DF),DF[,1]); DF[,2] <- DF[,2] +1; DF
  #names(DF) <- c('variable', 'value')
  #DF$variable <- factor(DF$variable, as.character(DF$variable))
#}
#fuck it were creating a new df
cluster <- merge(df_norm, cluster_details, by= 'ADM2_EN',all=FALSE)
#now I need to extract all the clusters with one value
#unique_clusters <- unique(cluster$cluster)
#split df into smaller dfs
singledf <- split(cluster, cluster$cluster)

# Create a list to store the plots
plot_list <- list()

# Loop over the values of 'i'
for (i in 1:25) {
  # Subset the cluster data frame for the desired cluster
  subset_df <- cluster[cluster$cluster == i, ]
  
  # Filter the numeric columns for mean calculation
  numeric_cols <- sapply(subset_df, is.numeric)
  mean_values <- colMeans(subset_df[, numeric_cols])
  
  # Create a data frame with variable and value columns
  DF <- data.frame(variable = names(mean_values), value = mean_values)
  DF[, 2] <- DF[, 2] + 1
  DF$variable <- factor(DF$variable, levels = as.character(DF$variable))
  
  # Create the plot
  plot <- ggplot(DF, aes(variable, value, fill = variable)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    ylim(0, 1.5) +
    scale_fill_manual(values = cols) +
    theme_gray() +
    theme(
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.line = element_blank()
    ) +
    labs(title = paste("Cluster",  i))
  
  
  
  nam <- paste('p', i, sep = '')
  plot_list[[nam]] <- plot + coord_polar()
}

# Accessing the individual plots
p1 <- plot_list$p1
p2 <- plot_list$p2
# and so on...

# Print or further manipulate the individual plots as desired
print(p1)
print(p2)

for (i in 1:25) {
  plot_name <- paste("p", i, sep = "")
  plot <- plot_list[[plot_name]]
  print(plot)
  savePlot(filename = paste0("Cluster", i, sep="", type="png"))
}




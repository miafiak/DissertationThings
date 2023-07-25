setwd("~/dissertation/DissertationThings")
library(dplyr)
library(ggplot2)
library(sf)

# Load the data from the CSV file.
data <- read.csv("Data/ArcTableAdm2R.csv")

#split the data into two different dataframes
cluster_df <- data %>%
  select("ADM2_EN_1", "cluster")
classification_df <- data%>%
  select("ADM2_EN_1", "classification")

#create lists for each cluster
clusters <- list()

# Iterate over each cluster
for (i in 1:25) {
  
  # Initialize the PCodes variable
  PCodes <- c()
  
  # Get the PCODEs for the current cluster
  PCodes <- cluster_df %>% filter(cluster == i) %>%
    pull(ADM2_EN_1)

  
    # Add the PCODE to the list of clusters
    clusters[[i]] <- as.character(PCodes)
}

#create a list for each classification
Mclassification <- list()

#do the same as above
for (i in 1:25) {
  PCodes <- c()
  PCodes <- classification_df %>% filter(classification ==i) %>%
    pull(ADM2_EN_1)
  Mclassification[[i]] <- as.character(PCodes)
}

# Create a function to calculate the Jaccard Index
jaccard_similarity <- function(A, B) {
  intersection = length(intersect(A,B))
  union = length(A) + length(B) - intersection
  return(intersection/union)
}

# Calculate the Jaccard Index for each combination of list items
jaccard_list <- list()
for (i in 1:25) {
  for (j in 1:25) {
    jaccard_list[[paste(i, j, sep = " -Mc ")]] <- jaccard_similarity(clusters[[i]], Mclassification[[j]])
  }
}

#jaccard_table <- matrix(data = jaccard_list, nrow = 25, ncol = 25)
#save this for further analysis
#write.csv(jaccard_table, file = "Data/jaccard_table.csv")

# Filter the Jaccard Index list to remove all items where the Jaccard Index is 0
jaccard_list <- jaccard_list[!jaccard_list %in% c(0)]

#save the simplified list:
#rite.table(as.matrix(jaccard_list), file = "Data/jaccard_simple_list.csv", sep = ";")




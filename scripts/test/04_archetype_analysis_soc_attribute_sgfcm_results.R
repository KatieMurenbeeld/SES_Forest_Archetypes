library(tidyverse)
library(terra)
library(raster)
library(sf)
library(ggplot2)
library(geocmeans)
library(RColorBrewer)
library(viridis)


#----Load the data----
rst_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_soc_attributes_scaled_2024-10-08.tif")
rst <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_soc_attributes_2024-10-08.tif")

# Format for use in geocmeans
dataset <- lapply(names(rst_sc), function(n){
  aband <- rst_sc[[n]]
  return(aband)
})
names(dataset) <- names(rst_sc)

# Calculate the correlation between the attributes


# Use Spatial Generalized Fuzzy C-Means clustering 
# Highest silhouette index = 0.48 with k = 8, m = 1.8, beta = 0.3, alpha = 1.3
# and a 3x3 window
# Silhouette index = 0.86, k = 3, m = 1.2, beta = 0.5, alpha = 0.9, window = 3x3

w <- matrix(1, nrow = 3, ncol = 3)

SGFCM_soc_result <- SGFCMeans(dataset, k = 8, m = 1.8, standardize = FALSE,
                              lag_method = "mean",
                              window = w, alpha = 1.3, beta = 0.3,
                              seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

map_SGFCM_result <- rast(SGFCM_soc_result$rasters)
plot(map_SGFCM_result[["Groups"]])
writeRaster(map_SGFCM_result[["Groups"]], filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_soc_result_", 
                                                            Sys.Date(), ".tif"), overwrite = TRUE)


ordered_eco_soc <- groups_matching(SGFCM_eco_result, SGFCM_soc_result)
plot(ordered_eco_soc$rasters$group1)
plot(ordered_eco_soc$rasters$group2)
plot(ordered_eco_soc$rasters$group3)




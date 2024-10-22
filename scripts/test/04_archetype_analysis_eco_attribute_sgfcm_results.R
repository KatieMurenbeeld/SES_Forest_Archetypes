library(tidyverse)
library(terra)
library(raster)
library(sf)
library(ggplot2)
library(geocmeans)
library(RColorBrewer)
library(viridis)


#----Load the data----
rst_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_eco_attributes_scaled_2024-10-08.tif")
rst <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_eco_attributes_2024-10-08.tif")

# Format for use in geocmeans
dataset <- lapply(names(rst_sc), function(n){
  aband <- rst_sc[[n]]
  return(aband)
})
names(dataset) <- names(rst_sc)

# Calculate the correlation between the attributes


# Use Spatial Generalized Fuzzy C-Means clustering 
# Highest silhouette index = 0.54 with k = 3, m = 1.6, beta = 0.4, alpha = 0.7
# and a 3x3 window

w <- matrix(1, nrow = 3, ncol = 3)

SGFCM_eco_result <- SGFCMeans(dataset, k = 3, m = 1.6, standardize = FALSE,
                              lag_method = "mean",
                              window = w, alpha = 0.7, beta = 0.4,
                              seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")
saveRDS(SGFCM_eco_result, paste0("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/outputs/SGFCM_eco_attr_", 
                                    Sys.Date(), ".rds"))
map_SGFCM_result <- rast(SGFCM_eco_result$rasters)
plot(map_SGFCM_result[["Groups"]])
writeRaster(map_SGFCM_result[["Groups"]], filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_eco_result_", 
                                                            Sys.Date(), ".tif"), overwrite = TRUE)



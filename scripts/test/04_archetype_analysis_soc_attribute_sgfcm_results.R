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
# Silhouette index = 0.87, k = 3, m = 1.2, beta = 0.2, alpha = 0.7, window = 3x3
# Silhouette index = 0.35, k = 6, m = 1.8, beta = 0.7, alpha = 1.4, window = 5x5

w1 <- matrix(1, nrow = 3, ncol = 3)
w2 <- matrix(1, nrow = 5, ncol = 5)

SGFCM_soc_result_k3 <- SGFCMeans(dataset, k = 3, m = 1.2, standardize = FALSE,
                              lag_method = "mean",
                              window = w1, alpha = 0.7, beta = 0.2,
                              seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

map_SGFCM_result_k3 <- rast(SGFCM_soc_result_k3$rasters)
plot(map_SGFCM_result_k3[["Groups"]])
writeRaster(map_SGFCM_result_k3[["Groups"]], filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_soc_result_k3_", 
                                                            Sys.Date(), ".tif"), overwrite = TRUE)
saveRDS(SGFCM_soc_result_k3, paste0("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/outputs/SGFCM_soc_attr_k3_", 
                                 Sys.Date(), ".rds"))

SGFCM_soc_result_k6 <- SGFCMeans(dataset, k = 6, m = 1.8, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w2, alpha = 1.4, beta = 0.7,
                                 seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

map_SGFCM_result_k6 <- rast(SGFCM_soc_result_k6$rasters)
plot(map_SGFCM_result_k6[["Groups"]])
writeRaster(map_SGFCM_result_k6[["Groups"]], filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_soc_result_k6_", 
                                                               Sys.Date(), ".tif"), overwrite = TRUE)
saveRDS(SGFCM_soc_result_k6, paste0("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/outputs/SGFCM_soc_attr_k6_", 
                                 Sys.Date(), ".rds"))

ordered_eco_soc <- groups_matching(SGFCM_eco_result, SGFCM_soc_result)
plot(ordered_eco_soc$rasters$group1)
plot(ordered_eco_soc$rasters$group2)
plot(ordered_eco_soc$rasters$group3)




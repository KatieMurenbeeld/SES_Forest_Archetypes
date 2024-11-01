library(tidyverse)
library(terra)
library(raster)
library(sf)
library(ggplot2)
library(tmap)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(future)
library(spdep)
library(classInt)

#----Load the data----
# need to rerun the cluster to get model results
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
w2 <- matrix(1, nrow = 7, ncol = 7)

SGFCM_soc_result_k3 <- SGFCMeans(dataset, k = 3, m = 1.2, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w1, alpha = 0.7, beta = 0.2,
                                 seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

SGFCM_soc_result_k6 <- SGFCMeans(dataset, k = 6, m = 1.9, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w2, alpha = 0.6, beta = 0.4,
                                 seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")


# Calculate Entropy
## k = 3
arch_all_rst <- rast(SGFCM_soc_result_k3$rasters)
arch_all_rst_conus_belong <- subset(arch_all_rst, 1:3)

arch_all_rst_conus_df <- as.data.frame(arch_all_rst, xy = TRUE)

all_ent_conus <- calcUncertaintyIndex(as.matrix(as.data.frame(arch_all_rst_conus_belong)))

all_ent_conus <- cbind(arch_all_rst_conus_df, all_ent_conus)
all_ent_rst_conus <- rast(all_ent_conus)
plot(all_ent_rst_conus)

writeRaster(all_ent_rst_conus, here::here(paste0("outputs/SGFCM_soc_k3_entropy_", Sys.Date(), ".tif")))

## k = 6
arch_all_rst <- rast(SGFCM_soc_result_k6$rasters)
arch_all_rst_conus_belong <- subset(arch_all_rst, 1:6)

arch_all_rst_conus_df <- as.data.frame(arch_all_rst, xy = TRUE)

all_ent_conus <- calcUncertaintyIndex(as.matrix(as.data.frame(arch_all_rst_conus_belong)))

all_ent_conus <- cbind(arch_all_rst_conus_df, all_ent_conus)
all_ent_rst_conus <- rast(all_ent_conus)
plot(all_ent_rst_conus)

writeRaster(all_ent_rst_conus, here::here(paste0("outputs/SGFCM_soc_k6_entropy_", Sys.Date(), ".tif")))







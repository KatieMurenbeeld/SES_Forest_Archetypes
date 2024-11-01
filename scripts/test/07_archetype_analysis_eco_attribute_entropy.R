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

w1 <- matrix(1, nrow = 3, ncol = 3)
w2 <- matrix(1, nrow = 7, ncol = 7)

SGFCM_eco_result_k3 <- SGFCMeans(dataset, k = 3, m = 1.6, standardize = FALSE,
                              lag_method = "mean",
                              window = w1, alpha = 0.7, beta = 0.4,
                              seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

SGFCM_eco_result_k6 <- SGFCMeans(dataset, k = 6, m = 1.9, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w2, alpha = 0.6, beta = 0.4,
                                 seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

# Calculate Entropy
## k = 3
arch_all_rst <- rast(SGFCM_eco_result_k3$rasters)
arch_all_rst_conus_belong <- subset(arch_all_rst, 1:3)

arch_all_rst_conus_df <- as.data.frame(arch_all_rst, xy = TRUE)

all_ent_conus <- calcUncertaintyIndex(as.matrix(as.data.frame(arch_all_rst_conus_belong)))

all_ent_conus <- cbind(arch_all_rst_conus_df, all_ent_conus)
all_ent_rst_conus <- rast(all_ent_conus)

writeRaster(all_ent_rst_conus, here::here(paste0("outputs/SGFCM_eco_k3_entropy_", Sys.Date(), ".tif")), overwrite = TRUE)

## k = 6
arch_all_rst <- rast(SGFCM_eco_result_k6$rasters)
arch_all_rst_conus_belong <- subset(arch_all_rst, 1:6)

arch_all_rst_conus_df <- as.data.frame(arch_all_rst, xy = TRUE)

all_ent_conus <- calcUncertaintyIndex(as.matrix(as.data.frame(arch_all_rst_conus_belong)))

all_ent_conus <- cbind(arch_all_rst_conus_df, all_ent_conus)
all_ent_rst_conus <- rast(all_ent_conus)
plot(all_ent_rst_conus)

writeRaster(all_ent_rst_conus, here::here(paste0("outputs/SGFCM_eco_k6_entropy_", Sys.Date(), ".tif")), overwrite = TRUE)

test_rast <- rast(here::here("outputs/SGFCM_eco_k6_entropy_2024-11-01.tif"))
class(test_rast)






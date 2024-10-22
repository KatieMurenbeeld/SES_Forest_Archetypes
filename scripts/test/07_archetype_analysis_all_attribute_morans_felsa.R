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
#sgfcm_all_result_k6_mod <- readRDS(here::here("outputs/SGFCM_all_attr_k6_2024-10-15.rds"))
#sgfcm_all_result_k8_mod <- readRDS(here::here("outputs/SGFCM_all_attr_k8_2024-10-15.rds"))
# need to rerun the cluster to get model results
rst_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")
rst <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")

# Format for use in geocmeans
dataset <- lapply(names(rst_sc), function(n){
  aband <- rst_sc[[n]]
  return(aband)
})
names(dataset) <- names(rst_sc)

# Calculate the correlation between the attributes


# Use Spatial Generalized Fuzzy C-Means clustering 
# FCM seed = 1234, Silhouette index = 0.48, k = 6, m = 1.9, window =  7x7 (w2), alpha = 0.6, beta = 0.4
# FCM seed = 1234, Silhouette index = 0.45, k = 8, m = 1.9, window =  3x3 (w1), alpha = 0.5, beta = 0.4

w1 <- matrix(1, nrow = 3, ncol = 3)
w2 <- matrix(1, nrow = 7, ncol = 7)

SGFCM_all_result_k6 <- SGFCMeans(dataset, k = 6, m = 1.9, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w2, alpha = 0.6, beta = 0.4,
                                 seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")


SGFCM_all_result_k8 <- SGFCMeans(dataset, k = 8, m = 1.9, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w1, alpha = 0.5, beta = 0.4,
                                 seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")


# Calculate Fuzzy ELSAs
## k = 6
fuzzy_elsa_k6_rast <- calcFuzzyELSA(SGFCM_all_result_k6, window = matrix(1,nrow = 3, ncol = 3))

cols <- RColorBrewer::brewer.pal(n = 7, "Blues")
vals <- terra::values(fuzzy_elsa_k6_rast, mat = FALSE)
limits <- classIntervals(vals[!is.na(vals)],  n = 7, style = "kmeans") 
plot(fuzzy_elsa_k6_rast, col = cols, breaks = limits$brks)
writeRaster(fuzzy_elsa_k6_rast, here::here(paste0("outputs/SGFCM_k6_felsa_", Sys.Date(), ".tif")))

## k = 8
fuzzy_elsa_k8_rast <- calcFuzzyELSA(SGFCM_all_result_k8, window = matrix(1,nrow = 3, ncol = 3))

cols <- RColorBrewer::brewer.pal(n = 7, "Blues")
vals <- terra::values(fuzzy_elsa_k8_rast, mat = FALSE)
limits <- classIntervals(vals[!is.na(vals)],  n = 7, style = "kmeans") 
plot(fuzzy_elsa_k8_rast, col = cols, breaks = limits$brks)
writeRaster(fuzzy_elsa_k8_rast, here::here(paste0("outputs/SGFCM_k8_felsa_", Sys.Date(), ".tif")))







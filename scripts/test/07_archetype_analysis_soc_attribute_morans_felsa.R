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
w2 <- matrix(1, nrow = 5, ncol = 5)

SGFCM_soc_result_k3 <- SGFCMeans(dataset, k = 3, m = 1.2, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w1, alpha = 0.7, beta = 0.2,
                                 seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

SGFCM_soc_result_k6 <- SGFCMeans(dataset, k = 6, m = 1.8, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w2, alpha = 1.4, beta = 0.7,
                                 seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")


# Calculate Fuzzy ELSAs
## k = 3
fuzzy_elsa_k3_rast <- calcFuzzyELSA(SGFCM_soc_result_k3, window = matrix(1,nrow = 3, ncol = 3))

cols <- RColorBrewer::brewer.pal(n = 7, "Blues")
vals <- terra::values(fuzzy_elsa_k3_rast, mat = FALSE)
limits <- classIntervals(vals[!is.na(vals)],  n = 7, style = "kmeans") 
plot(fuzzy_elsa_k3_rast, col = cols, breaks = limits$brks)
writeRaster(fuzzy_elsa_k3_rast, here::here(paste0("outputs/SGFCM_soc_k3_felsa_", Sys.Date(), ".tif")))

## k = 6
fuzzy_elsa_k6_rast <- calcFuzzyELSA(SGFCM_soc_result_k6, window = matrix(1,nrow = 3, ncol = 3))

cols <- RColorBrewer::brewer.pal(n = 7, "Blues")
vals <- terra::values(fuzzy_elsa_k6_rast, mat = FALSE)
limits <- classIntervals(vals[!is.na(vals)],  n = 7, style = "kmeans") 
plot(fuzzy_elsa_k6_rast, col = cols, breaks = limits$brks)
writeRaster(fuzzy_elsa_k6_rast, here::here(paste0("outputs/SGFCM_soc_k6_felsa_", Sys.Date(), ".tif")))







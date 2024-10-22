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

w <- matrix(1, nrow = 3, ncol = 3)

SGFCM_eco_result <- SGFCMeans(dataset, k = 3, m = 1.6, standardize = FALSE,
                              lag_method = "mean",
                              window = w, alpha = 0.7, beta = 0.4,
                              seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

# Calculate Fuzzy ELSAs
fuzzy_elsa_eco_rast <- calcFuzzyELSA(SGFCM_eco_result, window = matrix(1,nrow = 3, ncol = 3))

cols <- RColorBrewer::brewer.pal(n = 7, "Blues")
vals <- terra::values(fuzzy_elsa_eco_rast, mat = FALSE)
limits <- classIntervals(vals[!is.na(vals)],  n = 7, style = "kmeans") 
plot(fuzzy_elsa_eco_rast, col = cols, breaks = limits$brks)
writeRaster(fuzzy_elsa_eco_rast, here::here(paste0("outputs/SGFCM_eco_felsa_", Sys.Date(), ".tif")), 
            overwrite = TRUE)








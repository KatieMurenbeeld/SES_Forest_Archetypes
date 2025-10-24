library(tidyverse)
library(terra)
library(raster)
library(sf)
library(ggplot2)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(here)


#----Load the data----
rst_sc <- rast(here::here("data/processed/nf_buffers_all_attributes_scaled_2025-08-12.tif"))

# Format for use in geocmeans
dataset <- lapply(names(rst_sc), function(n){
  aband <- rst_sc[[n]]
  return(aband)
})
names(dataset) <- names(rst_sc)

#----Use sgfcm to create forest clusters
w1 <- matrix(1, nrow = 7, ncol = 7)

SGFCM_all_result_k3 <- SGFCMeans(dataset, k = 3, m = 1.2, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w1, alpha = 0.6, beta = 0.6,
                                 seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

saveRDS(SGFCM_all_result_k3, here::here(paste0("outputs/nfbuffers_SGFCM_all_attr_k3_", 
                                    Sys.Date(), ".rds")))

map_SGFCM_result_k3 <- rast(SGFCM_all_result_k3$rasters)
plot(map_SGFCM_result_k3[["Groups"]])
writeRaster(map_SGFCM_result_k3[["Groups"]], filename = here::here(paste0("outputs/nfbuffers_SGFCM_all_result_k3_", 
                                                               Sys.Date(), ".tif")))


            
            
            
            
            
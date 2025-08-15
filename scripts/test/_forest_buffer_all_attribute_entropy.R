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
#sgfcm_all_result_k3_mod <- readRDS(here::here("outputs/SGFCM_all_attr_k3_2024-10-15.rds"))
#sgfcm_all_result_k8_mod <- readRDS(here::here("outputs/SGFCM_all_attr_k8_2024-10-15.rds"))
# need to rerun the cluster to get model results
rst_sc <- rast("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/processed/nf_buffers_all_attributes_scaled_2025-08-15.tif")
rst <- rast("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/processed/nf_buffers_all_attributes_2025-08-15.tif")

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




# Calculate Entropy
## k = 3
arch_all_rst <- rast(SGFCM_all_result_k3$rasters)
arch_all_rst_conus_belong <- subset(arch_all_rst, 1:3)
plot(arch_all_rst_conus_belong)

arch_all_rst_conus_df <- as.data.frame(arch_all_rst, xy = TRUE)

all_ent_conus <- calcUncertaintyIndex(as.matrix(as.data.frame(arch_all_rst_conus_belong)))

all_ent_conus <- cbind(arch_all_rst_conus_df, all_ent_conus)
all_ent_rst_conus <- rast(all_ent_conus)
plot(all_ent_rst_conus)
plot(all_ent_rst_conus$all_ent_conus)

writeRaster(all_ent_rst_conus, here::here(paste0("outputs/nfbuffers_SGFCM_all_k3_entropy_", Sys.Date(), ".tif")))

#----Map the Entropy---------





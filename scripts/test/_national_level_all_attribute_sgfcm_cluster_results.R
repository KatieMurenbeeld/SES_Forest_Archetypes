library(tidyverse)
library(terra)
library(raster)
library(sf)
library(ggplot2)
library(geocmeans)
library(RColorBrewer)
library(viridis)


#----Load the data----
rst_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")
rst <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")

# Format for use in geocmeans
dataset <- lapply(names(rst_sc), function(n){
  aband <- rst_sc[[n]]
  return(aband)
})
names(dataset) <- names(rst_sc)

# Calculate the correlation between the attributes

w1 <- matrix(1, nrow = 3, ncol = 3)

# alpha = 0.3 or 0.5 or 0.8

## alpha = 0.3
SGFCM_all_result_alpha03 <- SGFCMeans(dataset, k = 32, m = 1.3, standardize = FALSE,
                              lag_method = "mean",
                              window = w1, alpha = 0.3, beta = 0.1,
                              seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

saveRDS(SGFCM_all_result_alpha03, 
        paste0("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/outputs/national_level/national_level_sgfcm_cluster_results_alpha03_",
                                    Sys.Date(), ".rds"))

map_SGFCM_all_result_alpha03 <- rast(SGFCM_all_result_alpha03$rasters)
plot(map_SGFCM_all_result_alpha03[["Groups"]])
writeRaster(map_SGFCM_all_result_alpha03[["Groups"]], 
            filename = paste0("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/outputs/national_level/national_level_sgfcm_cluster_results_alpha03_", 
                                                            Sys.Date(), ".tif"))

## alpha = 0.5
SGFCM_all_result_alpha05 <- SGFCMeans(dataset, k = 32, m = 1.3, standardize = FALSE,
                                      lag_method = "mean",
                                      window = w1, alpha = 0.5, beta = 0.1,
                                      seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

saveRDS(SGFCM_all_result_alpha05, 
        paste0("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/outputs/national_level/national_level_sgfcm_cluster_results_alpha05_",
               Sys.Date(), ".rds"))

map_SGFCM_all_result_alpha05 <- rast(SGFCM_all_result_alpha05$rasters)
plot(map_SGFCM_all_result_alpha05[["Groups"]])
writeRaster(map_SGFCM_all_result_alpha05[["Groups"]], 
            filename = paste0("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/outputs/national_level/national_level_sgfcm_cluster_results_alpha05_", 
                              Sys.Date(), ".tif"))

## alpha = 0.8
SGFCM_all_result_alpha08 <- SGFCMeans(dataset, k = 32, m = 1.3, standardize = FALSE,
                                      lag_method = "mean",
                                      window = w1, alpha = 0.8, beta = 0.1,
                                      seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

saveRDS(SGFCM_all_result_alpha08, 
        paste0("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/outputs/national_level/national_level_sgfcm_cluster_results_alpha08_",
               Sys.Date(), ".rds"))

map_SGFCM_all_result_alpha08 <- rast(SGFCM_all_result_alpha08$rasters)
plot(map_SGFCM_all_result_alpha08[["Groups"]])
writeRaster(map_SGFCM_all_result_alpha08[["Groups"]], 
            filename = paste0("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/outputs/national_level/national_level_sgfcm_cluster_results_alpha08_", 
                              Sys.Date(), ".tif"))


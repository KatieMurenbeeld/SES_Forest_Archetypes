library(tidyverse)
library(terra)
library(raster)
library(sf)
library(ggplot2)
library(geocmeans)
library(RColorBrewer)
library(viridis)


#----Load the data----
## this is the "raw" data not the scaled data
rst <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")

#----Log transform the data----
rst_log <- as.data.frame(lapply(rst, log))

rst$pct_forpay_log <- log(rst$pct_forpay)

#----Format for use in geocmeans----
dataset <- lapply(names(rst), function(n){
  aband <- rst[[n]]
  return(aband)
})
names(dataset) <- names(rst)




# Calculate the correlation between the attributes

w1 <- matrix(1, nrow = 3, ncol = 3)

# alpha = 0.3 or 0.5 or 0.8

## alpha = 0.3
SGFCM_all_result_alpha03_dflt_stnd <- SGFCMeans(dataset, k = 32, m = 1.3, standardize = TRUE,
                              lag_method = "mean",
                              window = w1, alpha = 0.3, beta = 0.1,
                              seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

 saveRDS(SGFCM_all_result_alpha03_dflt_stnd, 
        paste0("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/outputs/national_level/national_level_sgfcm_cluster_results_alpha03_dflt_stnd_",
                                    Sys.Date(), ".rds"))

map_SGFCM_all_result_alpha03_dflt_stnd <- rast(SGFCM_all_result_alpha03_dflt_stnd$rasters)
plot(map_SGFCM_all_result_alpha03_dflt_stnd[["Groups"]])
writeRaster(map_SGFCM_all_result_alpha03_dflt_stnd[["Groups"]], 
            filename = paste0("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/outputs/national_level/national_level_sgfcm_cluster_results_alpha03_dflt_stnd_", 
                                                            Sys.Date(), ".tif"))
options(future.globals.maxSize = 768 * 1024^2)
SGFCMvalues_dflt_stnd <- select_parameters.mc(algo = "SGFCM", data = dataset, standardize = TRUE,
                                  k = 32, m = 1.3,
                                  beta = 0.1, alpha = 0.3,
                                  window = w1, lag_method = "mean",
                                  spconsist = TRUE, 
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Negentropy.index", "Silhouette.index"),
                                  seed = 1234, verbose = TRUE) 
## do a log transform on heavily skewed data and then normalize with min-max



SGFCM_all_result_alpha03_log_then_norm <- SGFCMeans(dataset, k = 32, m = 1.3, standardize = FALSE,
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


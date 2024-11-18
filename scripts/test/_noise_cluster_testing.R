library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(raster)
library(future)

# Load the attribute data
rst_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")

# Format for use in geocmeans
dataset <- lapply(names(rst_sc), function(n){
  aband <- rst_sc[[n]]
  return(aband)
})
names(dataset) <- names(rst_sc)

# Use Spatial Generalized Fuzzy C-Means clustering 
# FCM seed = 1234, Silhouette index = 0.48, k = 6, m = 1.9, window =  7x7 (w2), alpha = 0.6, beta = 0.4

w <- matrix(1, nrow = 7, ncol = 7)

sgfcm_n1 <- SGFCMeans(dataset, k = 6, m = 1.9, standardize = FALSE,
                      lag_method = "mean", robust = TRUE, 
                      window = w, alpha = 0.6, beta = 0.4,
                      noise = TRUE, delta = 0.1,
                      seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

sgfcm_n2 <- SGFCMeans(dataset, k = 6, m = 1.9, standardize = FALSE,
                      lag_method = "mean", robust = TRUE,
                      window = w, alpha = 0.6, beta = 0.4,
                      noise = TRUE, delta = 0.25,
                      seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

sgfcm_n3 <- SGFCMeans(dataset, k = 6, m = 1.9, standardize = FALSE,
                      lag_method = "mean", robust = TRUE,
                      window = w, alpha = 0.6, beta = 0.4,
                      noise = TRUE, delta = 0.5,
                      seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

sgfcm_n4 <- SGFCMeans(dataset, k = 6, m = 1.9, standardize = FALSE,
                      lag_method = "mean", robust = TRUE,
                      window = w, alpha = 0.6, beta = 0.4,
                      noise = TRUE, delta = 0.75,
                      seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")


sgfcm_n1_rst <- rast(sgfcm_n1$rasters)
sgfcm_n2_rst <- rast(sgfcm_n2$rasters)
sgfcm_n3_rst <- rast(sgfcm_n3$rasters)
sgfcm_n4_rst <- rast(sgfcm_n4$rasters)

plot(sgfcm_n1_rst$Groups)
plot(sgfcm_n2_rst$Groups)
plot(sgfcm_n3_rst$Groups)
plot(sgfcm_n4_rst$Groups)


plot(sgfcm_n4$noise_cluster)

# look at the uncertainty
sgfcm_n1_belong <- subset(sgfcm_n1_rst, 1:6)

sgfcm_n1_belong_df <- as.data.frame(sgfcm_n1_belong, xy = TRUE)

sgfcm_n1_ent_conus <- calcUncertaintyIndex(as.matrix(as.data.frame(sgfcm_n1_belong)))

sgfcm_n1_ent_conus <- cbind(sgfcm_n1_belong_df, sgfcm_n1_ent_conus)
sgfcm_n1_ent_rst_conus <- rast(sgfcm_n1_ent_conus)
plot(sgfcm_n1_ent_rst_conus$sgfcm_n1_ent_conus)

# slightly different cluster distribution with noise cluster
# still getting high (>0.9) entropy values

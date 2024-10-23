library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(ggpattern)
library(distributional)
library(ggdist)
library(ggsci)
library(tigris)

# Load the data
sgfcm_all_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")
sgfcm_all_attri_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")

sgfcm_all_k6_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_all_result_k6_2024-10-15.tif")
sgfcm_all_k8_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_all_result_k8_2024-10-15.tif")

sgfcm_eco_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_eco_attributes_2024-10-08.tif")
sgfcm_eco_attri_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_eco_attributes_scaled_2024-10-08.tif")

sgfcm_soc_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_soc_attributes_2024-10-08.tif")
sgfcm_soc_attri_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_soc_attributes_scaled_2024-10-08.tif")

plot(sgfcm_all_k6_result)
plot(sgfcm_all_k8_result)

#sgfcm_all_result_k6_mod <- readRDS(here::here("outputs/SGFCM_all_attr_k6_2024-10-15.rds"))
#sgfcm_all_result_k8_mod <- readRDS(here::here("outputs/SGFCM_all_attr_k8_2024-10-15.rds"))


# Create belongings maps for each archetype
# set the undecided threshold to 0.167 for k = 6 and 0.125 for k = 8 
# Format for use in geocmeans
dataset <- lapply(names(sgfcm_all_attri_sc), function(n){
  aband <- sgfcm_all_attri_sc[[n]]
  return(aband)
})
names(dataset) <- names(sgfcm_all_attri_sc)

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

dataset <- lapply(names(sgfcm_eco_attri_sc), function(n){
  aband <- sgfcm_eco_attri_sc[[n]]
  return(aband)
})
names(dataset) <- names(sgfcm_eco_attri_sc)


SGFCM_eco_result <- SGFCMeans(dataset, k = 3, m = 1.6, standardize = FALSE,
                              lag_method = "mean",
                              window = w1, alpha = 0.7, beta = 0.4,
                              seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

dataset <- lapply(names(sgfcm_soc_attri_sc), function(n){
  aband <- sgfcm_soc_attri_sc[[n]]
  return(aband)
})
names(dataset) <- names(sgfcm_soc_attri_sc)


SGFCM_soc_result_k3 <- SGFCMeans(dataset, k = 3, m = 1.2, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w1, alpha = 0.7, beta = 0.2,
                                 seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")


# create the probability of belonging maps

sgfcm_all_k6_maps <- mapClusters(object = SGFCM_all_result_k6, undecided = 0.167)
sgfcm_all_k8_maps <- mapClusters(object = SGFCM_all_result_k8, undecided = 0.125)

sgfcm_eco_k3_maps <- mapClusters(object = SGFCM_eco_result, undecided = 0.33)

sgfcm_soc_k3_maps <- mapClusters(object = SGFCM_soc_result_k3, undecided = 0.33)


sgfcm_all_k6_maps$ProbaMaps[[1]]
sgfcm_all_k6_maps$ProbaMaps[[2]]
sgfcm_all_k6_maps$ProbaMaps[[3]]
sgfcm_all_k6_maps$ProbaMaps[[4]]
sgfcm_all_k6_maps$ProbaMaps[[5]]
sgfcm_all_k6_maps$ProbaMaps[[6]]

sgfcm_eco_k3_maps$ProbaMaps[[1]]
sgfcm_eco_k3_maps$ProbaMaps[[2]]
sgfcm_eco_k3_maps$ProbaMaps[[3]]

sgfcm_soc_k3_maps$ProbaMaps[[1]]
sgfcm_soc_k3_maps$ProbaMaps[[2]]
sgfcm_soc_k3_maps$ProbaMaps[[3]]












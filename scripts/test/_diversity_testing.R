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
library(exactextractr)
library(styler)

# Load the data
sgfcm_all_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")
sgfcm_all_attri_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")
sgfcm_eco_attri_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_eco_attributes_scaled_2024-10-08.tif")
sgfcm_soc_attri_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_soc_attributes_scaled_2024-10-08.tif")

sgfcm_all_k6_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_all_result_k6_2024-10-15.tif")

# Format for use in geocmeans
dataset <- lapply(names(sgfcm_all_attri_sc), function(n) {
  aband <- sgfcm_all_attri_sc[[n]]
  return(aband)
})
names(dataset) <- names(sgfcm_all_attri_sc)

# Use Spatial Generalized Fuzzy C-Means clustering
# FCM seed = 1234, Silhouette index = 0.48, k = 6, m = 1.9, window =  7x7 (w2), alpha = 0.6, beta = 0.4
# FCM seed = 1234, Silhouette index = 0.45, k = 8, m = 1.9, window =  3x3 (w1), alpha = 0.5, beta = 0.4

w <- matrix(1, nrow = 7, ncol = 7)

SGFCM_all_result_k6 <- SGFCMeans(dataset, k = 6, m = 1.9, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w, alpha = 0.6, beta = 0.4,
                                 seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

dataset <- lapply(names(sgfcm_eco_attri_sc), function(n) {
  aband <- sgfcm_eco_attri_sc[[n]]
  return(aband)
})
names(dataset) <- names(sgfcm_eco_attri_sc)

SGFCM_eco_result_k6 <- SGFCMeans(dataset, k = 6, m = 1.9, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w, alpha = 0.6, beta = 0.4,
                                 seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

dataset <- lapply(names(sgfcm_soc_attri_sc), function(n) {
  aband <- sgfcm_soc_attri_sc[[n]]
  return(aband)
})
names(dataset) <- names(sgfcm_soc_attri_sc)

SGFCM_soc_result_k6 <- SGFCMeans(dataset, k = 6, m = 1.9, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w, alpha = 0.6, beta = 0.4,
                                 seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

# remember the raster has the belongings for each group
# calculate H (shannon diversity) for each pixel
# use the calcUncertaintyIndex() from geocmeans

# load forest buffers shapefile

nf_buffers <- read_sf(here::here("data/processed/nf_buffers_50k_2024-10-22.shp"))

tmp_shp <- nf_buffers %>%
  filter(FORESTORGC == "0412")

arch_soc_rst <- rast(SGFCM_soc_result_k6$rasters)
arch_eco_rst <- rast(SGFCM_eco_result_k6$rasters)
arch_all_rst <- rast(SGFCM_all_result_k6$rasters)

arch_all_rst_crop <- crop(arch_all_rst, nf_buffers, mask = TRUE)
arch_eco_rst_crop <- crop(arch_eco_rst, nf_buffers, mask = TRUE)
arch_soc_rst_crop <- crop(arch_soc_rst, nf_buffers, mask = TRUE)

arch_all_rst_belong <- subset(arch_all_rst_crop, 1:6)
arch_all_rst_conus_belong <- subset(arch_all_rst, 1:6)
arch_eco_rst_belong <- subset(arch_eco_rst_crop, 1:6)
arch_eco_rst_conus_belong <- subset(arch_eco_rst, 1:6)
arch_soc_rst_belong <- subset(arch_soc_rst_crop, 1:6)
arch_soc_rst_conus_belong <- subset(arch_soc_rst, 1:6)

arch_all_rst_df <- as.data.frame(arch_all_rst_belong, xy = TRUE)
arch_all_rst_conus_df <- as.data.frame(arch_all_rst, xy = TRUE)
arch_eco_rst_df <- as.data.frame(arch_eco_rst_belong, xy = TRUE)
arch_eco_rst_conus_df <- as.data.frame(arch_eco_rst, xy = TRUE)
arch_soc_rst_df <- as.data.frame(arch_soc_rst_belong, xy = TRUE)
arch_soc_rst_conus_df <- as.data.frame(arch_soc_rst, xy = TRUE)

all_ent <- calcUncertaintyIndex(as.matrix(as.data.frame(arch_all_rst_belong)))
all_ent_conus <- calcUncertaintyIndex(as.matrix(as.data.frame(arch_all_rst_conus_belong)))
eco_ent <- calcUncertaintyIndex(as.matrix(as.data.frame(arch_eco_rst_belong)))
eco_ent_conus <- calcUncertaintyIndex(as.matrix(as.data.frame(arch_eco_rst_conus_belong)))
soc_ent <- calcUncertaintyIndex(as.matrix(as.data.frame(arch_soc_rst_belong)))
soc_ent_conus <- calcUncertaintyIndex(as.matrix(as.data.frame(arch_soc_rst_conus_belong)))

all_ent <- cbind(arch_all_rst_df, all_ent)
all_ent_rst <- rast(all_ent)
all_ent_conus <- cbind(arch_all_rst_conus_df, all_ent_conus)
all_ent_rst_conus <- rast(all_ent_conus)

eco_ent <- cbind(arch_eco_rst_df, eco_ent)
eco_ent_rst <- rast(eco_ent)
eco_ent_conus <- cbind(arch_eco_rst_conus_df, eco_ent_conus)
eco_ent_rst_conus <- rast(eco_ent_conus)

soc_ent <- cbind(arch_soc_rst_df, soc_ent)
soc_ent_rst <- rast(soc_ent)
soc_ent_conus <- cbind(arch_soc_rst_conus_df, soc_ent_conus)
soc_ent_rst_conus <- rast(soc_ent_conus)


plot(all_ent_rst_conus$all_ent_conus)
plot(eco_ent_rst_conus$eco_ent_conus)
plot(soc_ent_rst_conus$soc_ent_conus)


plot(arch_rst_crop)
plot(ent_rst$test_ent)
plot(ent_rst_conus$test_ent_conus)

test_ent <- test_ent %>%
  mutate(fct_ent = cut(test_ent, breaks = 3))

ggplot() +
  geom_bar(data = test_ent, aes(fct_ent))


#custom_bins <- c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
custom_bins <- c(0.9, 1.0)

test_ent <- test_ent %>%
  mutate(fct_ent_2 = cut(test_ent, breaks = custom_bins))

ggplot() +
  geom_raster(data = test_ent , aes(x = x, y = y, fill = fct_ent_2))

mean(test_ent$test_ent)

test_ent_conus <- test_ent_conus %>%
  mutate(fct_ent = cut(test_ent_conus, breaks = 3))

ggplot() +
  geom_bar(data = test_ent_conus, aes(fct_ent))


#custom_bins <- c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
custom_bins <- c(0.9, 1.0)

test_ent_conus <- test_ent_conus %>%
  mutate(fct_ent_2 = cut(test_ent_conus, breaks = custom_bins))

ggplot() +
  geom_raster(data = test_ent_conus , aes(x = x, y = y, fill = fct_ent_2))
mean(test_ent$test_ent)



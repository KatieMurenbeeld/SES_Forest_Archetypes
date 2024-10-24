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

# Load the data
sgfcm_all_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")
sgfcm_all_attri_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")

#sgfcm_all_k6_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_all_result_k6_2024-10-15.tif")
#sgfcm_all_k8_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_all_result_k8_2024-10-15.tif")

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

SGFCM_all_result_k6 <- SGFCMeans(dataset, k = 6, m = 1.9, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w2, alpha = 0.6, beta = 0.4,
                                 seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")



# crop to regions 1 and 4
fs_nf <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeForest.shp")
fs_reg <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeRegion.shp")

projection <- "epsg: 5070"

fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_nf.crop <- st_crop(fs_nf.proj, ext(sgfcm_all_attri_sc))
fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.crop <- st_crop(fs_reg.proj, ext(sgfcm_all_attri_sc))

# create buffers around the forests in regions 1 and 4
nf_create_buffers <- function(area_with_nf, dist_m){
  nf_buffs <- data.frame()
  for (nf in 1:109) {
    #print(reg4_nf$FORESTORGC[nf])
    tmp_nf <- area_with_nf %>%
      filter(FORESTORGC == area_with_nf$FORESTORGC[nf])
    tmp_nf_buf <- st_buffer(tmp_nf, dist = dist_m)
    nf_buffs <- rbind(nf_buffs, tmp_nf_buf)
  }
  return(nf_buffs)
}

nf_buffers <- nf_create_buffers(fs_nf.crop, 50000)


#----elsa for each forest?
library(elsa)
arch_rst <- rast(SGFCM_all_result_k6$rasters)

test_crop1 <- crop(arch_rst, tmp_shp, mask = TRUE)
test_sub1 <- subset(test_crop1, 1:6)

test_crop2 <- crop(arch_rst, tmp_shp2, mask = TRUE)
test_sub2 <- subset(test_crop2, 1:6)

elsa1 <- elsa(raster(test_crop1$Groups), d = 3000, stat = "Ec")
plot(test_crop1$Groups)
plot(elsa1)

elsa1_sub <- elsa(raster(test_sub1), d = 3000, stat = "Ec")
plot(test_sub1)
plot(crop(elsa1_sub, tmp_shp, mask = TRUE))

elsa2 <- elsa(raster(test_crop2$Groups), d = 3000, stat = "Ec")
plot(crop(rast(elsa2), tmp_shp2, mask = TRUE))
plot(test_crop2$Groups)

elsa2_sub <- elsa(raster(test_sub2), d = 3000, stat = "Ec")
plot(test_sub2)
plot(crop(rast(elsa2_sub), tmp_shp2, mask = TRUE))


mean(elsa2_sub$Ec)




felsa_1 <- calcFuzzyELSA(as.data.frame(test_crop1), window = matrix(1,nrow = 3, ncol = 3))
felsa_1 <- calcFuzzyELSA(maybe, window = matrix(1,nrow = 3, ncol = 3))

maybe <- as.matrix(as.data.frame(test_crop1))
as.matrix(test_crop1, )
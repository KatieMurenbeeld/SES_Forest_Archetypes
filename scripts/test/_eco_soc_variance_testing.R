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
w2 <- matrix(1, nrow = 7, ncol = 7)

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


# I want to get the variance of social and ecological variables for each forest
# Load the data
sgfcm_eco_attri_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_eco_attributes_scaled_2024-10-08.tif")
sgfcm_eco_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_eco_attributes_2024-10-08.tif")

sgfcm_soc_attri_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_soc_attributes_scaled_2024-10-08.tif")
sgfcm_soc_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_soc_attributes_2024-10-08.tif")

forest_var_df <- data.frame(
  region = as.character(),
  forest = as.character(),
  eco_var = as.numeric(),
  soc_var = as.numeric()
)

t = 75
for (nf in nf_buffers$FORESTORGC){
  tmp_shp <- nf_buffers %>%
    filter(FORESTORGC == nf)
  #tmp_eco_rast <- crop(sgfcm_eco_attri_sc, tmp_shp, mask = TRUE)
  #tmp_soc_rast <- crop(sgfcm_soc_attri_sc, tmp_shp, mask = TRUE)
  tmp_rast <- crop(sgfcm_all_attri, tmp_shp, mask = TRUE)
  region <- nf_buffers$REGION[nf_buffers$FORESTORGC == nf]
  forest <- nf
  eco_var <- sqrt(sum(exact_extract(sgfcm_eco_attri, tmp_shp, fun = "stdev")))
  soc_var <- sqrt(sum(exact_extract(sgfcm_soc_attri, tmp_shp, fun = "stdev")))
  tmp_undecided <- any(max(tmp_rast) < t)
  tmp_df <- freq(tmp_undecided)
  forest_var_df[nrow(forest_var_df) + 1,] <- as.list(c(region,
                                                       forest, 
                                                       as.numeric(eco_var),
                                                       as.numeric(soc_var)))
}


#plot(forest_var_df$eco_var, forest_var_df$soc_var)

test_scatter <- forest_var_df %>%
  ggplot(aes(x = as.numeric(eco_var), y = as.numeric(soc_var), group=region, color=region)) +
  geom_point() + 
  #scale_x_continuous(breaks = seq(10, 110, by = 10)) +
  theme_bw() +
  theme(legend.position="bottom")
test_scatter

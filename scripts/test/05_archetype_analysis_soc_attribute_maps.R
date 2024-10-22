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
fs_nf <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeForest.shp")
fs_reg <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeRegion.shp")
sgfcm_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_soc_attributes_2024-10-08.tif")
sgfcm_result_k3 <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_soc_result_k3_2024-10-22.tif") 
sgfcm_result_k6 <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_soc_result_k6_2024-10-22.tif") 

## Reproject the forest service shapes to NAD83
projection <- "epsg: 5070"

fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  filter(FORESTORGC != "0816") %>%
  st_transform(., crs=projection)
fs_nf.crop <- st_crop(fs_nf.proj, ext(sgfcm_result_k3))
fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.crop <- st_crop(fs_reg.proj, ext(sgfcm_result_k3))

## Create a map of the clusters with the Region and National Forest boundaries
# Silhouette index = 0.87, k = 3, m = 1.2, beta = 0.2, alpha = 0.7, window = 3x3
# Silhouette index = 0.35, k = 6, m = 1.8, beta = 0.7, alpha = 1.4, window = 5x5
sgfcm.k3.df <- sgfcm_result_k3$Groups %>% as.data.frame(xy = TRUE)
sgfcm.k6.df <- sgfcm_result_k6$Groups %>% as.data.frame(xy = TRUE)

soc_rg_nf_map_k3 <- ggplot() +
  geom_raster(aes(x = sgfcm.k3.df$x, y = sgfcm.k3.df$y, fill = as.factor(sgfcm.k3.df$Groups))) +
  geom_sf(data = fs_nf.crop, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Social Attributes:",
       subtitle = "k=3, m=1.2, alpha = 0.7, beta = 0.2, window = 3x3", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

soc_rg_nf_map_k3
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_soc_reg_nf_map_k3_", Sys.Date(), ".png"), 
       plot = soc_rg_nf_map_k3, width = 12, height = 12, dpi = 300)  

soc_rg_nf_map_k6 <- ggplot() +
  geom_raster(aes(x = sgfcm.k6.df$x, y = sgfcm.k6.df$y, fill = as.factor(sgfcm.k6.df$Groups))) +
  geom_sf(data = fs_nf.crop, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Social Attributes:",
       subtitle = "k=6, m=1.8, alpha = 1.4, beta = 0.7, window = 5x5", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

soc_rg_nf_map_k6
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_soc_reg_nf_map_k6_", Sys.Date(), ".png"), 
       plot = soc_rg_nf_map_k6, width = 12, height = 12, dpi = 300)  


#--------------
# quick groups matching test
ordered_sgfcm_eco <- groups_matching(SGFCM_all_result, SGFCM_eco_result)
# Error in qual_mat[rkeep, ckeep] : (subscript) logical subscript too long
# can't do because different number of clusters

ordered_sgfcm_eco <- groups_matching(SGFCM_all_result, SGFCM_eco_result_k8)

ordered_sgfcm_soc <- groups_matching(SGFCM_all_result, SGFCM_soc_result_k8)

ordered_soc_eco <- groups_matching(SGFCM_eco_result_k8, SGFCM_soc_result_k8)

map_groups_matching <- rast(ordered_sgfcm_eco$rasters)
plot(map_groups_matching[["Groups"]])
map_eco_k8 <- rast(SGFCM_eco_result_k8$rasters)
plot(map_eco_k8[["Groups"]])
all_map <- rast(SGFCM_all_result$rasters)
plot(all_map[["Groups"]])


matX <- SGFCM_all_result$Belongings
matY <- SGFCM_eco_result_k8$Belongings

k <- ncol(matX)
qual_mat <- calc_jaccard_mat(matX,matY)
colnames(qual_mat) <- 1:ncol(qual_mat)
rownames(qual_mat) <- 1:nrow(qual_mat)




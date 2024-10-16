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
sgfcm_all_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")
sgfcm_all_k6_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_all_result_k6_2024-10-15.tif")
sgfcm_all_k8_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_all_result_k8_2024-10-15.tif")

plot(sgfcm_all_k6_result)
plot(sgfcm_all_k8_result)

sgfcm_all_result_k6_mod <- readRDS(here::here("outputs/SGFCM_all_attr_k6_2024-10-15.rds"))
sgfcm_all_result_k8_mod <- readRDS(here::here("outputs/SGFCM_all_attr_k8_2024-10-15.rds"))

## Reproject the forest service shapes to NAD83
projection <- "epsg: 5070"

fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  filter(FORESTORGC != "0816") %>%
  st_transform(., crs=projection)
fs_nf.crop <- st_crop(fs_nf.proj, ext(sgfcm_all_k6_result))
fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.crop <- st_crop(fs_reg.proj, ext(sgfcm_all_k6_result))

## Create a map of the clusters with the Region and National Forest boundaries
# for k = 6
sgfcm.k6.all.df <- sgfcm_all_k6_result$Groups %>% as.data.frame(xy = TRUE)

all_k6_rg_nf_map <- ggplot() +
  geom_raster(aes(x = sgfcm.k6.all.df$x, y = sgfcm.k6.all.df$y, fill = as.factor(sgfcm.k6.all.df$Groups))) +
  geom_sf(data = fs_nf.crop, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "All Attributes:",
       subtitle = "k=6, m=1.9, alpha = 0.6, beta = 0.4, window = 7x7", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

all_k6_rg_nf_map
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_all_k6_reg_nf_map_", Sys.Date(), ".png"), 
       plot = all_k6_rg_nf_map, width = 12, height = 12, dpi = 300) 

# repeat for k = 8

sgfcm.k8.all.df <- sgfcm_all_k8_result$Groups %>% as.data.frame(xy = TRUE)

all_k8_rg_nf_map <- ggplot() +
  geom_raster(aes(x = sgfcm.k8.all.df$x, y = sgfcm.k8.all.df$y, fill = as.factor(sgfcm.k8.all.df$Groups))) +
  geom_sf(data = fs_nf.crop, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "All Attributes:",
       subtitle = "k=8, m=1.9, alpha = 0.5, beta = 0.4, window = 3x3", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

all_k8_rg_nf_map
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_all_k8_reg_nf_map_", Sys.Date(), ".png"), 
       plot = all_k8_rg_nf_map, width = 12, height = 12, dpi = 300) 

# Look at the belongings for each cluster/archetype







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
library(tidyverse)

# Load the data
fs_nf <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeForest.shp")
fs_reg <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeRegion.shp")
sgfcm_all_attri <- rast("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/processed/nf_buffers_all_attributes_scaled_2025-08-12.tif")
sgfcm_all_result <- rast("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/outputs/nfbuffers_SGFCM_all_result_k3_2025-08-12.tif") 
plot(sgfcm_all_result)
## Reproject the forest service shapes to NAD83
projection <- "epsg: 5070"
sf_use_s2(TRUE)

fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  filter(FORESTORGC != "0816") %>%
  st_transform(., crs=projection)
fs_nf.crop <- st_crop(fs_nf.proj, ext(sgfcm_all_result))
fs_reg.proj <- fs_reg %>% 
  filter(REGION != as.numeric("10")) %>%
  st_transform(., crs=projection)
fs_reg.crop <- st_crop(fs_reg.proj, ext(sgfcm_all_result))

## Create a map of the clusters with the Region and National Forest boundaries
sgfcm.all.df <- sgfcm_all_result$Groups %>% as.data.frame(xy = TRUE)

all_rg_nf_map <- ggplot() +
  geom_raster(aes(x = sgfcm.all.df$x, y = sgfcm.all.df$y, fill = as.factor(sgfcm.all.df$Groups))) +
  geom_sf(data = fs_nf.crop, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "All Attributes:",
       subtitle = "k=3, m=1.2, alpha = 0.6, beta = 0.6, window = 7x7", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

all_rg_nf_map
ggsave(paste0("~/Analysis/SES_Forest_Archetypes/outputs/plots/nfbuffers_sgfcm_all_reg_nf_map_", Sys.Date(), ".png"), 
       plot = all_rg_nf_map, width = 12, height = 12, dpi = 300)  



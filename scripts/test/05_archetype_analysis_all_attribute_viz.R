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
sgfcm_all_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-01.tif")
sgfcm_all_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_all_result_2024-10-02.tif") 
plot(sgfcm_all_result)
## Reproject the forest service shapes to NAD83
projection <- "epsg: 5070"
sf_use_s2(FALSE)

fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  filter(FORESTORGC != "0816") %>%
  st_transform(., crs=projection)
fs_nf.crop <- st_crop(fs_nf.proj, ext(sgfcm_all_result))
fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
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
       subtitle = "k=8, m=1.6, alpha = 1.3, beta = 0.3, window = 3x3", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

all_rg_nf_map
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_all_reg_nf_map_", Sys.Date(), ".png"), 
       plot = all_rg_nf_map, width = 12, height = 12, dpi = 300)  



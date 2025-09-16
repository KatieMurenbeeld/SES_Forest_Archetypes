library(tidyverse)
library(terra)
library(raster)
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
library(MetBrewer)
library(cowplot)

# Load the data
fs_nf <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeForest.shp")
fs_reg <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeRegion.shp")
sgfcm_all_k6_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_all_result_k6_2024-10-15.tif")
sgfcm_all_attri_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")
sgfcm_all_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")
sgfcm_all_result_k6_mod <- readRDS(here::here("outputs/SGFCM_all_attr_k6_2024-10-15.rds"))

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

nf_buffers <- st_read(here::here("data/processed/nf_buffers_50k_2024-10-22.shp"))

states <- tigris::states()
states.proj <- st_transform(states, crs=projection)
wusa <- states.proj %>%
  filter(STUSPS == "MT" | STUSPS == "ID" | STUSPS == "NV" | STUSPS == "UT" | STUSPS == "OR" | STUSPS == "CA" | STUSPS == "WA" | STUSPS == "AZ" | STUSPS == "WY")
wusa.crop <- st_crop(wusa, ext(sgfcm_all_k6_result))
plot(wusa.crop$geometry)

wusa_result <- crop(sgfcm_all_k6_result, wusa.crop, mask = TRUE)
plot(wusa_result)

wusa_nf <- st_intersection(fs_nf.crop, wusa.crop)
plot(wusa_nf$geometry)
## filter for sample forests
# sawtooth (l-l, 0414) and payette (h-h, 0412)

exp1 <- "Sequoia National Forest"
exp2 <- "Kaibab National Forest"

exp1_buff <- nf_buffers %>%
  filter(FORESTNAME == exp1)

exp2_buff <- nf_buffers %>%
  filter(FORESTNAME == exp2)

exp1_crop <- crop(sgfcm_all_k6_result, exp1_buff, mask = TRUE)
exp2_crop <- crop(sgfcm_all_k6_result, exp2_buff, mask = TRUE)

exp1_nf <- fs_nf.crop %>%
  filter(FORESTNAME == exp1)
exp2_nf <- fs_nf.crop %>%
  filter(FORESTNAME == exp2)
  
plot(exp1_crop)
plot(exp2_crop)

## Create a map of the clusters with the Region and National Forest boundaries
# for k = 6
exp1.df <- exp1_crop$Groups %>% as.data.frame(xy = TRUE)
exp2.df <- exp2_crop$Groups %>% as.data.frame(xy = TRUE)
wusa.df <- wusa_result$Groups %>% as.data.frame(xy = TRUE)
# Rename group (archetypes) to letters
exp1.df <- exp1.df %>%
  mutate(group_alpha = case_when(Groups == 1 ~ "A", 
                                 Groups == 2 ~ "B",
                                 Groups == 3 ~ "C",
                                 Groups == 4 ~ "D",
                                 Groups == 5 ~ "E", 
                                 Groups == 6 ~ "F"))

exp2.df <- exp2.df %>%
  mutate(group_alpha = case_when(Groups == 1 ~ "A", 
                                 Groups == 2 ~ "B",
                                 Groups == 3 ~ "C",
                                 Groups == 4 ~ "D",
                                 Groups == 5 ~ "E", 
                                 Groups == 6 ~ "F"))

wusa.df <- wusa.df %>%
  mutate(group_alpha = case_when(Groups == 1 ~ "A", 
                                 Groups == 2 ~ "B",
                                 Groups == 3 ~ "C",
                                 Groups == 4 ~ "D",
                                 Groups == 5 ~ "E", 
                                 Groups == 6 ~ "F"))


inset_map1 <- ggplot() +
  geom_raster(aes(x = exp1.df$x, y = exp1.df$y, fill = exp1.df$group_alpha)) +
  geom_sf(data = exp1_nf, fill = NA, color = "black", linewidth = 1.1) +
  #scale_fill_met_d("Hokusai3") +
  scale_fill_manual(values = c("#95c36e", "#74c8c3", "#5a97c1", "#295384", "#0a2e57")) +
  labs(title = "Sequoia NF",
       fill = "Archetypes") +
  theme_bw() + 
  theme(#text = element_text(size = 20),
    legend.position = "right",
    legend.text = element_text(size = 10),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
inset_map1

inset_map2 <- ggplot() +
  geom_raster(aes(x = exp2.df$x, y = exp2.df$y, fill = exp2.df$group_alpha)) +
  geom_sf(data = exp2_nf, fill = NA, color = "black", linewidth = 1.1) +
  #scale_fill_met_d("Hokusai3") +
  scale_fill_manual(values = c("#5a97c1", "#0a2e57")) +
  labs(title = "Kaibab NF",
       fill = "Archetypes") +
  theme_bw() + 
  theme(#text = element_text(size = 20),
    legend.position = "right",
    legend.text = element_text(size = 10),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
inset_map2

main_map <- ggplot() +
  geom_raster(aes(x = wusa.df$x, y = wusa.df$y, fill = wusa.df$group_alpha)) +
  geom_sf(data = wusa_nf, fill = NA, color = "black") +
  geom_rect(aes(xmin = st_bbox(exp1_buff)[[1]], ymin = st_bbox(exp1_buff)[[2]], xmax = st_bbox(exp1_buff)[[3]], ymax = st_bbox(exp1_buff)[[4]]),
            fill = NA, color = "red", linewidth = 0.6) +
  geom_rect(aes(xmin = st_bbox(exp2_buff)[[1]], ymin = st_bbox(exp2_buff)[[2]], xmax = st_bbox(exp2_buff)[[3]], ymax = st_bbox(exp2_buff)[[4]]),
            fill = NA, color = "red", linewidth = 0.6) +
  scale_fill_met_d("Hokusai3") +
  labs(fill = "Archetypes") +
  theme_bw() + 
  theme(#text = element_text(size = 20),
    legend.position = "none",
    #legend.text = element_text(size = 10),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    axis.text = element_text(size = 8),
    plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
main_map

## map with insets

main_map_insets <- main_map %>%
  ggdraw() +
  draw_plot(
    {inset_map1 + 
        coord_sf(
          xlim = sf::st_bbox(exp1_buff)[c(1,3)],
          ylim = sf::st_bbox(exp1_buff)[c(2,4)],
          expand = FALSE) +
        theme(legend.position = "none",
              title = element_text(size = 8), 
              axis.ticks = element_blank())}, 
    x = 0.00, y = 0.45, width = 0.3, height = 0.3) +
  draw_plot(
    {inset_map2 + 
        coord_sf(
          xlim = sf::st_bbox(exp2_buff)[c(1,3)],
          ylim = sf::st_bbox(exp2_buff)[c(2,4)],
          expand = FALSE) +
        theme(legend.position = "none",
              title = element_text(size = 8), 
              axis.ticks = element_blank())},
    x = 0.6, y = 0.17, width = 0.3, height = 0.3) 
main_map_insets
ggsave(paste0("/Users/katiemurenbeeld/Desktop/HES_seminar/diversity_exp_maps_", Sys.Date(), ".png"), 
       plot = main_map_insets, width = 8, height = 8, dpi = 300)

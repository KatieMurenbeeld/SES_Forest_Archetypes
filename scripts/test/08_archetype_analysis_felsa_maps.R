library(sf)
library(raster)
library(terra)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(exactextractr)
library(tidyverse)
library(scales)
#library(ggsn)

# load the attribute data
rst_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")
rst <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")

# load the fuzzy elsa rasters
fuzzy_elsa_all <- rast(here::here("outputs/SGFCM_k6_felsa_2024-10-22.tif"))
fuzzy_elsa_eco <- rast(here::here("outputs/SGFCM_eco_felsa_2024-10-22.tif"))
fuzzy_elsa_soc <- rast(here::here("outputs/SGFCM_soc_k3_felsa_2024-10-22.tif"))

# Load the USFS boundaries
fs_nf <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeForest.shp")
fs_reg <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeRegion.shp")

projection <- "epsg: 5070"

fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_nf.crop <- st_crop(fs_nf.proj, ext(rst_sc))
fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.crop <- st_crop(fs_reg.proj, ext(rst_sc))

# for each forest calculate the average Fuzzy ELSA 
# first create buffers around each forest
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
write_sf(nf_buffers, here::here(paste0("data/processed/nf_buffers_50k_", 
                                       Sys.Date(), 
                                       ".shp")))

# get the average felsa score for each forest
nf_calc_felsa <- function(area_to_calc, felsa_rast){
  l <- nrow(area_to_calc)
  felsa_means <- data.frame()
  for (nf in 1:l) {
    tmp_nf <- area_to_calc %>%
      filter(FORESTORGC == area_to_calc$FORESTORGC[nf])
    e <- terra::extract(felsa_rast, tmp_nf, fun=mean, na.rm = TRUE)
    e$ID <- tmp_nf$FORESTORGC
    felsa_means <- rbind(felsa_means, e)
  }
  return(felsa_means)
} 

felsa_mean_all <- nf_calc_felsa(nf_buffers, fuzzy_elsa_all) 
felsa_mean_all <- rename(felsa_mean_all, c(FORESTORGC = ID,
                                           felsa_all = aip))

felsa_mean_soc <- nf_calc_felsa(nf_buffers, fuzzy_elsa_soc) 
felsa_mean_soc <- rename(felsa_mean_soc, c(FORESTORGC = ID,
                                           felsa_soc = aip))

felsa_mean_eco <- nf_calc_felsa(nf_buffers, fuzzy_elsa_eco)
felsa_mean_eco <- rename(felsa_mean_eco, c(FORESTORGC = ID,
                                           felsa_eco = forproc))

# join the calculated fuzzy elsa averages to the NF shapes
felsa_sf <- left_join(felsa_mean_all, fs_nf.crop, by = "FORESTORGC") %>%
  dplyr::select(FORESTORGC, felsa_all, geometry)
felsa_sf <- left_join(felsa_sf, felsa_mean_eco, by = "FORESTORGC")
felsa_sf <- left_join(felsa_sf, felsa_mean_soc, by = "FORESTORGC")

felsa_sf <- st_as_sf(felsa_sf)
write_sf(felsa_sf, here::here(paste0("data/processed/felsa_nf_", 
                                     Sys.Date(), ".shp")), overwrite = TRUE)

# get the average felsa score for each region
reg_calc_felsa <- function(area_to_calc, felsa_rast){
  l <- nrow(area_to_calc)
  felsa_means <- data.frame()
  for (nf in 1:l) {
    tmp_nf <- area_to_calc %>%
      filter(REGION == area_to_calc$REGION[nf])
    e <- terra::extract(felsa_rast, tmp_nf, fun=mean, na.rm = TRUE)
    e$ID <- tmp_nf$REGION
    felsa_means <- rbind(felsa_means, e)
  }
  return(felsa_means)
} 

felsa_mean_all_reg <- reg_calc_felsa(fs_reg.proj, fuzzy_elsa_all)
felsa_mean_all_reg <- rename(felsa_mean_all_reg, c(REGION = ID,
                                                   felsa_all = aip))

felsa_mean_soc_reg <- reg_calc_felsa(fs_reg.proj, fuzzy_elsa_soc) 
felsa_mean_soc_reg <- rename(felsa_mean_soc_reg, c(REGION = ID,
                                                   felsa_soc = aip))

felsa_mean_eco_reg <- reg_calc_felsa(fs_reg.proj, fuzzy_elsa_eco)
felsa_mean_eco_reg <- rename(felsa_mean_eco_reg, c(REGION = ID,
                                                   felsa_eco = forproc))

# join the calculated fuzzy elsa averages to the NF shapes
felsa_reg_sf <- left_join(felsa_mean_all_reg, fs_reg.crop, by = "REGION") %>%
  dplyr::select(REGION, felsa_all, geometry)
felsa_reg_sf <- left_join(felsa_reg_sf, felsa_mean_eco_reg, by = "REGION")
felsa_reg_sf <- left_join(felsa_reg_sf, felsa_mean_soc_reg, by = "REGION")

felsa_reg_sf <- st_as_sf(felsa_reg_sf)

write_sf(felsa_reg_sf, here::here(paste0("data/processed/felsa_reg_", 
                                         Sys.Date(), ".shp")), overwrite = TRUE)

## Make Maps
felsa_all_df <- fuzzy_elsa_all$aip %>% as.data.frame(xy = TRUE)
felsa_soc_df <- fuzzy_elsa_soc$aip %>% as.data.frame(xy = TRUE)
felsa_eco_df <- fuzzy_elsa_eco$forproc %>% as.data.frame(xy = TRUE)

## Map with NF boundaries and all attributes
felsa_all_nf_map <- ggplot() +
  geom_raster(aes(x = felsa_all_df$x, y = felsa_all_df$y, fill = felsa_all_df$aip)) +
  geom_sf(data = fs_nf.crop, fill = NA, color = "black") +
  #geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  labs(title = "Fuzzy ELSA: All Attributes") +
  scale_fill_gradient2(
    low = "white", 
    mid = "lightgrey", 
    high = "blue4", 
    midpoint = median(felsa_all_df$aip)
  ) +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
ggsave(here::here(paste0("outputs/plots/felsa_all_nf_", Sys.Date(), ".png")), 
       felsa_all_nf_map, height = 5, width = 7, dpi = 300)
#felsa_all_nf_map

## Map with NF boundaries and social attributes
felsa_soc_nf_map <- ggplot() +
  geom_raster(aes(x = felsa_soc_df$x, y = felsa_soc_df$y, fill = felsa_soc_df$aip)) +
  geom_sf(data = fs_nf.crop, fill = NA, color = "black") +
  #geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  labs(title = "Fuzzy ELSA: Social Attributes") +
  scale_fill_gradient2(
    low = "white", 
    mid = "lightgrey", 
    high = "blue4", 
    midpoint = median(felsa_soc_df$aip)
  ) +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
ggsave(here::here(paste0("outputs/plots/felsa_soc_nf_", Sys.Date(), ".png")), 
       felsa_soc_nf_map, height = 5, width = 7, dpi = 300)
#felsa_soc_nf_map

## Map with NF boundaries and ecological attributes
felsa_eco_nf_map <- ggplot() +
  geom_raster(aes(x = felsa_eco_df$x, y = felsa_eco_df$y, fill = felsa_eco_df$forproc)) +
  geom_sf(data = fs_nf.crop, fill = NA, color = "black") +
  #geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  labs(title = "Fuzzy ELSA: Ecological Attributes") +
  scale_fill_gradient2(
    low = "white", 
    mid = "lightgrey", 
    high = "blue4", 
    midpoint = median(felsa_eco_df$forproc)
  ) +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
ggsave(here::here(paste0("outputs/plots/felsa_eco_nf_", Sys.Date(), ".png")), 
       felsa_eco_nf_map, height = 5, width = 7, dpi = 300)
#felsa_eco_nf_map

# Create maps or scatter plots of Fuzzy ELSA values

## FELSA All
mean_felsa_all_nf <- ggplot() +
  #geom_sf(data = fs_nf.crop, fill = NA, color = "black", linewidth = 0.75) +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = felsa_sf, aes(fill = felsa_all, color = NULL)) +
  labs(title = "Fuzzy ELSA of SE Archetypes",
       subtitle = "Calculated from 50km buffer around National Forests") +
  theme_bw() + 
  theme(text = element_text(size = 16),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

#mean_felsa_all_nf
ggsave(here::here(paste0("outputs/plots/felsa_all_nf_mean_", Sys.Date(), ".png")),
       plot = mean_felsa_all_nf, width = 7, height = 5, dpi = 300)  

## FELSA Eco
mean_felsa_eco_nf <- ggplot() +
  #geom_sf(data = fs_nf.crop, fill = NA, color = "black", linewidth = 0.75) +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = felsa_sf, aes(fill = felsa_eco, color = NULL)) +
  labs(title = "Fuzzy ELSA of Eco Archetypes",
       subtitle = "Calculated from 50km buffer around National Forests") +
  theme_bw() + 
  theme(text = element_text(size = 16),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

#mean_felsa_all_nf
ggsave(here::here(paste0("outputs/plots/felsa_eco_nf_mean_", Sys.Date(), ".png")),
       plot = mean_felsa_eco_nf, width = 7, height = 5, dpi = 300)  

## FELSA Soc
mean_felsa_soc_nf <- ggplot() +
  #geom_sf(data = fs_nf.crop, fill = NA, color = "black", linewidth = 0.75) +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = felsa_sf, aes(fill = felsa_soc, color = NULL)) +
  labs(title = "Fuzzy ELSA of Social Archetypes",
       subtitle = "Calculated from 50km buffer around National Forests") +
  theme_bw() + 
  theme(text = element_text(size = 16),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

#mean_felsa_all_nf
ggsave(here::here(paste0("outputs/plots/felsa_soc_nf_mean_", Sys.Date(), ".png")),
       plot = mean_felsa_soc_nf, width = 7, height = 5, dpi = 300)  


# scatter plot
test_scatter <- felsa_sf %>%
  ggplot(aes(x=felsa_eco, y=felsa_soc, group=FORESTORGC, color=FORESTORGC)) +
  geom_point() + 
  #scale_x_continuous(breaks = seq(10, 110, by = 10)) +
  theme_bw() +
  theme(legend.position="none")
test_scatter
#ggsave(here::here("figures/test_felsa_scatter_2024-09-04.png"), test_scatter, 
#       width = 6, height = 4, dpi = 300)

# use scaled values?
test_scatter_zsc <- felsa_sf %>%
  #ggplot(aes(x=scale(felsa_eco), y=scale(felsa_soc), group=FORESTORGC, color=FORESTORGC)) +
  ggplot(aes(x=scale(felsa_eco), y=scale(felsa_soc))) +
  geom_point(color = "green4") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  #scale_x_continuous(breaks = seq(10, 110, by = 10)) +
  theme_bw() +
  theme(legend.position="none")
test_scatter_zsc
ggsave(here::here(paste0("outputs/plots/test_felsa_scatter_zsc_", Sys.Date(), ".png")),
       plot = test_scatter_zsc, width = 5, height = 5, dpi = 300)
## I should try to color the forests by their dominant archetype? 

# normalized between -1 and 1?
felsa_sf_norm <- felsa_sf %>% 
  mutate(felsa_eco_norm = rescale(felsa_eco, to = c(-1, 1)),
         felsa_soc_norm = rescale(felsa_soc, to = c(-1, 1)))

test_scatter_norm <- felsa_sf_norm %>%
  ggplot(aes(x= felsa_eco_norm, y= felsa_soc_norm, group=FORESTORGC, color=FORESTORGC)) +
  geom_point() + 
  #scale_x_continuous(breaks = seq(10, 110, by = 10)) +
  theme_bw() +
  theme(legend.position="none")
test_scatter_norm

# rescale between 0 and 1?
felsa_sf_sc <- felsa_sf %>% 
  mutate(felsa_eco_sc = (felsa_eco - global(felsa_eco, "min", na.rm=TRUE)[,1])/(global(felsa_eco, "max", na.rm=TRUE)[,1] - global(felsa_eco, "min", na.rm=TRUE)[,1]),
         felsa_soc_norm = (felsa_soc - global(felsa_soc, "min", na.rm=TRUE)[,1])/(global(felsa_soc, "max", na.rm=TRUE)[,1] - global(felsa_soc, "min", na.rm=TRUE)[,1]))
#eco_rast_stack_sc <- (eco_rast_stack - global(eco_rast_stack, "min", na.rm=TRUE)[,1])/(global(eco_rast_stack, "max", na.rm=TRUE)[,1] - global(eco_rast_stack, "min", na.rm=TRUE)[,1])
felsa_sf_sc <- felsa_sf %>% 
  mutate(felsa_eco_sc = rescale(felsa_eco, to = c(0, 1)),
         felsa_soc_sc = rescale(felsa_soc, to = c(0, 1)))

test_scatter_sc <- felsa_sf_sc %>%
  ggplot(aes(x= felsa_eco_sc, y= felsa_soc_sc, group=FORESTORGC, color=FORESTORGC)) +
  geom_point() + 
  #scale_x_continuous(breaks = seq(10, 110, by = 10)) +
  theme_bw() +
  theme(legend.position="none")
test_scatter_sc


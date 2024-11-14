library(sf)
library(raster)
library(terra)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(exactextractr)
library(tidyverse)
library(scales)
library(stringr)
library(cowplot)
library(viridis)
#library(ggsn)

# load the attribute data
rst_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")
rst <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")

# load the entropy rasters
ent_all <- rast(here::here("outputs/SGFCM_all_k6_entropy_2024-11-01.tif"))
ent_eco_k3 <- rast(here::here("outputs/SGFCM_eco_k3_entropy_2024-11-01.tif"))
ent_eco_k6 <- rast(here::here("outputs/SGFCM_eco_k6_entropy_2024-11-01.tif"))
ent_soc_k3 <- rast(here::here("outputs/SGFCM_soc_k3_entropy_2024-11-01.tif"))
ent_soc_k6 <- rast(here::here("outputs/SGFCM_soc_k6_entropy_2024-11-01.tif"))

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

# read in the 50km buffer shape
nf_buffers <- read_sf(here::here("data/processed/nf_buffers_50k_2024-10-22.shp"))

# get the average entropy score for each forest
nf_calc_ent <- function(area_to_calc, ent_rast){
  l <- nrow(area_to_calc)
  ent_means <- data.frame()
  for (nf in 1:l) {
    tmp_nf <- area_to_calc %>%
      filter(FORESTORGC == area_to_calc$FORESTORGC[nf])
    e <- terra::extract(ent_rast, tmp_nf, fun=mean, na.rm = TRUE)
    e$ID <- tmp_nf$FORESTORGC
    ent_means <- rbind(ent_means, e)
  }
  return(ent_means)
} 

ent_mean_all <- nf_calc_ent(nf_buffers, ent_all) 
ent_mean_all <- rename(ent_mean_all, c(FORESTORGC = ID,
                                           ent_all = all_ent_conus))

ent_mean_eco_k3 <- nf_calc_ent(nf_buffers, ent_eco_k3) 
ent_mean_eco_k3 <- rename(ent_mean_eco_k3, c(FORESTORGC = ID,
                                           ent_k3_eco = all_ent_conus))

ent_mean_eco_k6 <- nf_calc_ent(nf_buffers, ent_eco_k6) 
ent_mean_eco_k6 <- rename(ent_mean_eco_k6, c(FORESTORGC = ID,
                                             ent_k6_eco = all_ent_conus))

ent_mean_soc_k3 <- nf_calc_ent(nf_buffers, ent_soc_k3)
ent_mean_soc_k3 <- rename(ent_mean_soc_k3, c(FORESTORGC = ID,
                                           ent_k3_soc = all_ent_conus))

ent_mean_soc_k6 <- nf_calc_ent(nf_buffers, ent_soc_k6)
ent_mean_soc_k6 <- rename(ent_mean_soc_k6, c(FORESTORGC = ID,
                                             ent_k6_soc = all_ent_conus))

# join the calculated fuzzy elsa averages to the NF shapes
ent_sf <- left_join(ent_mean_all, fs_nf.crop, by = "FORESTORGC") %>%
  dplyr::select(FORESTORGC, ent_all, geometry)
ent_sf <- left_join(ent_sf, ent_mean_eco_k3, by = "FORESTORGC")
ent_sf <- left_join(ent_sf, ent_mean_eco_k6, by = "FORESTORGC")
ent_sf <- left_join(ent_sf, ent_mean_soc_k3, by = "FORESTORGC")
ent_sf <- left_join(ent_sf, ent_mean_soc_k6, by = "FORESTORGC") %>%
  dplyr::select(FORESTORGC, ent_all, ent_k3_eco, ent_k6_eco, 
                ent_k3_soc, ent_k6_soc, geometry)

ent_sf <- st_as_sf(ent_sf)
ent_df <- as.data.frame(st_drop_geometry(ent_sf))

#write_csv(ent_df, here::here(paste0("outputs/nf_entropy_", Sys.Date(), ".csv")))
#write_sf(ent_sf, here::here(paste0("data/processed/ent_nf_", 
#                                     Sys.Date(), ".shp")), overwrite = TRUE)
ent_df <- read_csv(here::here("outputs/nf_entropy_2024-11-01.csv"))
ent_sf <- read_sf(here::here("data/processed/ent_nf_2024-11-01.shp"))

# get the average felsa score for each region
reg_calc_ent <- function(area_to_calc, ent_rast){
  l <- nrow(area_to_calc)
  ent_means <- data.frame()
  for (nf in 1:l) {
    tmp_nf <- area_to_calc %>%
      filter(REGION == area_to_calc$REGION[nf])
    e <- terra::extract(ent_rast, tmp_nf, fun=mean, na.rm = TRUE)
    e$ID <- tmp_nf$REGION
    ent_means <- rbind(ent_means, e)
  }
  return(ent_means)
} 

ent_mean_all_reg <- reg_calc_ent(fs_reg.proj, ent_all)
ent_mean_all_reg <- rename(ent_mean_all_reg, c(REGION = ID,
                                               ent_all = all_ent_conus))

ent_mean_eco_reg_k3 <- reg_calc_ent(fs_reg.proj, ent_eco_k3)
ent_mean_eco_reg_k3 <- rename(ent_mean_eco_reg_k3, c(REGION = ID,
                                                     ent_k3_eco = all_ent_conus))

ent_mean_eco_reg_k6 <- reg_calc_ent(fs_reg.proj, ent_eco_k6)
ent_mean_eco_reg_k6 <- rename(ent_mean_eco_reg_k6, c(REGION = ID,
                                                     ent_k6_eco = all_ent_conus))

ent_mean_soc_reg_k3 <- reg_calc_ent(fs_reg.proj, ent_soc_k3)
ent_mean_soc_reg_k3 <- rename(ent_mean_soc_reg_k3, c(REGION = ID,
                                                     ent_k3_soc = all_ent_conus))

ent_mean_soc_reg_k6 <- reg_calc_ent(fs_reg.proj, ent_soc_k6)
ent_mean_soc_reg_k6 <- rename(ent_mean_soc_reg_k6, c(REGION = ID,
                                                     ent_k6_soc = all_ent_conus))

# join the calculated fuzzy elsa averages to the NF shapes
ent_reg_sf <- left_join(ent_mean_all_reg, fs_reg.crop, by = "REGION") %>%
  dplyr::select(REGION, ent_all, geometry)
ent_reg_sf <- left_join(ent_reg_sf, ent_mean_eco_reg_k3, by = "REGION")
ent_reg_sf <- left_join(ent_reg_sf, ent_mean_eco_reg_k6, by = "REGION")
ent_reg_sf <- left_join(ent_reg_sf, ent_mean_soc_reg_k3, by = "REGION")
ent_reg_sf <- left_join(ent_reg_sf, ent_mean_soc_reg_k6, by = "REGION") %>%
  dplyr::select(REGION, ent_all, ent_k3_eco, ent_k6_eco, 
                ent_k3_soc, ent_k6_soc, geometry)

ent_reg_sf <- st_as_sf(ent_reg_sf)
ent_reg_df <- as.data.frame(st_drop_geometry(ent_reg_sf))

#write_csv(ent_reg_df, here::here(paste0("outputs/reg_entropy_", Sys.Date(), ".csv")))
#write_sf(ent_reg_sf, here::here(paste0("data/processed/ent_reg_", 
#                                         Sys.Date(), ".shp")), overwrite = TRUE)

ent_reg_df <- read_csv(here::here("outputs/reg_entropy_2024-11-01.csv"))
ent_reg_sf <- read_sf(here::here("data/processed/ent_reg_2024-11-01.shp"))

## Make Maps
ent_all_df <- ent_all$all_ent_conus %>% as.data.frame(xy = TRUE)
ent_soc_df <- ent_soc_k6$all_ent_conus %>% as.data.frame(xy = TRUE)
ent_eco_df <- ent_eco_k6$all_ent_conus %>% as.data.frame(xy = TRUE)

## Map with NF and region boundaries and archetype entropy
ent_all_nf_map <- ggplot() +
  geom_raster(aes(x = ent_all_df$x, y = ent_all_df$y, fill = ent_all_df$all_ent_conus)) +
  geom_sf(data = fs_nf.crop, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  labs(title = "Entropy: All Attributes",
       fill = "Entropy") +
  scale_fill_viridis(limits = c(0, 1)) +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "right",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
ent_all_nf_map

ggsave(here::here(paste0("outputs/plots/ent_all_nf_", Sys.Date(), ".png")), 
       ent_all_nf_map, height = 5, width = 7, dpi = 300)


### Maps of "exceptional" forests
nf0118_shp <- nf_buffers %>%
  filter(FORESTORGC == "0118")
nf0621_shp <- nf_buffers %>%
  filter(FORESTORGC == "0621")
nf0903_shp <- nf_buffers %>%
  filter(FORESTORGC == "0903")
nf0909_shp <- nf_buffers %>%
  filter(FORESTORGC == "0909")

nf0118_ent <- ent_sf %>%
  filter(FORESTORGC == "0118")
nf0621_ent <- ent_sf %>%
  filter(FORESTORGC == "0621")
nf0903_ent <- ent_sf %>%
  filter(FORESTORGC == "0903")
nf0909_ent <- ent_sf %>%
  filter(FORESTORGC == "0909")

crs(ent_all) <- crs(nf0118_shp)
crs(ent_eco_k6) <- crs(nf0118_shp)
crs(ent_soc_k6) <- crs(nf0118_shp)

nf0118_ent_rst <- crop(ent_soc_k6, nf0118_shp, mask = TRUE)
nf0118_ent_rst_df <- nf0118_ent_rst$all_ent_conus %>% as.data.frame(xy = TRUE)

nf0118_ent_all_map <- ggplot() +
  geom_raster(aes(x = nf0118_ent_rst_df$x, y = nf0118_ent_rst_df$y, fill = nf0118_ent_rst_df$all_ent_conus)) +
  geom_sf(data = nf0118_ent, fill = NA, color = "black") +
  labs(title = "Dakota Grasslands Entropy",
       fill = "Entropy") +
  scale_fill_viridis(limits = c(0, 1)) +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "right",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
nf0118_ent_all_map


nf0621_ent_rst <- crop(ent_soc_k6, nf0621_shp, mask = TRUE)
nf0621_ent_rst_df <- nf0621_ent_rst$all_ent_conus %>% as.data.frame(xy = TRUE)

nf0621_ent_all_map <- ggplot() +
  geom_raster(aes(x = nf0621_ent_rst_df$x, y = nf0621_ent_rst_df$y, fill = nf0621_ent_rst_df$all_ent_conus)) +
  geom_sf(data = nf0621_ent, fill = NA, color = "black") +
  labs(title = "Colville NF Entropy",
       fill = "Entropy") +
  scale_fill_viridis(limits = c(0, 1)) +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "right",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
nf0621_ent_all_map

nf0903_ent_rst <- crop(ent_soc_k6, nf0903_shp, mask = TRUE)
nf0903_ent_rst_df <- nf0903_ent_rst$all_ent_conus %>% as.data.frame(xy = TRUE)

nf0903_ent_all_map <- ggplot() +
  geom_raster(aes(x = nf0903_ent_rst_df$x, y = nf0903_ent_rst_df$y, fill = nf0903_ent_rst_df$all_ent_conus)) +
  geom_sf(data = nf0903_ent, fill = NA, color = "black") +
  labs(title = "Chippewa NF Entropy",
       fill = "Entropy") +
  scale_fill_viridis(limits = c(0, 1)) +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "right",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
nf0903_ent_all_map

nf0909_ent_rst <- crop(ent_soc_k6, nf0909_shp, mask = TRUE)
nf0909_ent_rst_df <- nf0909_ent_rst$all_ent_conus %>% as.data.frame(xy = TRUE)

nf0909_ent_all_map <- ggplot() +
  geom_raster(aes(x = nf0909_ent_rst_df$x, y = nf0909_ent_rst_df$y, fill = nf0909_ent_rst_df$all_ent_conus)) +
  geom_sf(data = nf0909_ent, fill = NA, color = "black") +
  labs(title = "Superior NF Entropy",
       fill = "Entropy") +
  scale_fill_viridis(limits = c(0, 1)) +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "right",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
nf0909_ent_all_map

nf0612_ent_rst <- crop(ent_soc_k6, nf0612_shp, mask = TRUE)
nf0612_ent_rst_df <- nf0612_ent_rst$all_ent_conus %>% as.data.frame(xy = TRUE)

nf0612_ent_all_map <- ggplot() +
  geom_raster(aes(x = nf0612_ent_rst_df$x, y = nf0612_ent_rst_df$y, fill = nf0612_ent_rst_df$all_ent_conus)) +
  geom_sf(data = nf0612_ent, fill = NA, color = "black") +
  labs(title = "Siuslaw NF Entropy",
       fill = "Entropy") +
  scale_fill_viridis(limits = c(0, 1)) +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "right",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
nf0612_ent_all_map

# Create maps or scatter plots of Fuzzy ELSA values

## Entropy All
mean_ent_all_nf <- ggplot() +
  #geom_sf(data = fs_nf.crop, fill = NA, color = "black", linewidth = 0.75) +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = ent_sf, aes(fill = ent_all, color = NULL)) +
  labs(title = "Entropy of SE Archetypes: k = 6",
       subtitle = "Calculated from 50km buffer around National Forests", 
       fill = "Entropy") +
  scale_fill_viridis(limits = c(0, 1)) +
  theme_bw() + 
  theme(text = element_text(size = 16),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

#mean_ent_all_nf
ggsave(here::here(paste0("outputs/plots/ent_all_nf_mean_", Sys.Date(), ".png")),
       plot = mean_ent_all_nf, width = 7, height = 5, dpi = 300)  

mean_ent_all_reg <- ggplot() +
  #geom_sf(data = fs_nf.crop, fill = NA, color = "black", linewidth = 0.75) +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = ent_reg_sf, aes(fill = ent_all, color = NULL)) +
  labs(title = "Entropy of SE Archetypes: k = 6",
       subtitle = "Average for Regions", 
       fill = "Entropy") +
  scale_fill_viridis(limits = c(0, 1)) +
  theme_bw() + 
  theme(text = element_text(size = 16),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

#mean_ent_all_reg
ggsave(here::here(paste0("outputs/plots/ent_all_reg_mean_", Sys.Date(), ".png")),
       plot = mean_ent_all_reg, width = 7, height = 5, dpi = 300)  

# make map with insets
nf0118_shp <- nf_buffers %>%
  filter(FORESTORGC == "0118")
nf0621_shp <- nf_buffers %>%
  filter(FORESTORGC == "0621")
nf0903_shp <- nf_buffers %>%
  filter(FORESTORGC == "0903")
nf0909_shp <- nf_buffers %>%
  filter(FORESTORGC == "0909")
nf0612_shp <- nf_buffers %>%
  filter(FORESTORGC == "0612")

nf0118_ent <- ent_sf %>%
  filter(FORESTORGC == "0118")
nf0621_ent <- ent_sf %>%
  filter(FORESTORGC == "0621")
nf0903_ent <- ent_sf %>%
  filter(FORESTORGC == "0903")
nf0909_ent <- ent_sf %>%
  filter(FORESTORGC == "0909")
nf0612_ent <- ent_sf %>%
  filter(FORESTORGC == "0612")


mean_ent_all_nf0118 <- ggplot() +
  #geom_sf(data = fs_nf.crop, fill = NA, color = "black", linewidth = 0.75) +
  #geom_sf(data = nf0118_shp, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = nf0118_ent, aes(fill = ent_all, color = NULL))
mean_ent_all_nf0118

ggdraw() +
  draw_plot(mean_ent_all_nf) +
  draw_plot(mean_ent_all_nf0118,
            height = 0.2,
            x = -0.25,
            y = 0.08
  )

## Entropy Eco
mean_ent_eco_k3_nf <- ggplot() +
  #geom_sf(data = fs_nf.crop, fill = NA, color = "black", linewidth = 0.75) +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = ent_sf, aes(fill = ent_k3_eco, color = NULL)) +
  labs(title = "Entropy of Eco Archetypes: k = 3",
       subtitle = "Calculated from 50km buffer around National Forests") +
  scale_fill_viridis(limits = c(0, 1)) +
  theme_bw() + 
  theme(text = element_text(size = 16),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

#mean_ent_eco_k3_nf
ggsave(here::here(paste0("outputs/plots/ent_eco_k3_nf_mean_", Sys.Date(), ".png")),
       plot = mean_ent_eco_k3_nf, width = 7, height = 5, dpi = 300)  

mean_ent_eco_k6_nf <- ggplot() +
  #geom_sf(data = fs_nf.crop, fill = NA, color = "black", linewidth = 0.75) +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = ent_sf, aes(fill = ent_k6_eco, color = NULL)) +
  labs(title = "Entropy of Eco Archetypes: k = 6",
       subtitle = "Calculated from 50km buffer around National Forests") +
  scale_fill_viridis(limits = c(0, 1)) +
  theme_bw() + 
  theme(text = element_text(size = 16),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

#mean_ent_eco_k6_nf
ggsave(here::here(paste0("outputs/plots/ent_eco_k6_nf_mean_", Sys.Date(), ".png")),
       plot = mean_ent_eco_k6_nf, width = 7, height = 5, dpi = 300)  

## Entropy Soc
mean_ent_soc_k3_nf <- ggplot() +
  #geom_sf(data = fs_nf.crop, fill = NA, color = "black", linewidth = 0.75) +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = ent_sf, aes(fill = ent_k3_soc, color = NULL)) +
  labs(title = "Entropy of Soc Archetypes: k = 3",
       subtitle = "Calculated from 50km buffer around National Forests") +
  scale_fill_viridis(limits = c(0, 1)) +
  theme_bw() + 
  theme(text = element_text(size = 16),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

#mean_ent_soc_k3_nf
ggsave(here::here(paste0("outputs/plots/ent_soc_k3_nf_mean_", Sys.Date(), ".png")),
       plot = mean_ent_soc_k3_nf, width = 7, height = 5, dpi = 300)  

mean_ent_soc_k6_nf <- ggplot() +
  #geom_sf(data = fs_nf.crop, fill = NA, color = "black", linewidth = 0.75) +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = ent_sf, aes(fill = ent_k6_soc, color = NULL)) +
  labs(title = "Entropy of Soc Archetypes: k = 6",
       subtitle = "Calculated from 50km buffer around National Forests") +
  scale_fill_viridis(limits = c(0, 1)) +
  theme_bw() + 
  theme(text = element_text(size = 16),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

#mean_ent_soc_k6_nf
ggsave(here::here(paste0("outputs/plots/ent_soc_k6_nf_mean_", Sys.Date(), ".png")),
       plot = mean_ent_soc_k6_nf, width = 7, height = 5, dpi = 300)  


# scatter plot
# add a region column to the fesla_sf shapefile
ent_sf <- ent_sf %>%
  mutate(region = substr(FORESTORGC, 1, 2),
         ent_eco_sc = scale(ent_k6_eco),
         ent_soc_sc = scale(ent_k6_soc))

test_scatter_k3 <- ent_sf %>%
  ggplot(aes(x=ent_k3_eco, y=ent_k3_soc, group=region, color=region)) +
  geom_point() + 
  xlim(0, 1) +
  ylim(0, 1) +
  theme_bw() +
  theme(legend.position="bottom")
test_scatter_k3
#ggsave(here::here(paste0("outputs/plots/test_ent_k3_scatter_", Sys.Date(), ".png"), test_scatter_k3, 
#       width = 6, height = 4, dpi = 300)

test_scatter_k6 <- ent_sf %>%
  ggplot(aes(x=ent_k6_eco, y=ent_k6_soc, group=region, color=region)) +
  geom_point() +
  geom_hline(yintercept = 0.5) + 
  geom_vline(xintercept = 0.5) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_bw() +
  theme(legend.position="bottom")
test_scatter_k6
ggsave(here::here(paste0("outputs/plots/test_ent_k6_scatter_", Sys.Date(), ".png")), test_scatter_k6, 
       width = 4, height = 4, dpi = 300)

test_scatter_sc <- ent_sf %>%
  ggplot(aes(x=ent_soc_sc, y=ent_eco_sc, group=region, color=region)) +
           geom_point()
test_scatter_sc


test_scatter <- ent_sf %>%
  ggplot(aes(x=ent_k6_soc, y=ent_all, group=region, color=region)) +
  geom_point() + 
  xlim(0, 1) +
  ylim(0, 1) +
  theme_bw() +
  theme(legend.position="bottom")
test_scatter

test_scatter <- ent_sf %>%
  ggplot(aes(x=ent_k6_eco, y=ent_all, group=region, color=region)) +
  geom_point() + 
  xlim(0, 1) +
  ylim(0, 1) +
  theme_bw() +
  theme(legend.position="bottom")
test_scatter








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

nf0118_shp <- nf_buffers %>%
  filter(FORESTORGC == "0118")
nf0621_shp <- nf_buffers %>%
  filter(FORESTORGC == "0621")
nf0903_shp <- nf_buffers %>%
  filter(FORESTORGC == "0903")
nf0909_shp <- nf_buffers %>%
  filter(FORESTORGC == "0909")



arch_soc_rst <- rast(SGFCM_soc_result_k6$rasters)
arch_eco_rst <- rast(SGFCM_eco_result_k6$rasters)
arch_all_rst <- rast(SGFCM_all_result_k6$rasters)

arch_all_rst_crop <- crop(arch_all_rst, nf0909_shp, mask = TRUE)
arch_eco_rst_crop <- crop(arch_eco_rst, nf0909_shp, mask = TRUE)
arch_soc_rst_crop <- crop(arch_soc_rst, nf0909_shp, mask = TRUE)

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

plot(arch_all_rst_crop$Groups)
#plot(arch_all_rst_crop$group1)
plot(all_ent_rst$all_ent)
plot(eco_ent_rst$eco_ent)
plot(soc_ent_rst$soc_ent)

#attach(mtcars)
par(mfrow=c(2,2))
plot(arch_all_rst_crop$Groups, main = "NF 0909: Crisp Archetype")
plot(all_ent_rst$all_ent, main = "Entropy: All Variables")
plot(eco_ent_rst$eco_ent, main = "Entropy: Eco Variables")
plot(soc_ent_rst$soc_ent, main = "Entropy: Soc Variables")

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

#----create a map of the NF + buffers with final archetype groups and regional boundaries-----

all_k6_nf_ent_map <- ggplot() +
  geom_raster(aes(x = all_ent_conus$x, y = all_ent_conus$y, fill = all_ent_conus$all_ent_conus)) +
  #geom_sf(data = fs_nf.crop, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  #scale_fill_brewer(palette = "Set2") +
  labs(title = "All Attributes:",
       subtitle = "k=6, m=1.9, alpha = 0.6, beta = 0.4, window = 7x7", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "mm"))

all_k6_nf_ent_map
#ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_all_k6_nf_ent_map_", Sys.Date(), ".png"), 
#       plot = all_k6_nf_ent_map, width = 12, height = 12, dpi = 300)

all_ent_conus_map <- ggplot() +
  geom_raster(aes(x = all_ent_conus$x, y = all_ent_conus$y, fill = all_ent_conus$all_ent_conus)) + 
  scale_fill_viridis(option = "C", limits = c(0, 1)) + 
  theme_bw() +
  theme(aspect.ratio = 1,
        text = element_text(size = 12),
        legend.position = "none",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "mm"))

all_ent_conus_map

eco_ent_conus_map <- ggplot() +
  geom_raster(aes(x = eco_ent_conus$x, y = eco_ent_conus$y, fill = eco_ent_conus$eco_ent_conus)) + 
  scale_fill_viridis(option = "C", limits = c(0, 1)) + 
  theme_bw() +
  theme(aspect.ratio = 1,
        text = element_text(size = 12),
        legend.position = "none",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "mm"))

eco_ent_conus_map

soc_ent_conus_map <- ggplot() +
  geom_raster(aes(x = soc_ent_conus$x, y = soc_ent_conus$y, fill = soc_ent_conus$soc_ent_conus)) + 
  scale_fill_viridis(option = "C", limits = c(0, 1)) + 
  theme_bw() +
  theme(aspect.ratio = 1, 
        text = element_text(size = 12),
        legend.position = "none",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "mm"))

soc_ent_conus_map

patch1 <- eco_ent_conus_map / soc_ent_conus_map
patch1

all_ent_conus_map | patch1

all_ent_conus_map + plot_spacer() | patch1 + plot_layout(widths = c(4, 1, 2), guides = "collect")

(eco_ent_conus_map | soc_ent_conus_map) /
  all_ent_conus_map + plot_layout(widths = c(3, 2))

all_ent_conus_map + eco_ent_conus_map + soc_ent_conus_map +
  plot_layout(widths = c(2, 2))

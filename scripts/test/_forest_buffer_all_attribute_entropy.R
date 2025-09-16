library(tidyverse)
library(terra)
library(raster)
library(sf)
library(ggplot2)
library(tmap)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(future)
library(spdep)
library(classInt)

#----Load the data----
#sgfcm_all_result_k3_mod <- readRDS(here::here("outputs/SGFCM_all_attr_k3_2024-10-15.rds"))
#sgfcm_all_result_k8_mod <- readRDS(here::here("outputs/SGFCM_all_attr_k8_2024-10-15.rds"))
# need to rerun the cluster to get model results
rst_sc <- rast("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/processed/nf_buffers_all_attributes_scaled_2025-08-15.tif")
rst <- rast("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/processed/nf_buffers_all_attributes_2025-08-15.tif")

# Format for use in geocmeans
dataset <- lapply(names(rst_sc), function(n){
  aband <- rst_sc[[n]]
  return(aband)
})
names(dataset) <- names(rst_sc)

#----Use sgfcm to create forest clusters
w1 <- matrix(1, nrow = 7, ncol = 7)

SGFCM_all_result_k3 <- SGFCMeans(dataset, k = 3, m = 1.2, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w1, alpha = 0.6, beta = 0.6,
                                 seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")




# Calculate Entropy
## k = 3
arch_all_rst <- rast(SGFCM_all_result_k3$rasters)
arch_all_rst_conus_belong <- subset(arch_all_rst, 1:3)
plot(arch_all_rst_conus_belong)

arch_all_rst_conus_df <- as.data.frame(arch_all_rst, xy = TRUE)

all_ent_conus <- calcUncertaintyIndex(as.matrix(as.data.frame(arch_all_rst_conus_belong)))

all_ent_conus <- cbind(arch_all_rst_conus_df, all_ent_conus)
all_ent_rst_conus <- rast(all_ent_conus)
plot(all_ent_rst_conus)
plot(all_ent_rst_conus$all_ent_conus)

#writeRaster(all_ent_rst_conus, here::here(paste0("outputs/nfbuffers_SGFCM_all_k3_entropy_", Sys.Date(), ".tif")))

#----Map the Entropy---------

# load the entropy raster

ent_rast <- rast(here::here("outputs/nfbuffers_SGFCM_all_k3_entropy_2025-08-15.tif"))

# read in the 50km buffer shape
nf_buffers_test #need to bring in the code or save the updated nf_buffers shape

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

ent_all <- ent_rast$all_ent_conus
ent_mean_all <- nf_calc_ent(nf_buffers_test, ent_all) 
ent_mean_all <- rename(ent_mean_all, c(FORESTORGC = ID,
                                       ent_all = all_ent_conus))

# join the calculated fuzzy elsa averages to the NF shapes
ent_sf <- left_join(ent_mean_all, nf_buffers_test, by = "FORESTORGC") %>%
  dplyr::select(FORESTORGC, ent_all, geometry)

ent_sf <- st_as_sf(ent_sf)
ent_df <- as.data.frame(st_drop_geometry(ent_sf))

#write_csv(ent_df, here::here(paste0("outputs/nf_entropy_", Sys.Date(), ".csv")))
write_sf(ent_sf, here::here(paste0("data/processed/nfbuffers_ent_nf_", 
                                     Sys.Date(), ".shp")), overwrite = TRUE)

## Make Maps
ent_all_df <- ent_all$all_ent_conus %>% as.data.frame(xy = TRUE)

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


## Map with NF and region boundaries and archetype entropy
ent_all_nf_map <- ggplot() +
  geom_raster(aes(x = ent_all_df$x, y = ent_all_df$y, fill = ent_all_df$all_ent_conus)) +
  geom_sf(data = fs_nf.crop, fill = NA, color = "black") +
 # geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
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

## Mean entropy for national forests
mean_ent_all_nf <- ggplot() +
#  geom_sf(data = fs_nf.crop, fill = NA, color = "black", linewidth = 0.75) +
#  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = ent_sf, aes(fill = ent_all, color = NULL)) +
  labs(title = "Entropy of Forest Archetypes: k = 3",
       subtitle = "Calculated from 50km buffer around National Forests", 
       fill = "Entropy") +
  scale_fill_viridis(limits = c(0, 1), alpha = 0.5) +
  theme_bw() + 
  theme(text = element_text(size = 16),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
mean_ent_all_nf

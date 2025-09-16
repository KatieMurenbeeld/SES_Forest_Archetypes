library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(exactextractr)
library(tigris)
library(viridis)

# Load the cluster results
sgfcm_result <- rast("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/outputs/nfbuffers_SGFCM_all_result_k3_2025-08-12.tif")

# Load the attribute data
rst_sc <- rast("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/processed/nf_buffers_all_attributes_scaled_2025-08-15.tif")
rst <- rast("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/processed/nf_buffers_all_attributes_2025-08-15.tif")

## Forest with 50km buffer shape file
nf_sf <- read_sf(here::here("data/processed/nf_buffers_50k_2024-10-22.shp"))

## Have to crop to CONUS. Load in regional USFS boundaries.
## Load the USFS boundaries
fs_reg <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeRegion.shp")

## Set the projection
projection <- "epsg: 5070"

## Filter out Region 10 and crop to the raster stack
fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.crop <- st_crop(fs_reg.proj, ext(rst_sc))

## create a new nation forest buffers shape using st_intersection
#nf_buffers <- st_intersection(nf_sf, fs_reg.crop)
nf_buffers_test <- st_crop(nf_sf, fs_reg.crop)

# calculate the shannon diversity assuming cripst archetypes for each forest 
buff <- nf_buffers_test

v <- buff %>% st_cast("MULTIPOLYGON")
z <- crop(sgfcm_result, v, mask = TRUE)

x <- exact_extract(z, v, coverage_area = TRUE)
names(x) <- v$FORESTORGC

areas <- bind_rows(x, .id = "FORESTORGC") %>%
  group_by(FORESTORGC, value) %>%
  summarize(total_arch_area = sum(coverage_area)) %>%
  group_by(FORESTORGC) %>%
  mutate(proportion_pct = round((total_arch_area/sum(total_arch_area))*100, 2)) %>%
  mutate(proportion = (total_arch_area/sum(total_arch_area)))

areas <- areas %>% 
  replace_na(list(value = 0))

shan_h <- areas %>%
  dplyr::select(FORESTORGC, proportion) %>%
  group_by(FORESTORGC) %>%
  summarise(shan_div = -sum(proportion * log(proportion)),
            shan_div_sc = -sum(proportion * log(proportion))/log(6))

# join to forest boundary shapefile
shan_h_sf <- shan_h %>%
  left_join(nf_buffers_test, by = "FORESTORGC")
shan_h_sf <- st_as_sf(shan_h_sf)

# save the shape file
write_sf(shan_h_sf, here::here(paste0("data/processed/nfbuffers_shan_h_nf_", 
                                      Sys.Date(), ".shp")), overwrite = TRUE)


# make map
shan_nf <- ggplot() +
  geom_sf(data = nf_buffers_test, fill = NA, color = "black", linewidth = 0.75) +
  geom_sf(data = shan_h_sf, aes(fill = shan_div_sc)) +
  labs(title = "Shannon Entropy (H) of Archetypes",
       subtitle = "Calculated from 50km buffer around National Forests") +
  scale_fill_viridis(limits = c(0, 1), alpha = 0.5) +
  theme_bw() + 
  theme(text = element_text(size = 20),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
shan_nf
ggsave(here::here(paste0("outputs/plots/nfbuffers_shan_div_all_nf_", Sys.Date(), ".png")),
       plot = shan_nf, width = 7, height = 5, dpi = 300)
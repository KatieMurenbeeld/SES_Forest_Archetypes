library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(exactextractr)
library(tigris)

# Load the cluster results
sgfcm_all_k6_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_all_result_k6_2024-10-15.tif")

# Load the attribute data
rst_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")
rst <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")

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

# calculate the shannon diversity assuming cripst archetypes for each forest 
buff <- nf_buffers

v <- buff %>% st_cast("MULTIPOLYGON")
z <- crop(sgfcm_all_k6_result, v, mask = TRUE)

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
  left_join(fs_nf.crop, by = "FORESTORGC")
shan_h_sf <- st_as_sf(shan_h_sf)

# save the shape file
write_sf(shan_h_sf, here::here(paste0("data/processed/shan_h_nf_", 
                                      Sys.Date(), ".shp")), overwrite = TRUE)

# make map
shan_nf <- ggplot() +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 0.75) +
  geom_sf(data = shan_h_sf, aes(fill = shan_div_sc)) +
  labs(title = "Shannon Entropy (H) of Archetypes",
       subtitle = "Calculated from 50km buffer around National Forests") +
  scale_fill_viridis(limits = c(0, 1)) +
  theme_bw() + 
  theme(text = element_text(size = 20),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
#shan_nf
ggsave(here::here(paste0("outputs/plots/shan_div_all_nf_", Sys.Date(), ".png")),
       plot = shan_nf, width = 7, height = 5, dpi = 300)

# calculate the shannon diversity assuming cripst archetypes for each Region 
z_reg <- crop(sgfcm_all_k6_result, fs_reg.crop, mask = TRUE)

x_reg <- exact_extract(z_reg, fs_reg.crop, coverage_area = TRUE)
names(x_reg) <- fs_reg.crop$REGION

areas_reg <- bind_rows(x_reg, .id = "REGION") %>%
  group_by(REGION, value) %>%
  summarize(total_arch_area = sum(coverage_area)) %>%
  group_by(REGION) %>%
  mutate(proportion_pct = round((total_arch_area/sum(total_arch_area))*100, 2)) %>%
  mutate(proportion = (total_arch_area/sum(total_arch_area)))

areas_reg <- areas_reg %>% 
  replace_na(list(value = 0))

shan_h_reg <- areas_reg %>%
  dplyr::select(REGION, proportion) %>%
  group_by(REGION) %>%
  summarise(shan_div = -sum(proportion * log(proportion)),
            shan_div_sc = -sum(proportion * log(proportion))/log(6))

# join to forest boundary shapefile
shan_h_reg_sf <- shan_h_reg %>%
  left_join(fs_reg.crop, by = "REGION")
shan_h_reg_sf <- st_as_sf(shan_h_reg_sf)

# save the shape file
write_sf(shan_h_reg_sf, here::here(paste0("data/processed/shan_h_reg_", 
                                      Sys.Date(), ".shp")), overwrite = TRUE)

# make map
shan_reg <- ggplot() +
  geom_sf(data = shan_h_reg_sf, aes(fill = shan_div_sc)) +
  labs(title = "Shannon Entropy (H) of Archetypes",
       subtitle = "Calculated for each Region") +
  scale_fill_viridis(limits = c(0, 1)) +
  theme_bw() + 
  theme(text = element_text(size = 20),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
#shan_reg
ggsave(here::here(paste0("outputs/plots/shan_div_all_reg_", Sys.Date(), ".png")),
       plot = shan_reg, width = 7, height = 5, dpi = 300)




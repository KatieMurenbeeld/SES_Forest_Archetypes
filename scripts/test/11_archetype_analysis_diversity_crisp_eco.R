library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(exactextractr)
library(tigris)

# Load the cluster results
sgfcm_eco_k3_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_eco_result_2024-10-21.tif")
sgfcm_eco_k6_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_eco_result_k6_2024-11-06.tif")

# Load the attribute data - I don't think I need this here
#rst_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")
#rst <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")

# Load the USFS boundaries
fs_nf <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeForest.shp")
fs_reg <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeRegion.shp")

projection <- "epsg: 5070"

fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_nf.crop <- st_crop(fs_nf.proj, ext(sgfcm_eco_k6_result))
fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.crop <- st_crop(fs_reg.proj, ext(sgfcm_eco_k6_result))

# read in the 50km buffer shape

nf_buffers <- read_sf(here::here("data/processed/nf_buffers_50k_2024-10-22.shp"))

# calculate the shannon diversity assuming cripst archetypes for each forest 
buff <- nf_buffers

v <- buff %>% st_cast("MULTIPOLYGON")
z_k3 <- crop(sgfcm_eco_k3_result, v, mask = TRUE)

x_k3 <- exact_extract(z_k3, v, coverage_area = TRUE)
names(x_k3) <- v$FORESTORGC

areas_k3 <- bind_rows(x_k3, .id = "FORESTORGC") %>%
  group_by(FORESTORGC, value) %>%
  summarize(total_arch_area = sum(coverage_area)) %>%
  group_by(FORESTORGC) %>%
  mutate(proportion_pct = round((total_arch_area/sum(total_arch_area))*100, 2)) %>%
  mutate(proportion = (total_arch_area/sum(total_arch_area)))

areas_k3 <- areas_k3 %>% 
  replace_na(list(value = 0))

shan_h_k3 <- areas_k3 %>%
  dplyr::select(FORESTORGC, proportion) %>%
  group_by(FORESTORGC) %>%
  summarise(shan_div_k3 = -sum(proportion * log(proportion)),
            shan_div_sc_k3 = -sum(proportion * log(proportion))/log(3))

# repeat for k = 6
z_k6 <- crop(sgfcm_eco_k6_result, v, mask = TRUE)

x_k6 <- exact_extract(z_k6, v, coverage_area = TRUE)
names(x_k6) <- v$FORESTORGC

areas_k6 <- bind_rows(x_k6, .id = "FORESTORGC") %>%
  group_by(FORESTORGC, value) %>%
  summarize(total_arch_area = sum(coverage_area)) %>%
  group_by(FORESTORGC) %>%
  mutate(proportion_pct = round((total_arch_area/sum(total_arch_area))*100, 2)) %>%
  mutate(proportion = (total_arch_area/sum(total_arch_area)))

areas_k6 <- areas_k6 %>% 
  replace_na(list(value = 0))

shan_h_k6 <- areas_k6 %>%
  dplyr::select(FORESTORGC, proportion) %>%
  group_by(FORESTORGC) %>%
  summarise(shan_div_k6 = -sum(proportion * log(proportion)),
            shan_div_sc_k6 = -sum(proportion * log(proportion))/log(6))

# join to forest boundary shapefile
shan_h_eco_sf <- shan_h_k3 %>%
  left_join(fs_nf.crop, by = "FORESTORGC")
shan_h_eco_sf <- left_join(shan_h_eco_sf, shan_h_k6, by = "FORESTORGC")
shan_h_eco_sf <- st_as_sf(shan_h_eco_sf)

# save the shape file
write_sf(shan_h_eco_sf, here::here(paste0("data/processed/shan_h_eco_nf_", 
                                      Sys.Date(), ".shp")), overwrite = TRUE)

# make maps
shan_eco_k3_nf <- ggplot() +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 0.75) +
  geom_sf(data = shan_h_eco_sf, aes(fill = shan_div_sc_k3)) +
  labs(title = "Shannon Entropy (H) of Eco Archetypes (k = 3)",
       subtitle = "Calculated from 50km buffer around National Forests") +
  scale_fill_viridis(limits = c(0, 1)) +
  theme_bw() + 
  theme(text = element_text(size = 20),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
#shan_eco_k3_nf
ggsave(here::here(paste0("outputs/plots/shan_div_eco_k3_nf_", Sys.Date(), ".png")),
       plot = shan_eco_k3_nf, width = 7, height = 5, dpi = 300)

shan_eco_k6_nf <- ggplot() +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 0.75) +
  geom_sf(data = shan_h_eco_sf, aes(fill = shan_div_sc_k6)) +
  labs(title = "Shannon Entropy (H) of Eco Archetypes (k = 6)",
       subtitle = "Calculated from 50km buffer around National Forests") +
  scale_fill_viridis(limits = c(0, 1)) +
  theme_bw() + 
  theme(text = element_text(size = 20),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
#shan_eco_k6_nf
ggsave(here::here(paste0("outputs/plots/shan_div_eco_k6_nf_", Sys.Date(), ".png")),
       plot = shan_eco_k6_nf, width = 7, height = 5, dpi = 300)

# calculate the shannon diversity assuming crips archetypes for each Region 
z_reg_k3 <- crop(sgfcm_eco_k3_result, fs_reg.crop, mask = TRUE)

x_reg_k3 <- exact_extract(z_reg_k3, fs_reg.crop, coverage_area = TRUE)
names(x_reg_k3) <- fs_reg.crop$REGION

areas_reg_k3 <- bind_rows(x_reg_k3, .id = "REGION") %>%
  group_by(REGION, value) %>%
  summarize(total_arch_area = sum(coverage_area)) %>%
  group_by(REGION) %>%
  mutate(proportion_pct = round((total_arch_area/sum(total_arch_area))*100, 2)) %>%
  mutate(proportion = (total_arch_area/sum(total_arch_area)))

areas_reg_k3 <- areas_reg_k3 %>% 
  replace_na(list(value = 0))

shan_h_reg_k3 <- areas_reg_k3 %>%
  dplyr::select(REGION, proportion) %>%
  group_by(REGION) %>%
  summarise(shan_div_k3 = -sum(proportion * log(proportion)),
            shan_div_sc_k3 = -sum(proportion * log(proportion))/log(3))

# k = 6
z_reg_k6 <- crop(sgfcm_eco_k6_result, fs_reg.crop, mask = TRUE)

x_reg_k6 <- exact_extract(z_reg_k6, fs_reg.crop, coverage_area = TRUE)
names(x_reg_k6) <- fs_reg.crop$REGION

areas_reg_k6 <- bind_rows(x_reg_k6, .id = "REGION") %>%
  group_by(REGION, value) %>%
  summarize(total_arch_area = sum(coverage_area)) %>%
  group_by(REGION) %>%
  mutate(proportion_pct = round((total_arch_area/sum(total_arch_area))*100, 2)) %>%
  mutate(proportion = (total_arch_area/sum(total_arch_area)))

areas_reg_k6 <- areas_reg_k6 %>% 
  replace_na(list(value = 0))

shan_h_reg_k6 <- areas_reg_k6 %>%
  dplyr::select(REGION, proportion) %>%
  group_by(REGION) %>%
  summarise(shan_div_k6 = -sum(proportion * log(proportion)),
            shan_div_sc_k6 = -sum(proportion * log(proportion))/log(6))

# join to region boundary shapefile
shan_h_eco_reg_sf <- shan_h_reg_k3 %>%
  left_join(fs_reg.crop, by = "REGION")
shan_h_eco_reg_sf <- left_join(shan_h_eco_reg_sf, shan_h_reg_k6, by = "REGION")
shan_h_eco_reg_sf <- st_as_sf(shan_h_eco_reg_sf)

# save the shape file
write_sf(shan_h_eco_reg_sf, here::here(paste0("data/processed/shan_h_eco_reg_", 
                                      Sys.Date(), ".shp")), overwrite = TRUE)

# make maps
shan_reg_k3 <- ggplot() +
  geom_sf(data = shan_h_eco_reg_sf, aes(fill = shan_div_sc_k3)) +
  labs(title = "Shannon Entropy (H) of Eco Archetypes k = 3",
       subtitle = "Calculated for each Region") +
  scale_fill_viridis(limits = c(0, 1)) +
  theme_bw() + 
  theme(text = element_text(size = 20),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
#shan_reg_k3
ggsave(here::here(paste0("outputs/plots/shan_div_eco_reg_k3_", Sys.Date(), ".png")),
       plot = shan_reg_k3, width = 7, height = 5, dpi = 300)

# k = 6
shan_reg_k6 <- ggplot() +
  geom_sf(data = shan_h_eco_reg_sf, aes(fill = shan_div_sc_k6)) +
  labs(title = "Shannon Entropy (H) of Eco Archetypes k = 6",
       subtitle = "Calculated for each Region") +
  scale_fill_viridis(limits = c(0, 1)) +
  theme_bw() + 
  theme(text = element_text(size = 20),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
#shan_reg_k6
ggsave(here::here(paste0("outputs/plots/shan_div_eco_reg_k6_", Sys.Date(), ".png")),
       plot = shan_reg_k6, width = 7, height = 5, dpi = 300)




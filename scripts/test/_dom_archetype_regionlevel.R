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
library(MetBrewer)

# Load the data
sgfcm_all_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")
sgfcm_all_attri_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")
sgfcm_all_k6_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_all_result_k6_2024-10-15.tif")

# Load the USFS boundaries
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

# read in the national forest level shannon diversity shapefile and entropy shapefile 
shan_nf <- read_sf(here::here("data/processed/shan_h_nf_2024-11-05.shp"))
ent_nf <- read_sf(here::here("data/processed/ent_nf_2024-11-01.shp"))

# calculate the archetype areas assuming crisp archetypes for each forest 
reg <- fs_reg.crop

v <- reg %>% st_cast("MULTIPOLYGON")
z <- crop(sgfcm_all_k6_result, reg, mask = TRUE)

x <- exact_extract(z, v, coverage_area = TRUE)
names(x) <- v$REGION

areas <- bind_rows(x, .id = "REGION") %>%
  group_by(REGION, value) %>%
  summarize(total_arch_area = sum(coverage_area)) %>%
  group_by(REGION) %>%
  mutate(proportion_pct = round((total_arch_area/sum(total_arch_area))*100, 2)) %>%
  mutate(proportion = (total_arch_area/sum(total_arch_area)))

areas <- areas %>% 
  replace_na(list(value = 0)) %>%
  dplyr::select(-total_arch_area)

write_csv(areas, here::here(paste0("outputs/tables/dominant_arch_region_",
                                   Sys.Date(), ".csv")), append = FALSE)

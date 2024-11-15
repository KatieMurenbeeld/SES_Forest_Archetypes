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

# read in the 50km buffer shape

nf_buffers <- read_sf(here::here("data/processed/nf_buffers_50k_2024-10-22.shp"))

# crop the buffers to the outline of conus
reg_test <- fs_reg.crop %>%
  filter(REGION == "06")

nf_buff_test <- nf_buffers %>%
  filter(FORESTORGC == "0612")

test_int <- st_intersection(nf_buff_test, reg_test)
plot(test_int$geometry)

nf_buffers_int <- st_intersection(nf_buffers, fs_reg.crop)


# read in the national forest level shannon diversity shapefile and entropy shapefile 
shan_nf <- read_sf(here::here("data/processed/shan_h_nf_2024-11-05.shp"))
ent_nf <- read_sf(here::here("data/processed/ent_nf_2024-11-01.shp"))

# calculate the archetype areas assuming crisp archetypes for each forest 
buff <- nf_buffers_int

v <- buff %>% st_cast("MULTIPOLYGON")
z <- crop(sgfcm_all_k6_result, buff, mask = TRUE)

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

areas <- areas %>%
  group_by(FORESTORGC) %>%
  mutate(max_pct = max(proportion_pct)) %>%
  ungroup()

dom_arch_test <- areas %>%
  filter(max_pct == proportion_pct)

dom_arch <- left_join(shan_nf, dom_arch_test, by = c("FORESTO" = "FORESTORGC")) %>%
  st_drop_geometry()
dom_arch <- left_join(ent_nf, dom_arch, by = c("FORESTORGC" = "FORESTO"))

dom_arch_df <- dom_arch %>%
  dplyr::select(FORESTNA, FORESTORGC, REGION, value, max_pct, shan_dv, shn_dv_, ent_all, ent_k6_eco, ent_k6_soc) %>%
  st_drop_geometry() %>%
  mutate(ent_eco_sc = scale(ent_k6_eco)[1:109]) %>% 
  mutate(ent_soc_sc = scale(ent_k6_soc)[1:109]) %>%
  mutate(eco_to_soc = case_when(ent_eco_sc < 0 & ent_soc_sc < 0 ~ "low_eco_low_soc",
                                ent_eco_sc < 0 & ent_soc_sc > 0 ~ "low_eco_high_soc",
                                ent_eco_sc > 0 & ent_soc_sc < 0 ~ "high_eco_low_soc",
                                ent_eco_sc > 0 & ent_soc_sc > 0 ~ "high_eco_high_soc"))
  #%>% filter(value != 0)
names(dom_arch_df) <- c("forest_name", "forest_num", "region", "dom_archetype", "pct_area_dom_arch", "shan_diverse", "shan_diverse_norm", "entropy_all", "entropy_eco", "entropy_soc", "ent_eco_sc", "ent_soc_sc", "eco_to_soc")

write_csv(dom_arch_df, here::here(paste0("outputs/tables/nf_level_dominant_archetypes_uncertainty_", Sys.Date(), ".csv")))

test_scatter <- dom_arch_df %>%
  ggplot(aes(x=ent_soc_sc, y=ent_eco_sc, group=as.factor(value), color=as.factor(value))) +
  geom_point() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  xlim(-4, 4) +
  ylim(-4, 4) +
  scale_color_brewer(palette = "Set2") + 
  theme_bw() +
  theme(legend.position="bottom")
test_scatter

test_scatter <- dom_arch_df %>%
  ggplot(aes(x=scale(ent_k6_soc), y=scale(ent_k6_eco), group=REGION, color=max_pct >= 59.5)) +
  geom_point() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  xlim(-4, 4) +
  ylim(-4, 4) +
  scale_colour_manual(name = 'Dom arch > 60%', values = setNames(c('red','green'),c(T, F))) +
#  scale_color_brewer(palette = "Set2") + 
  theme_bw() +
  theme(legend.position="bottom")
test_scatter

soc_eco_ent_sc <- dom_arch_df %>%
  ggplot(aes(x=ent_soc_sc, y=ent_eco_sc, color=as.factor(value), size = max_pct, alpha = 0.25)) +
  geom_point() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  xlim(-3.75, 3.75) +
  ylim(-3.75, 3.75) +
  scale_color_brewer(palette = "Set2") + 
  theme_bw() +
  guides(size = "none") +
  guides(alpha = "none") +
  theme(legend.position="bottom")
soc_eco_ent_sc
ggsave(here::here(paste0("outputs/plots/soc_eco_ent_sc_scatter_", Sys.Date(), ".png")),
       soc_eco_ent_sc, height = 4, width = 4, dpi = 300)

                                                                                               

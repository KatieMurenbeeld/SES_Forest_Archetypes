# This is an important script to include on github. 
##This is a table that will be used for creating figure 5(3) see 16_arch....R
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

# read in the 50km buffer shape
# something is wrong with the buffers - they aren't cropped to conus
# get the intersection with the regional boundaries to "crop" to conus
# I was getting a bunch of NA for archetypes from water 
nf_buffers <- read_sf(here::here("data/processed/nf_buffers_50k_2024-10-22.shp"))

# crop the buffers to the outline of conus
reg_test <- fs_reg.crop %>%
  filter(REGION == "06")

# select a forest to check
nf_buff_test <- nf_buffers %>%
  filter(FORESTORGC == "0612")

# get the intersection with the region
test_int <- st_intersection(nf_buff_test, reg_test)
plot(test_int$geometry)

# create a new nation forest buffers shape
nf_buffers_int <- st_intersection(nf_buffers, fs_reg.crop)


# read in the national forest level shannon diversity shapefile and entropy shapefile 
shan_nf <- read_sf(here::here("data/processed/shan_h_nf_2024-11-05.shp"))
ent_nf <- read_sf(here::here("data/processed/ent_nf_2024-11-01.shp"))

# drop the shan div from shan_nf
shan_nf <- shan_nf %>%
  select(!shan_dv) %>%
  select(!shn_dv_)

# calculate the archetype areas assuming crisp archetypes for each forest 
buff <- nf_buffers_int

v <- buff %>% st_cast("MULTIPOLYGON")
z <- crop(sgfcm_all_k6_result, buff, mask = TRUE)

x <- exact_extract(z, v, coverage_area = TRUE)
names(x) <- v$FORESTORGC

areas_nf <- bind_rows(x, .id = "FORESTORGC") %>%
  group_by(FORESTORGC, value) %>%
  summarize(total_arch_area = sum(coverage_area)) %>%
  group_by(FORESTORGC) %>%
  mutate(proportion_pct = round((total_arch_area/sum(total_arch_area))*100, 2)) %>%
  mutate(proportion = (total_arch_area/sum(total_arch_area)))

areas_nf <- areas_nf %>% 
  replace_na(list(value = 0))

nf_arch_areas_for_table <- areas_nf %>%
  select(FORESTORGC, value, proportion_pct) %>%
  pivot_wider(names_from = value, values_from = proportion_pct)

# Write to csv
write_csv(nf_arch_areas_for_table, here::here(paste0("outputs/tables/table_b2_", Sys.Date(), ".csv")))

# recalculate shan diversity and normalized diversity
shan_h_int <- areas_nf %>%
  dplyr::select(FORESTORGC, proportion) %>%
  group_by(FORESTORGC) %>%
  summarise(shan_div = -sum(proportion * log(proportion)),
            shan_div_norm = -sum(proportion * log(proportion))/log(6))

areas <- areas_nf %>%
  group_by(FORESTORGC) %>%
  mutate(max_pct = max(proportion_pct)) %>%
  ungroup()

dom_arch_test <- areas %>%
  filter(max_pct == proportion_pct)

dom_arch <- left_join(shan_nf, shan_h_int, by = c("FORESTO" = "FORESTORGC")) %>%
  st_drop_geometry()
dom_arch <- left_join(dom_arch, dom_arch_test, by = c("FORESTO" = "FORESTORGC")) %>%
  st_drop_geometry()
dom_arch <- left_join(ent_nf, dom_arch, by = c("FORESTORGC" = "FORESTO")) %>%
  st_drop_geometry()

dom_arch_df <- dom_arch %>%
  dplyr::select(FORESTNA, FORESTORGC, REGION, value, max_pct, shan_div, shan_div_norm, ent_all, ent_k6_eco, ent_k6_soc) %>%
  #mutate(shan_div = shan_h_int$shan_div) %>%
  #mutate(shan_div_norm = shan_h_int$shan_div_norm) %>%
  mutate(ent_eco_sc = scale(ent_k6_eco)[1:109]) %>% 
  mutate(ent_soc_sc = scale(ent_k6_soc)[1:109]) %>%
  mutate(eco_to_soc = case_when(ent_eco_sc < 0 & ent_soc_sc < 0 ~ "low_eco_low_soc",
                                ent_eco_sc < 0 & ent_soc_sc > 0 ~ "low_eco_high_soc",
                                ent_eco_sc > 0 & ent_soc_sc < 0 ~ "high_eco_low_soc",
                                ent_eco_sc > 0 & ent_soc_sc > 0 ~ "high_eco_high_soc"))
  #%>% filter(value != 0)
names(dom_arch_df) <- c("forest_name", "forest_num", "region", "dom_archetype", "pct_area_dom_arch", "shan_diverse", "shan_diverse_norm", "entropy_all", "entropy_eco", "entropy_soc", "ent_eco_sc", "ent_soc_sc", "eco_to_soc")

write_csv(dom_arch_df, here::here(paste0("outputs/tables/nf_level_dominant_archetypes_uncertainty_", Sys.Date(), ".csv")))
#dom_arch_df <- read_csv(here::here("outputs/tables/nf_level_dominant_archetypes_uncertainty_2024-11-14.csv"))

dom_arch_df <- dom_arch_df %>%
  mutate(ent_all_sc = scale(entropy_all)[1:109],
         shan_div_sc = scale(shan_diverse)[1:109]) %>%
  mutate(div_to_ent = case_when(ent_all_sc < 0 & shan_div_sc < 0 ~ "low_ent_low_div",
                                ent_all_sc < 0 & shan_div_sc > 0 ~ "low_ent_high_div",
                                ent_all_sc > 0 & shan_div_sc < 0 ~ "high_ent_low_div",
                                ent_all_sc > 0 & shan_div_sc > 0 ~ "high_ent_high_div"))
write_csv(dom_arch_df, here::here(paste0("outputs/tables/nf_level_dominant_archetypes_uncertainty_", Sys.Date(), ".csv")))

table_for_appen <- dom_arch_df %>%
  select(!entropy_eco) %>%
  select(!entropy_soc) %>%
  select(!ent_eco_sc) %>%
  select(!ent_soc_sc) %>%
  select(!eco_to_soc) %>%
  select(!dom_archetype)

write_csv(table_for_appen, here::here(paste0("outputs/tables/table_b3_", Sys.Date(), ".csv")))


test_scatter <- dom_arch_df %>%
  ggplot(aes(x=ent_soc_sc, y=ent_eco_sc, group=as.factor(dom_archetype), color=as.factor(dom_archetype))) +
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
  ggplot(aes(x=scale(entropy_soc), y=scale(ent_eco_sc), group=region, color=pct_area_dom_arch >= 59.5)) +
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
  ggplot(aes(x=ent_soc_sc, y=ent_eco_sc, color=as.factor(dom_archetype), size = pct_area_dom_arch, alpha = 0.25)) +
  geom_point() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  xlim(-3.75, 3.75) +
  ylim(-3.75, 3.75) +
  #scale_color_brewer(palette = "Set2") + 
  scale_color_met_d("Hokusai3") +
  theme_bw() +
  guides(size = "none") +
  guides(alpha = "none") +
  annotate("text", x = 2, y = 2, label= "high-high") + 
  annotate("text", x = -2, y = -3, label = "low-low") +
  annotate("text", x = -1.27, y = 2, label= "high-low") + 
  annotate("text", x = 2, y = -3, label = "low-high") +
  theme(legend.position = "inside",
        legend.position.inside = c(0.11, 0.787)) +
  labs(x = "Social Entropy (scaled)",
       y = "Ecological Entropy (scaled)",
       color = "Archetype",
       size = "% Area Dom. Archetype")
soc_eco_ent_sc
ggsave(here::here(paste0("outputs/plots/soc_eco_ent_sc_scatter_", Sys.Date(), ".png")),
       soc_eco_ent_sc, height = 4, width = 4, dpi = 300)

axLabels <- c("", "Low","", "", "","High", "")
div_ent <- dom_arch_df %>%
  ggplot() +
  geom_point(aes(x=scale(entropy_all), y=scale(shan_diverse), 
                 color=as.factor(dom_archetype), size = pct_area_dom_arch, 
                 alpha = 0.25), show.legend = TRUE) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  #scale_color_brewer(palette = "Set2") + 
  scale_color_met_d("Hokusai3") +
  #scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3), labels = axLabels) +
  #scale_y_continuous(breaks = c(-3,-2,-1,0,1,2,3), labels = axLabels) +
  xlim(-3, 3) +
  ylim(-3, 3) +
  scale_size_continuous(name = "% Area Dom. Archetype") + 
  theme_bw() +
  guides(alpha = "none") +
  guides(size = "none") +
  annotate("text", x = 2, y = 2, label= "high-high") + 
  annotate("text", x = -2, y = -3, label = "low-low") +
  annotate("text", x = -1.25, y = 2, label= "high-low") + 
  annotate("text", x = 2, y = -3, label = "low-high") +
  theme(legend.position = "inside",
        legend.position.inside = c(0.11, 0.787)) +
  labs(x = "Entropy (scaled)",
       y = "Diveristy (scaled)",
       color = "Archetype",
       size = "% Area Dom. Archetype")
div_ent

ggsave(here::here(paste0("outputs/plots/diverse_entropy_sc_scatter_", Sys.Date(), ".png")),
       div_ent, height = 4, width = 4, dpi = 300)
       
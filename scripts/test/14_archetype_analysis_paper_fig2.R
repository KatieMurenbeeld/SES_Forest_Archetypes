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
library(MetBrewer)

# Load the data
fs_nf <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeForest.shp")
fs_reg <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeRegion.shp")
sgfcm_all_k6_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_all_result_k6_2024-10-15.tif")
sgfcm_all_attri_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")
sgfcm_all_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")
sgfcm_all_result_k6_mod <- readRDS(here::here("outputs/SGFCM_all_attr_k6_2024-10-15.rds"))

## Reproject the forest service shapes to NAD83
projection <- "epsg: 5070"

fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  filter(FORESTORGC != "0816") %>%
  st_transform(., crs=projection)
fs_nf.crop <- st_crop(fs_nf.proj, ext(sgfcm_all_k6_result))
fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.crop <- st_crop(fs_reg.proj, ext(sgfcm_all_k6_result))

# create region labels to plot on map

reg_cent <- st_centroid(fs_reg.crop)

## Create a map of the clusters with the Region and National Forest boundaries
# for k = 6
sgfcm.k6.all.df <- sgfcm_all_k6_result$Groups %>% as.data.frame(xy = TRUE)

# Rename group (clusters)
# will need to reorder
sgfcm.k6.all.df <- sgfcm.k6.all.df %>%
  mutate(group_alpha = case_when(Groups == 1 ~ "A: Private land-dominated plains", 
                                 Groups == 2 ~ "B: Productive forests near urbanized areas",
                                 Groups == 3 ~ "C: Old forests in rural areas",
                                 Groups == 4 ~ "D: Forest-adjacent systems",
                                 Groups == 5 ~ "E: Urban non-forested areas", 
                                 Groups == 6 ~ "F: Mountain forests and shrublands"))

all_k6_rg_nf_map <- ggplot() +
  geom_raster(aes(x = sgfcm.k6.all.df$x, y = sgfcm.k6.all.df$y, fill = sgfcm.k6.all.df$group_alpha)) +
  geom_sf(data = fs_nf.crop, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  #geom_sf_text(data = reg_cent, aes(label = REGION), fontface = "bold") +
  geom_sf_label(data = reg_cent, aes(label = REGION), nudge_x = 0.5, nudge_y = 0.5, alpha = 0.5, fontface = "bold") +
  #scale_fill_brewer(palette = "Set2") +
  scale_fill_met_d("Hokusai3") +
  labs(#title = "All Attributes:",
       #subtitle = "k=6, m=1.9, alpha = 0.6, beta = 0.4, window = 7x7", 
       fill = "Clusters") +
  #ggthemes::theme_map() +
  theme_minimal() + 
  theme(#text = element_text(size = 20),
        legend.position = "right",
        legend.text = element_text(size = 10),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(color = "white"),
        #rect = element_rect(color = "white"),
        axis.text = element_text(size = 8),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm")) #+ 
  #ggtitle("A")
#all_k6_rg_nf_map
ggsave(here::here(paste0("outputs/plots/fig2_map_testing_", Sys.Date(), ".png")), 
       plot = all_k6_rg_nf_map, width = 8, height = 8, dpi = 300)

# Variable interp plot
data <- as.data.frame(scale(sgfcm_all_attri)) # use non scaled data for IQR overlap
data$groups_k6 <- sgfcm_all_result_k6_mod$Groups

data$groups_k6 <- gsub('V', 'A', data$groups_k6)

k6_long_df <- data %>%
  pivot_longer(!groups_k6, names_to = "var_name")

k6_means_long <- data %>%
  group_by(groups_k6) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k6, names_to = "var_name", values_to = "mean")

k6_sd_long <- data %>%
  group_by(groups_k6) %>%
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k6, names_to = "var_name", values_to = "sd")

k6_med_long <- data %>%
  group_by(groups_k6) %>%
  summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups_k6, names_to = "var_name", values_to = "median")

k6_long <- left_join(k6_means_long, k6_sd_long)
k6_long <- left_join(k6_long, k6_med_long)

# reorder the variables
k6_long_reorder <- k6_long %>% 
  mutate(var_name = fct_relevel(var_name, 
                                "treecov", "forprod", "forgain",
                                "treeage", "tempseas", 
                                "precseas", "rough", "whp", 
                                "distcrit", "distwild",
                                "fedrich", "lesshs", "travtime", "hsbrd", 
                                "engbrd", "pm25", "aip", "netmig", "comm_cap",
                                "pct_forpay", "pct_delmill"
                                  ))

k6_long_reorder <- k6_long_reorder %>%
  mutate(ostrom = case_when(var_name == "treecov" | var_name == "forprod" | var_name == "tempseas" | var_name == "precseas" | var_name == "rough" | var_name == "treeage" | var_name == "forgain" | var_name == "whp" ~ "Biophysical",
                            var_name == "distwild" | var_name == "distcrit" | var_name == "fedrich" ~ "Rules of Use",
                            var_name == "pm25" | var_name == "pct_forpay" | var_name == "pct_delmill" | var_name == "netmig" | var_name == "comm_cap" | var_name == "aip" | var_name == "travtime" | var_name == "hsbrd" | var_name == "engbrd" | var_name == "lesshs" ~ "Attributes of Community"))





k6_var_interp <- ggplot(k6_long_reorder, aes(x = var_name, y = mean, fill = ostrom)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  #scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  facet_wrap(~groups_k6) +
  theme(text = element_text(size = 20),
        legend.position = "right", 
        axis.title.y = element_blank()) 

k6_var_interp

# Create bar plots of the variables where IQR != 0
check_iqr_overlap <- function(x) {
  # Calculate the 25th and 75th percentiles
  lower <- quantile(x, 0.25, na.rm = TRUE)
  upper <- quantile(x, 0.75, na.rm = TRUE)
  
  # Check if the IQR range overlaps 0
  return(lower <= 0 & upper >= 0)
}

overlap <- k6_long_df %>% 
  group_by(groups_k6, var_name) %>% 
  summarise(overlap = check_iqr_overlap(value), .groups="drop")


k6_long_join <- k6_long_df %>% 
  left_join(overlap) %>% 
  filter(overlap == FALSE)

k6_long_overlap_reorder <- k6_long_join %>% # need to remove dist to critical habitat and less hs
  mutate(ostrom = case_when(var_name == "treecov" | var_name == "forprod" | var_name == "tempseas" | var_name == "precseas" | var_name == "rough" | var_name == "treeage" | var_name == "forgain" | var_name == "whp" ~ "Biophysical",
                            var_name == "distwild" | var_name == "distcrit" | var_name == "fedrich" ~ "Rules of Use",
                            var_name == "pm25" | var_name == "pct_forpay" | var_name == "pct_delmill" | var_name == "netmig" | var_name == "comm_cap" | var_name == "aip" | var_name == "travtime" | var_name == "hsbrd" | var_name == "engbrd" | var_name == "lesshs" ~ "Attributes of Community"))


# replace var_name with more easily interpreted names 
k6_long_overlap_reorder_newnames <- k6_long_overlap_reorder %>%
  mutate(new_var_name = case_when(var_name == "treecov" ~ "tree cover",
                                  var_name == "forprod" ~ "forest productivity",
                                  var_name == "tempseas" ~ "temp. seasonality",
                                  var_name == "precseas" ~ "precip. seasonality", 
                                  var_name == "rough" ~ "topo. roughness", 
                                  var_name == "whp" ~ "wildfire haz. potential",
                                  var_name == "distwild" ~ "dist. to wilderness area",
                                 # var_name == "distcrit" ~ "dist. to critical habitat",
                                  var_name == "forgain" ~ "forest gain",
                                  var_name == "fedrich" ~ "num. federal agencies", 
                                  var_name == "pm25" ~ "exposure to PM2.5",
                                  var_name == "treeage" ~ "forest stand age", 
                                  var_name == "pct_forpay" ~ "income from forestry",
                                  var_name == "pct_delmill" ~ "change in mill capacity",
                                  var_name == "netmig" ~ "net migration", 
                                  var_name == "comm_cap" ~ "community capital",
                                  var_name == "aip" ~ "conservatism",
                                  var_name == "travtime" ~ "time to cities",
                                  var_name == "hsbrd" ~ "housing burden",
                                  var_name == "engbrd" ~ "energy burden"))
                                 # var_name == "lesshs" ~ "less highschool ed."))

# reorder the variables
k6_long_overlap_reorder_newnames <- k6_long_overlap_reorder_newnames %>% 
  mutate(new_var_name = factor(new_var_name, levels = c( 
                                "tree cover", 
                                "forest productivity", 
                                "forest gain", 
                                "forest stand age", 
                                "temp. seasonality", 
                                "precip. seasonality", 
                                "topo. roughness", 
                                "wildfire haz. potential", 
                                #"dist. to critical habitat", 
                                "dist. to wilderness area",
                                "num. federal agencies", 
                                #"less highschool ed.",
                                "time to cities", 
                                "housing burden", 
                                "energy burden",
                                "exposure to PM2.5", 
                                "conservatism",
                                "net migration", 
                                "community capital", 
                                "income from forestry", 
                                "change in mill capacity"
                                )))
# Rename the variables for the facet headers
k6_long_overlap_reorder_newnames <- k6_long_overlap_reorder_newnames %>%
  mutate(groups_k6_alpha = case_when(groups_k6 == "A1" ~ "A: Private land-dominated plains", 
                                     groups_k6 == "A2" ~ "B: Productive forests near urbanized areas",
                                     groups_k6 == "A3" ~ "C: Old forests in rural areas",
                                     groups_k6 == "A4" ~ "D: Forest-adjacent systems",
                                     groups_k6 == "A5" ~ "E: Urban non-forested areas", 
                                     groups_k6 == "A6" ~ "F: Mountain forests and shrublands"))

k6_long_overlap_reorder_newnames$ostrom <- factor(k6_long_overlap_reorder_newnames$ostrom, 
                                                  levels=c('Biophysical', 'Rules of Use',
                                                           'Attributes of Community'))


k6_iqr_no_overlap <- ggplot(data=k6_long_overlap_reorder_newnames, 
                            mapping = aes(y=reorder(new_var_name, desc(new_var_name)), 
                                          x=value, fill=ostrom)) +
  geom_boxplot(outliers = FALSE, coef=0) +
 # geom_hline(yintercept = 0, linetype=2) +
  scale_fill_met_d("Java") +
  #scale_fill_met_d("Isfahan2") +
  #coord_flip() +
  #scale_y_reverse() +
  theme_bw() +
  theme(#text = element_text(size = 20),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    axis.text = element_text(size = 8),
    plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm")) +
    facet_wrap(vars(groups_k6_alpha)) #+ 
  #ggtitle("B")

k6_iqr_no_overlap
ggsave(here::here(paste0("outputs/plots/fig2_iqr_panel_testing_", Sys.Date(), ".png")), 
       plot = k6_iqr_no_overlap, width = 8, height = 6, dpi = 300)

panel <- all_k6_rg_nf_map / k6_iqr_no_overlap +
  plot_layout(heights = unit(c(8, 6), c('cm', 'null')))

ggsave(here::here(paste0("outputs/plots/archetype_analysis_fig2_testing_", Sys.Date(), ".png")), 
       plot = panel, width = 8, height = 8, dpi = 300)

ggsave(here::here(paste0("outputs/plots/archetype_analysis_figure_2_testing_", Sys.Date(), ".jpeg")), 
       plot = panel, width = 190, height = 190, dpi = 300, units = "mm", device = "jpeg")


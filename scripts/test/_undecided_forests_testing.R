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

sgfcm_all_k6_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_all_result_k6_2024-10-15.tif")
#sgfcm_all_k8_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_all_result_k8_2024-10-15.tif")

# Format for use in geocmeans
dataset <- lapply(names(sgfcm_all_attri_sc), function(n) {
  aband <- sgfcm_all_attri_sc[[n]]
  return(aband)
})
names(dataset) <- names(sgfcm_all_attri_sc)

# Use Spatial Generalized Fuzzy C-Means clustering
# FCM seed = 1234, Silhouette index = 0.48, k = 6, m = 1.9, window =  7x7 (w2), alpha = 0.6, beta = 0.4
# FCM seed = 1234, Silhouette index = 0.45, k = 8, m = 1.9, window =  3x3 (w1), alpha = 0.5, beta = 0.4

w2 <- matrix(1, nrow = 7, ncol = 7)

SGFCM_all_result_k6 <- SGFCMeans(dataset, k = 6, m = 1.9, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w2, alpha = 0.6, beta = 0.4,
                                 seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")



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

# create buffers around the forests in regions 1 and 4
nf_create_buffers <- function(area_with_nf, dist_m){
  nf_buffs <- data.frame()
  for (nf in 1:109) {
    #print(reg4_nf$FORESTORGC[nf])
    tmp_nf <- area_with_nf %>%
      filter(FORESTORGC == area_with_nf$FORESTORGC[nf])
    tmp_nf_buf <- st_buffer(tmp_nf, dist = dist_m)
    nf_buffs <- rbind(nf_buffs, tmp_nf_buf)
  }
  return(nf_buffs)
}

nf_buffers <- nf_create_buffers(fs_nf.crop, 50000)

#----Get raster of groups and belonging from the sgfcm results-----

arch_rst <- rast(SGFCM_all_result_k6$rasters)

arch_rst_crop <- crop(arch_rst, nf_buffers, mask = TRUE)
plot(arch_rst_crop$Groups)
arch_rst_belong <- subset(arch_rst_crop, 1:6)
plot(arch_rst_belong)

arch_df <- as.data.frame(arch_rst, xy = FALSE)

## calculate the average belongings for each archetype and plot histograms
ave_belong_df <- arch_df %>%
  group_by(Groups) %>%
  summarise_all(mean)


arch1 <- arch_df %>%
  filter(Groups == 1) %>%
  select(group1)
arch2 <- arch_df %>%
  filter(Groups == 2) %>%
  select(group2)
arch3 <- arch_df %>%
  filter(Groups == 3) %>%
  select(group3)
arch4 <- arch_df %>%
  filter(Groups == 4) %>%
  select(group4)
arch5 <- arch_df %>%
  filter(Groups == 5) %>%
  select(group5)
arch6 <- arch_df %>%
  filter(Groups == 6) %>%
  select(group6)

arch_df %>%
  filter(Groups == 6) %>%
  ggplot(., aes(x = group6)) + 
  geom_histogram(fill = "steelblue") +
  xlim(0, 1) +
  labs(title = "Multiple Histograms", x = "Value", y = "Frequency") +
  theme_minimal()

hist(arch1$group1, main = "Histogram 1", xlab = "Value", ylab = "Frequency",
     col = "powderblue")
# Create the second histogram
hist(arch2$group2, main = "Histogram 2", xlab = "Value", ylab = "Frequency",
     col = "pink", add = TRUE)
hist(arch3$group3, main = "Histogram 2", xlab = "Value", ylab = "Frequency",
     col = "yellow", add = TRUE)
hist(arch4$group4, main = "Histogram 2", xlab = "Value", ylab = "Frequency",
     col = "orange", add = TRUE)
hist(arch5$group5, main = "Histogram 2", xlab = "Value", ylab = "Frequency",
     col = "purple3", add = TRUE)
hist(arch6$group6, main = "Histogram 2", xlab = "Value", ylab = "Frequency",
     col = "salmon", add = TRUE)

#----create a map of the NF + buffers with final archetype groups and regional boundaries-----
sgfcm.k6.all.df <- arch_rst_crop$Groups %>% as.data.frame(xy = TRUE)

all_k6_nf_buff_map <- ggplot() +
  geom_raster(aes(x = sgfcm.k6.all.df$x, y = sgfcm.k6.all.df$y, fill = as.factor(sgfcm.k6.all.df$Groups))) +
  geom_sf(data = fs_nf.crop, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "All Attributes:",
       subtitle = "k=6, m=1.9, alpha = 0.6, beta = 0.4, window = 7x7", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "mm"))

all_k6_nf_buff_map
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_all_k6_nf_buff_map_", Sys.Date(), ".png"), 
       plot = all_k6_nf_buff_map, width = 12, height = 12, dpi = 300) 

#----create the threshold sequence and empty data frame----
thres <- seq(0.1, 1, 0.05)

undecided_df <- data.frame(
  region = as.character(),
  forest = as.character(),
  pct_undecided = as.numeric(),
  threshold = as.numeric()
)

#----run a for loop that----
## 1. crops the belongings raster to the national forest and buffer
## 2. for each threshold determines the number of undecided (any group belonging < threshold)
## 3. calculates the percentage of undecided pixels out of all pixels in the forest (not including NAs)
## 4. fills in the undecided data frame

#for (nf in nf_buffers$FORESTORGC){
#  tmp_shp <- nf_buffers %>%
#    filter(FORESTORGC == nf)
#  print(nf)
#  print(nf_buffers$REGION[nf_buffers$FORESTORGC == nf])
#}

for (nf in nf_buffers$FORESTORGC) {
  tmp_shp <- nf_buffers %>%
    filter(FORESTORGC == nf)
  tmp_rast <- crop(arch_rst_belong, tmp_shp, mask = TRUE)
  for (t in thres){
    region <- nf_buffers$REGION[nf_buffers$FORESTORGC == nf]
    forest <- nf
    threshold <- t
    tmp_undecided <- any(max(tmp_rast) < t)
    tmp_df <- freq(tmp_undecided)
    if (length(tmp_df$count[tmp_df$value == 1]) == 0) {
      pct_undecided <- 0
    } else {
      pct_undecided <- (tmp_df$count[tmp_df$value == 1] / sum(tmp_df$count)) * 100
    }
    undecided_df[nrow(undecided_df) + 1, ] <- as.list(c(region,
                                                       forest, 
                                                       pct_undecided,
                                                       threshold))
  }
}

#----save the data frame as a csv----
#write_csv(undecided_df, here::here(paste0("outputs/tables/usfs_nf_undecided_thresholds_", Sys.Date(), ".csv")))
undecided_df <- read_csv(here::here("outputs/tables/usfs_nf_undecided_thresholds_2024-10-24.csv"))

#----plot the results as regional panels with a line for each forest in the region----
#my_colors <- list(rep("darkgrey", 109))
region_names <- list(
  '01'="Region 01",
  '02'="Region 02",
  '03'="Region 03",
  '04'="Region 04",
  '05'="Region 05",
  '06'="Region 06",
  '08'="Region 08",
  '09'="Region 09"
)

region_labeller <- function(variable,value){
  return(region_names[value])
}

undecided_plot <- ggplot(data = undecided_df, mapping = aes(x = as.numeric(threshold), y = as.numeric(pct_undecided), color = forest)) + 
  geom_line() + 
#  scale_colour_manual(name = "forest", values = my_colors, limits = force) +
  facet_wrap(~region, ncol = 4, labeller = region_labeller) + 
  theme_bw() +
  theme(legend.position = "none") + 
  labs(x = "Belongings", 
       y = "Area Forest of Undecided Pixels (%)")
undecided_plot

# save the plot
ggsave(here::here(paste0("outputs/plots/usfs_nf_undecided_thresholds_", Sys.Date(), ".png")), 
       undecided_plot, width = 8, height = 5, dpi = 300)


test_plot <- undecided_df %>%
  group_by(forest) %>%
  ggplot(data = undecided_df, mapping = aes(x = as.numeric(threshold), y = as.numeric(pct_undecided))) + 
  geom_line() + 
  facet_wrap(~region, ncol = 4) + 
  theme_bw() +
  theme(legend.position = "none") + 
  labs(x = "Belongings", 
       y = "Area Forest of Undecided Pixels (%)")
test_plot

#----from the undecided threshold plot, find the "outliers"------
undecided_plot <- ggplot(data = undecided_df %>%
                           filter(forest == "0601" | forest == "0621"), mapping = aes(x = as.numeric(threshold), y = as.numeric(pct_undecided), color = forest)) + 
  geom_line() + 
  facet_wrap(~region, ncol = 4) + 
  theme(legend.position = "right")
undecided_plot


#----determine the average maximum belonging for each forest----
ave_belong_df <- data.frame(
  region = as.character(),
  forest = as.character(),
  ave_max = as.numeric(),
  med_max = as.numeric()
)

for (nf in nf_buffers$FORESTORGC) {
  region <- nf_buffers$REGION[nf_buffers$FORESTORGC == nf]
  forest <- nf
  tmp_shp <- nf_buffers %>%
    filter(FORESTORGC == nf)
  tmp_rast <- crop(arch_rst_belong, tmp_shp, mask = TRUE)
  tmp_df <- as.data.frame(tmp_rast, xy = FALSE)
  tmp_df <- tmp_df %>%
    mutate(max_belong = pmax(group1, group2, group3, group4, group5, group6),
           max_belong_arch = names(.)[max.col(., 'first')], 
           forestorgc = nf)
  ave_max <- mean(tmp_df$max_belong)
  med_max <- median(tmp_df$max_belong)
  ave_belong_df[nrow(ave_belong_df) + 1, ] <- as.list(c(region,
                                                      forest, 
                                                      ave_max,
                                                      med_max))
}  
  
#----save the data frame as a csv----
write_csv(ave_belong_df, here::here(paste0("outputs/tables/usfs_nf_max_belongings_", Sys.Date(), ".csv")))

#----determine the forests with a "dominant" archetype-------
v <- nf_buffers %>% st_cast("MULTIPOLYGON")
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

areas_wide <- areas %>%
  dplyr::select(-total_arch_area) %>%
  dplyr::select(-proportion) %>%
  pivot_wider(names_from = value, values_from = proportion_pct)


arche1 <-  areas_wide %>%
  filter(`1` >= 50.0)
arche2 <-  areas_wide %>%
  filter(`2` >= 50.0)
arche3 <-  areas_wide %>%
  filter(`3` >= 50.0)
arche4 <-  areas_wide %>%
  filter(`4` >= 50.0)
arche5 <-  areas_wide %>%
  filter(`5` >= 50.0)
arche6 <-  areas_wide %>%
  filter(`6` >= 50.0)
arche_na <-  areas_wide %>%
  filter(`NA` >= 50.0)

forest_arches <- areas_wide %>%
  filter(`1` >= 50 | `2` >= 50 | `3` >= 50 | `4` >= 50 | `5` >= 50 | `6` >= 50)

tmp_shp <- nf_buffers %>%
  filter(FORESTORGC == "0915")
plot(crop(sgfcm_all_k6_result, tmp_shp, mask = TRUE))

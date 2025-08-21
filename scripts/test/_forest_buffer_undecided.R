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

#----Use sgfcm to create forest clusters
w1 <- matrix(1, nrow = 7, ncol = 7)

SGFCM_all_result_k3 <- SGFCMeans(dataset, k = 3, m = 1.2, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w1, alpha = 0.6, beta = 0.6,
                                 seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")


#----Get raster of groups and belonging from the sgfcm results-----

arch_rst <- rast(SGFCM_all_result_k3$rasters)

#arch_rst_crop <- crop(arch_rst, nf_buffers, mask = TRUE)
plot(arch_rst$Groups)
arch_rst_belong <- subset(arch_rst, 1:3)
plot(arch_rst_belong)

arch_df <- as.data.frame(arch_rst, xy = FALSE)

## calculate the average belongings for each archetype and plot histograms
ave_belong_df <- arch_df %>%
  group_by(Groups) %>%
  summarise_all(mean)

## use the undecidedUnits from geocemans
Undecided <- undecidedUnits(SGFCM_all_result_k3$Belongings,0.45)

mapClusters(geodata = NULL, SGFCM_all_result_k3, undecided = 0.50)
mapClusters(geodata = NULL, SGFCM_all_result_k3, undecided = 0.60)
mapClusters(geodata = NULL, SGFCM_all_result_k3, undecided = 0.70)
mapClusters(geodata = NULL, SGFCM_all_result_k3, undecided = 0.80)
mapClusters(geodata = NULL, SGFCM_all_result_k3, undecided = 0.90)


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
nf_buffers <- st_intersection(nf_sf, fs_reg.crop)
nf_buffers_test <- st_crop(nf_sf, fs_reg.crop)

for (nf in nf_buffers_test$FORESTORGC) {
  tmp_shp <- nf_buffers_test %>%
    filter(FORESTORGC == nf)
  tmp_rast <- crop(arch_rst_belong, tmp_shp, mask = TRUE)
  for (t in thres){
    region <- nf_buffers_test$REGION[nf_buffers_test$FORESTORGC == nf]
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
  labs(x = "Belongings Threshold", 
       y = "Area Forest of Undecided Pixels (%)")
undecided_plot

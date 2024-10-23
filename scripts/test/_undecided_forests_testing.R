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

# Load the data
sgfcm_all_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")
sgfcm_all_attri_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")

#sgfcm_all_k6_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_all_result_k6_2024-10-15.tif")
#sgfcm_all_k8_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_all_result_k8_2024-10-15.tif")

# Format for use in geocmeans
dataset <- lapply(names(sgfcm_all_attri_sc), function(n){
  aband <- sgfcm_all_attri_sc[[n]]
  return(aband)
})
names(dataset) <- names(sgfcm_all_attri_sc)

# Use Spatial Generalized Fuzzy C-Means clustering 
# FCM seed = 1234, Silhouette index = 0.48, k = 6, m = 1.9, window =  7x7 (w2), alpha = 0.6, beta = 0.4
# FCM seed = 1234, Silhouette index = 0.45, k = 8, m = 1.9, window =  3x3 (w1), alpha = 0.5, beta = 0.4

w1 <- matrix(1, nrow = 3, ncol = 3)
w2 <- matrix(1, nrow = 7, ncol = 7)

SGFCM_all_result_k6 <- SGFCMeans(dataset, k = 6, m = 1.9, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w2, alpha = 0.6, beta = 0.4,
                                 seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")


SGFCM_all_result_k8 <- SGFCMeans(dataset, k = 8, m = 1.9, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w1, alpha = 0.5, beta = 0.4,
                                 seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

# crop to regions 1 and 4
fs_reg <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeRegion.shp")

projection <- "epsg: 5070"

fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  #filter(REGION == "01" | REGION == "04") %>%
  #filter(FORESTORGC == "0414") %>%
  st_transform(., crs=projection)
fs_nf.crop <- st_crop(fs_nf.proj, ext(sgfcm_all_attri_sc))
fs_reg.proj <- fs_reg %>% 
  filter(REGION == "01" | REGION == "04") %>%
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

thres <- seq(0.05, 1, 0.05)

sgfcm_all_k6_maps <- mapClusters(object = SGFCM_all_result_k6, undecided = 0.167)

test <- as.data.frame(SGFCM_all_result_k6$Belongings)
test_rst_df <- as.data.frame(SGFCM_all_result_k6$rasters, xy = TRUE)
test_rst <- rast(SGFCM_all_result_k6$rasters)

test_rst_crop <- crop(test_rst, nf_buffers, mask = TRUE)
plot(test_rst_crop)
rst_crop_df <- as.data.frame(test_rst_crop, xy = TRUE)

#rst_crop_df$threshold <- 0.1
#rst_crop_df$threshold

test_rst_belong <- subset(test_rst_crop, 1:6)
plot(test_rst_belong)
#test_rst_crop$undecided <- 0

thres <- 0.4
r_undecided <- max(test_rst_belong)
plot(r_undecided)

thres <- 0.1
r_undecided <- any(max(test_rst_belong) < thres) # < threshold so that 1 = undecided
plot(r_undecided)
sum(values(r_undecided), na.rm=TRUE)
ncell(r_undecided)
freq_df <- freq(r_undecided)
#pct_undecided <- (freq_df$count[freq_df$value == 1] / sum(freq_df$count)) * 100
(freq_df$count[freq_df$value == 1] / sum(freq_df$count)) * 100
freq_df$count[freq_df$value == 1]

thres <- seq(0.1, 1, 0.1)

undecided_df <- data.frame(
  forest = as.character(),
  pct_undecided = as.numeric(),
  threshold = as.numeric()
)

for (t in thres){
  #print(paste0("threshold = ", t))
  forest <- nf_buffers$FORESTORGC
  threshold <- t
  tmp_undecided <- any(max(test_rst_belong) < t)
  tmp_df <- freq(tmp_undecided)
  if (length(tmp_df$count[tmp_df$value == 1]) == 0){
    pct_undecided <- 0
  }else{
  pct_undecided <- (tmp_df$count[tmp_df$value == 1] / sum(tmp_df$count)) * 100
  }
  #print(paste0("% undecided = ", pct_undecided))
  undecided_df[nrow(undecided_df) + 1,] <- as.list(c(forest, 
                                                     pct_undecided,
                                                     threshold))
}

plot(undecided_df$threshold, undecided_df$pct_undecided)


test_rst <- rast(SGFCM_all_result_k6$rasters)

test_rst_crop <- crop(test_rst, nf_buffers, mask = TRUE)
plot(test_rst_crop$group1)
test_rst_belong <- subset(test_rst_crop, 1:6)
plot(test_rst_belong$group1)

thres <- seq(0.1, 1, 0.05)

undecided_df <- data.frame(
  region = as.character(),
  forest = as.character(),
  pct_undecided = as.numeric(),
  threshold = as.numeric()
)


for (nf in nf_buffers$FORESTORGC){
  tmp_shp <- nf_buffers %>%
    filter(FORESTORGC == nf)
  print(nf)
  print(nf_buffers$REGION[nf_buffers$FORESTORGC == nf])
}

for (nf in nf_buffers$FORESTORGC){
  #print(nf)
  tmp_shp <- nf_buffers %>%
    filter(FORESTORGC == nf)
  tmp_rast <- crop(test_rst_belong, tmp_shp, mask = TRUE)
  for (t in thres){
    #print(paste0("threshold = ", t))
    region <- nf_buffers$REGION[nf_buffers$FORESTORGC == nf]
    forest <- nf
    threshold <- t
    #tmp_rast <- crop()
    #tmp_undecided <- any(max(test_rst_belong) < t)
    tmp_undecided <- any(max(tmp_rast) < t)
    tmp_df <- freq(tmp_undecided)
    if (length(tmp_df$count[tmp_df$value == 1]) == 0){
      pct_undecided <- 0
    }else{
      pct_undecided <- (tmp_df$count[tmp_df$value == 1] / sum(tmp_df$count)) * 100
    }
  #print(paste0("% undecided = ", pct_undecided))
    undecided_df[nrow(undecided_df) + 1,] <- as.list(c(region,
                                                       forest, 
                                                       pct_undecided,
                                                       threshold))
  }
}

#filter <- undecided_df %>%
#  filter(forest == "0419")

ggplot(data = undecided_df, mapping = aes(x = as.numeric(threshold), y = as.numeric(pct_undecided), color = forest)) + 
  geom_line() + 
  facet_wrap(~region) + 
  theme(legend.position = "none")







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
fs_nf <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeForest.shp")
fs_reg <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeRegion.shp")
sgfcm_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_eco_attributes_2024-10-02.tif")
sgfcm_result <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_eco_result_2024-10-03.tif") 

## Reproject the forest service shapes to NAD83
projection <- "epsg: 5070"

fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  filter(FORESTORGC != "0816") %>%
  st_transform(., crs=projection)
fs_nf.crop <- st_crop(fs_nf.proj, ext(sgfcm_all_result))
fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.crop <- st_crop(fs_reg.proj, ext(sgfcm_all_result))

## Create a map of the clusters with the Region and National Forest boundaries
sgfcm.df <- sgfcm_result$Groups %>% as.data.frame(xy = TRUE)

eco_rg_nf_map <- ggplot() +
  geom_raster(aes(x = sgfcm.df$x, y = sgfcm.df$y, fill = as.factor(sgfcm.df$Groups))) +
  geom_sf(data = fs_nf.crop, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Ecological Attributes:",
       subtitle = "k=3, m=1.7, alpha = 0.6, beta = 0.3, window = 3x3", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

eco_rg_nf_map
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_eco_reg_nf_map_", Sys.Date(), ".png"), 
       plot = eco_rg_nf_map, width = 12, height = 12, dpi = 300)  



# quick groups matching test
ordered_sgfcm_eco <- groups_matching(SGFCM_all_result, SGFCM_eco_result)
# Error in qual_mat[rkeep, ckeep] : (subscript) logical subscript too long
# can't do because different number of clusters

ordered_sgfcm_eco <- groups_matching(SGFCM_all_result, SGFCM_eco_result_k8)

ordered_sgfcm_soc <- groups_matching(SGFCM_all_result, SGFCM_soc_result_k8)

ordered_soc_eco <- groups_matching(SGFCM_eco_result_k8, SGFCM_soc_result_k8)

map_groups_matching <- rast(ordered_sgfcm_eco$rasters)
plot(map_groups_matching[["Groups"]])
map_eco_k8 <- rast(SGFCM_eco_result_k8$rasters)
plot(map_eco_k8[["Groups"]])
all_map <- rast(SGFCM_all_result$rasters)
plot(all_map[["Groups"]])


matX <- SGFCM_all_result$Belongings
matY <- SGFCM_eco_result_k8$Belongings

k <- ncol(matX)
qual_mat <- calc_jaccard_mat(matX,matY)
colnames(qual_mat) <- 1:ncol(qual_mat)
rownames(qual_mat) <- 1:nrow(qual_mat)




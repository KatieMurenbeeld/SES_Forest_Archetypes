library(tidyverse)
library(terra)
library(raster)
library(sf)
library(ggplot2)
library(geocmeans)
library(RColorBrewer)
library(viridis)


#----Load the data----
rst_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")
rst <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")

# Format for use in geocmeans
dataset <- lapply(names(rst_sc), function(n){
  aband <- rst_sc[[n]]
  return(aband)
})
names(dataset) <- names(rst_sc)

# Calculate the correlation between the attributes


# Use Spatial Generalized Fuzzy C-Means clustering 
# FCM seed = 1234, Silhouette index = 0.48, k = 6, m = 1.9, window =  7x7 (w2), alpha = 0.6, beta = 0.4
# FCM seed = 1234, Silhouette index = 0.45, k = 8, m = 1.9, window =  3x3 (w1), alpha = 0.5, beta = 0.4

w1 <- matrix(1, nrow = 3, ncol = 3)
w2 <- matrix(1, nrow = 7, ncol = 7)

SGFCM_all_result_k6 <- SGFCMeans(dataset, k = 6, m = 1.9, standardize = FALSE,
                              lag_method = "mean",
                              window = w2, alpha = 0.6, beta = 0.4,
                              seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

saveRDS(SGFCM_all_result_k6, paste0("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/outputs/SGFCM_all_attr_k6_", 
                                    Sys.Date(), ".rds"))
map_SGFCM_result_k6 <- rast(SGFCM_all_result_k6$rasters)
plot(map_SGFCM_result_k6[["Groups"]])
writeRaster(map_SGFCM_result_k6[["Groups"]], filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_all_result_k6_", 
                                                            Sys.Date(), ".tif"))

SGFCM_all_result_k8 <- SGFCMeans(dataset, k = 8, m = 1.9, standardize = FALSE,
                                 lag_method = "mean",
                                 window = w1, alpha = 0.5, beta = 0.4,
                                 seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

saveRDS(SGFCM_all_result_k8, paste0("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/outputs/SGFCM_all_attr_k8_", 
                                    Sys.Date(), ".rds"))
map_SGFCM_result_k8 <- rast(SGFCM_all_result_k8$rasters)
plot(map_SGFCM_result_k8[["Groups"]])
writeRaster(map_SGFCM_result_k8[["Groups"]], filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/SGFCM_all_result_k8_", 
                                                               Sys.Date(), ".tif"))

## Save a few data frames to help with visualization
SGFCM_k6_groups <- SGFCM_all_result_k6$Groups
df_all <- as.data.frame(scale(rst), na.rm = TRUE)
vplots_k6 <- violinPlots(df_all, SGFCM_all_result_k6$Groups)
vplots_k8 <- violinPlots(df_all, SGFCM_all_result_k6$Groups)

for ( i in 1:2){
  print(i)
  vplots_tmp <- vplots_k6[[i]] +
    scale_x_discrete(labels=c("A 1", "A 2", 
                              "A 3", "A 4",
                              "A 5", "A 6")) + 
    scale_fill_brewer(labels=c("A 1", "A 2", 
                               "A 3", "A 4",
                               "A 5", "A 6"),
                      palette = "Set2") +
    geom_violin() + 
    geom_hline(yintercept = 0) +
    labs(title = "SGFCM k = 6", 
         subtitle = names(df_all)[i],
         fill = "Archetype") +
    theme_bw() + 
    theme(text = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 16), 
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 16))
  plot(vplots_tmp)
  ggsave(paste0("~/Analysis/SES_Forest_Archetypes/outputs/plots/SGFCM_k6_vplot_", names(df_all)[i], "_", Sys.Date(), ".png"), 
         plot = vplots_tmp, width = 12, height = 12, dpi = 300)
}

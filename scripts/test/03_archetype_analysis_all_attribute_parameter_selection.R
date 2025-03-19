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
rst_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")
rst <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")

# Format for use in geocmeans
dataset <- lapply(names(rst_sc), function(n){
  aband <- rst_sc[[n]]
  return(aband)
})
names(dataset) <- names(rst_sc)

#----Use a non spatial and non generalized fuzzy c-means to determine number of k and value for m
future::plan(future::multisession(workers = 2))
FCMvalues <- select_parameters.mc(algo = "FCM", data = dataset, 
                                  k = 2:10, m = seq(1.1,2,0.1), spconsist = FALSE, 
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Negentropy.index", "Silhouette.index"),
                                  seed = 1234, verbose = TRUE) 

write_csv(FCMvalues, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/fcm_all_attri_param_indices_",
                            Sys.Date(), ".csv"), append = FALSE)

# plotting the silhouette index
fcm_si <- ggplot(FCMvalues) + 
  geom_raster(aes(x = k, y = m, fill = Silhouette.index)) + 
  geom_text(aes(x = k, y = m, label = round(Silhouette.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=2)
fcm_si
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_fcm_si_", 
                  Sys.Date(), ".jpeg")), 
       plot = fcm_si, height = 6, width = 10, dpi = 300)
# plotting the Xie Beni index
fcm_xb <- ggplot(FCMvalues) + 
  geom_raster(aes(x = k, y = m, fill = XieBeni.index)) + 
  geom_text(aes(x = k, y = m, label = round(XieBeni.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=2)
fcm_xb
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_fcm_xb_", 
                  Sys.Date(), ".jpeg")), 
       plot = fcm_xb, height = 6, width = 10, dpi = 300)
# seed 6891 SI = 0.32, k = 3, m = 1.5 (SI = 0.34, k = 9, m = 1.6 but 9 too high)
# FCM seed 1234 SI = 0.45, k = 6, m = 1.6. SI = 0.43, k = 8, m = 1.6

# Highest silhouette index = 0.48  with k = 5 and m = 1.6
# Silhouette index = 0.46 with k = 8 and m = 1.6

#----Use a generalized fuzzy c-means to determine the value for m and beta
# k = 6
GFCMvalues_k6 <- select_parameters.mc(algo = "GFCM", data = dataset, seed = 6891,
                                  k = 6, m = seq(1.1,2,0.1), beta = seq(0.1,0.9,0.1),
                                  spconsist = FALSE, verbose = TRUE, init = "kpp",
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Negentropy.index", "Silhouette.index"))  

write_csv(GFCMvalues_k6, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/gfcm_all_attri_param_indices_k6_",
                            Sys.Date(), ".csv"), append = FALSE)


# plotting the silhouette index
gfcm_si <- ggplot(GFCMvalues_k6) + 
  geom_raster(aes(x = m, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = beta, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
gfcm_si
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_gfcm_si_", 
                         Sys.Date(), ".jpeg")), 
       plot = gfcm_si, height = 6, width = 10, dpi = 300)
# plotting the Xie Beni
gfcm_xb <- ggplot(GFCMvalues_k6) + 
  geom_raster(aes(x = m, y = beta, fill = XieBeni.index)) + 
  geom_text(aes(x = m, y = beta, label = round(XieBeni.index, 2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
gfcm_xb
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_gfcm_xb_", 
                         Sys.Date(), ".jpeg")), 
       plot = gfcm_xb, height = 6, width = 10, dpi = 300)
# Silhouette index = 0.46, k = 5, m = 1.6, beta = 0.1 (XB = 1288.89)
# Silhouette index = 0.45, k = 5, m = 2.0, beta = 0.3 (XB = 714.09) !!
# Silhouette index = 0.30, k = 5, m = 1.5, beta = 0.2 (XB = 18.6)
# FCM seed = 1234, SI = 0.46, k = 6, m = 1.9, beta = 0.3

# k = 8
GFCMvalues_k8 <- select_parameters.mc(algo = "GFCM", data = dataset, seed = 6891,
                                     k = 8, m = seq(1.1,2,0.1), beta = seq(0.1,0.9,0.1),
                                     spconsist = FALSE, verbose = TRUE, init = "kpp",
                                     indices = c("XieBeni.index", "Explained.inertia",
                                                 "Negentropy.index", "Silhouette.index"))  

write_csv(GFCMvalues_k8, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/gfcm_all_attri_param_indices_k8_",
                                Sys.Date(), ".csv"), append = FALSE)


# plotting the silhouette index
ggplot(GFCMvalues_k8) + 
  geom_raster(aes(x = m, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = beta, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# plotting the Xie Beni
ggplot(GFCMvalues_k8) + 
  geom_raster(aes(x = m, y = beta, fill = XieBeni.index)) + 
  geom_text(aes(x = m, y = beta, label = round(XieBeni.index, 2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# Silhouette index = 0.43, k = 8, m = 1.6, beta = 0.1 (XB = 2770.66)
# Silhouette index = 0.42, k = 8, m = 1.9, beta = 0.3 (XB = 1390.96)!!
# Silhouette index = 0.36, k = 8, m = 1.8, beta = 0.2 (XB = 385.27)
# FCM seed = 1234, SI = 0.42, k = 8, m = 1.9, beta = 0.3


# Spatial FCM
w1 <- matrix(1, nrow = 3, ncol = 3)
w2 <- matrix(1, nrow = 5, ncol = 5)
w3 <- matrix(1, nrow = 7, ncol = 7)

# k = 6
SFCMvalues_k6 <- select_parameters.mc(algo = "SFCM", data = dataset, 
                                          k = 6, m = 1.9,
                                          alpha = seq(0.1,2,0.1),
                                          window = list(w1,w2,w3),
                                          spconsist = TRUE, nrep = 5, 
                                          verbose = TRUE, chunk_size = 4,
                                          seed = 6891, init = "kpp",
                                          indices = c("XieBeni.index", "Explained.inertia",
                                                      "Negentropy.index", "Silhouette.index"))


dict <- data.frame(
  w = c(1,2,3),
  window = c("3x3","5x5","7x7")
)

SFCMvalues_k6$window <- dict$window[match(SFCMvalues_k6$window,dict$w)]
write_csv(SFCMvalues_k6, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/sfcm_all_attri_param_indices_k6_", 
                             Sys.Date(), ".csv"), append = FALSE)


# plotting the silhouette index
sfcm_si <- ggplot(SFCMvalues_k6) + 
  geom_raster(aes(x = alpha, y = window, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = window, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.5)
sfcm_si
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_sfcm_si_", 
                         Sys.Date(), ".jpeg")), 
       plot = sfcm_si, height = 6, width = 10, dpi = 300)
# plotting the Xie Beni
sfcm_xb <- ggplot(SFCMvalues_k6) + 
  geom_raster(aes(x = alpha, y = window, fill = XieBeni.index)) + 
  geom_text(aes(x = alpha, y = window, label = round(XieBeni.index, 2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.5)
sfcm_xb
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_sfcm_xb_", 
                         Sys.Date(), ".jpeg")), 
       plot = sfcm_xb, height = 6, width = 10, dpi = 300)
# Silhouette index = 0.36, k = 5, m = 2.0, window = 3x3 (w1), alpha = 0.3
# FCM seed = 1234, SI = 0.34, k = 6, m = 1.9, window =  7x7 (w3), alpha = 1.1

# k = 8
SFCMvalues_k8 <- select_parameters.mc(algo = "SFCM", data = dataset, 
                                      k = 8, m = 1.9,
                                      alpha = seq(0.1,2,0.1),
                                      window = list(w1,w2,w3),
                                      spconsist = TRUE, nrep = 5, 
                                      verbose = TRUE, chunk_size = 4,
                                      seed = 6891, init = "kpp",
                                      indices = c("XieBeni.index", "Explained.inertia",
                                                  "Negentropy.index", "Silhouette.index"))


dict <- data.frame(
  w = c(1,2,3),
  window = c("3x3","5x5","7x7")
)

SFCMvalues_k8$window <- dict$window[match(SFCMvalues_k8$window,dict$w)]
write_csv(SFCMvalues_k8, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/sfcm_all_attri_param_indices_k8_", 
                                Sys.Date(), ".csv"), append = FALSE)


# plotting the silhouette index
ggplot(SFCMvalues_k8) + 
  geom_raster(aes(x = alpha, y = window, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = window, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.5)

# plotting the Xie Beni
ggplot(SFCMvalues_k8) + 
  geom_raster(aes(x = alpha, y = window, fill = XieBeni.index)) + 
  geom_text(aes(x = alpha, y = window, label = round(XieBeni.index, 2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.5)

# Silhouette index = 0.21, k = 8, m = 1.9, window = 3x3 (w1), alpha = 0.7
# FCM seed = 1234, SI = 0.21, k = 8, m = 1.9, window =  3x3 (w1), alpha = 0.7

# Spatial GFCM
# Start here when you get home. 
future::plan(future::multisession(workers = 2))
# k = 6
SGFCMvalues_k6 <- select_parameters.mc(algo = "SGFCM", data = dataset,
                                    k = 6, m = 1.9,
                                    beta = seq(0.1,0.9,0.1), alpha = seq(0.5,2,0.1),
                                    window = w3,
                                    spconsist = TRUE, nrep = 5, 
                                    verbose = TRUE, chunk_size = 4,
                                    seed = 456, init = "kpp",
                                    indices = c("XieBeni.index", "Explained.inertia",
                                                "Negentropy.index", "Silhouette.index"))


#write_csv(SGFCMvalues_k6, here::here(paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/sgfcm_all_attri_param_indices_k6_", 
#                             Sys.Date(), ".csv")), append = FALSE)
write_csv(SGFCMvalues_k6, here::here(paste0("outputs/sgfcm_all_attri_param_indices_k6_", 
                                            Sys.Date(), ".csv")), append = FALSE)
#SGFCMvalues_k6_check <- read_csv(here::here("outputs/sgfcm_all_attri_param_indices_k6_2024-10-28.csv"))

# showing the silhouette index
sgfcm_si <- ggplot(SGFCMvalues_k6) + 
  geom_raster(aes(x = alpha, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(Silhouette.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
sgfcm_si
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_sgfcm_si_", 
                         Sys.Date(), ".jpeg")), 
       plot = sgfcm_si, height = 6, width = 10, dpi = 300)
# showing the spatial inconsistency
ggplot(SGFCMvalues_k6) + 
  geom_raster(aes(x = alpha, y = beta, fill = spConsistency)) + 
  geom_text(aes(x = alpha, y = beta, label = round(spConsistency,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# showing the Xie Beni index
sgfcm_xb <- ggplot(SGFCMvalues_k6) + 
  geom_raster(aes(x = alpha, y = beta, fill = XieBeni.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(XieBeni.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
sgfcm_xb 
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_sgfcm_xb_", 
                         Sys.Date(), ".jpeg")), 
       plot = sgfcm_xb, height = 6, width = 10, dpi = 300)
# Silhouette index = 0.51, k = 5, m = 2.0, beta = 0.8, alpha = 1.2, w = 3x3 (w1)
# FCM seed = 1234, SI = 0.48, XB = 607.38, k = 6, m = 1.9, window =  7x7 (w3), alpha = 0.6, beta = 0.4

# k = 8
SGFCMvalues_k8 <- select_parameters.mc(algo = "SGFCM", data = dataset,
                                       k = 8, m = 1.9,
                                       beta = seq(0.1,0.9,0.1), alpha = seq(0.5,2,0.1),
                                       window = w1,
                                       spconsist = TRUE, nrep = 5, 
                                       verbose = TRUE, chunk_size = 4,
                                       seed = 456, init = "kpp",
                                       indices = c("XieBeni.index", "Explained.inertia",
                                                   "Negentropy.index", "Silhouette.index"))


write_csv(SGFCMvalues_k8, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/sgfcm_all_attri_param_indices_k8_", 
                                 Sys.Date(), ".csv"), append = FALSE)
# showing the silhouette index
ggplot(SGFCMvalues_k8) + 
  geom_raster(aes(x = alpha, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(Silhouette.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)


# showing the spatial inconsistency
ggplot(SGFCMvalues_k8) + 
  geom_raster(aes(x = alpha, y = beta, fill = spConsistency)) + 
  geom_text(aes(x = alpha, y = beta, label = round(spConsistency,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# showing the Xie Beni index
ggplot(SGFCMvalues_k8) + 
  geom_raster(aes(x = alpha, y = beta, fill = XieBeni.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(XieBeni.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# Silhouette index = 0.47, k = 8, m = 1.9, beta = 0.5, alpha = 1.1, w = 3x3 (w1) 
# seems that with this set of attributes the best SI I will get will be around 0.5
# FCM seed = 1234, SI = 0.45, k = 8, m = 1.9, window =  3x3 (w1), alpha = 0.5, beta = 0.4 (a little lower XB than with an SI of 0.47) 

# --- from a PREVIOUS EXAMPLE BUT I DIDN'T SET THE SEED
# Silhouette index = 0.54, k = 8, m = 1.7, beta = 0.2, alpha = 1.1, w = 7x7 (but xie beni highest)
# Silhouette index = 0.51, k = 8, m = 1.7, beta = 0.2, alpha = 1.7, w = 7x7 (xie beni lower)




#-------------------ignore code below: delete later--------------------------------------------------------
# alpha of 1.3 will be good
SGFCM_result <- SGFCMeans(dataset_pmrc_poli, k = 8, m = 1.6, standardize = FALSE,
                          lag_method = "mean",
                          window = w1, alpha = 1.3, beta = 0.1,
                          seed = 6891, tol = 0.001, verbose = FALSE, init = "kpp")
#saveRDS(SGFCM_result, here::here(paste0("data/processed/SGFCM_result_pmrc_poli_", Sys.Date(), ".rds")))
#SGFCM_result <- readRDS(here::here("data/processed/SGFCM_result_pmrc_poli_2024-08-26.rds"))

map_SGFCM_result <- rast(SGFCM_result$rasters)
plot(map_SGFCM_result[["Groups"]])
#writeRaster(map_SGFCM_result[["Groups"]], filename = paste0("data/processed/SGFCM_result_pmrc_poli_", Sys.Date(), ".tif"))

maps_sgfcm <- mapClusters(object = SGFCM_result, undecided = 0.2)

# plotting the most likely categories
sgfcm_undecided <- maps_sgfcm$ClusterPlot + theme(legend.position = "bottom") + 
  scale_fill_brewer(palette = "Set3")
ggsave(filename = here::here(paste0("figures/sgfcm_undecided_cluster_", Sys.Date(), ".png")),
       plot = sgfcm_undecided, 
       width = 12, height = 12, dpi = 300)

# violin plots
violinPlots(df_all_zsc, SGFCM_result$Groups)

## looking at alpha and beta for SGFCM
future::plan(future::multisession(workers=2))
DFindices_SGFCM <- select_parameters.mc(algo = "SGFCM", data = dataset_pmrc_poli,
                                       k = 8, m = 1.625, 
                                       beta = seq(0,1.0,0.1), alpha = seq(0,2,0.1),
                                       window = w1, spconsist = TRUE, nrep = 5, 
                                       verbose = TRUE, chunk_size = 4,
                                       seed = 456, init = "kpp",
                                       indices = c("XieBeni.index", "Explained.inertia",
                                                   "Negentropy.index", "Silhouette.index"))
#write_csv(DFindices_SGFCM, here::here(paste0("outputs/sgfcm_pmrc_poli_indices_alpha_beta_", Sys.Date(), ".csv")), append = FALSE)
DFindices_SGFCM <- read_csv(here::here("outputs/sgfcm_pmrc_poli_indices_alpha_beta_2024-08-22.csv"))

ggplot(DFindices_SGFCM) + 
  geom_raster(aes(x = alpha, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(Silhouette.index,2)), size = 2.0)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
# alpha = 1.3 and beta = 0.1 look good. Could also try alpha of 1.6
ggplot(DFindices_SFGCM) + 
  geom_raster(aes(x = alpha, y = beta, fill = XieBeni.index),  size = 5) + 
  scale_fill_viridis() +
  coord_fixed(ratio=1)
ggplot(DFindices_SFGCM) + 
  geom_raster(aes(x = alpha, y = beta, fill = spConsistency),  size = 5) + 
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# want to try alpha of 1.6 too (will result in more spatial smoothing)
SGFCM_result_a1.6 <- SGFCMeans(dataset_pmrc_poli, k = 8, m = 1.625, standardize = FALSE,
                          lag_method = "mean",
                          window = w1, alpha = 1.6, beta = 0.1,
                          seed = 6891, tol = 0.001, verbose = FALSE, init = "kpp")

maps_a1.6 <- mapClusters(object = SGFCM_result_a1.6, undecided = 0.2)

# plotting the most likely categories
maps_a1.6$ClusterPlot + theme(legend.position = "bottom") + 
  scale_fill_brewer(palette = "Set3")


# Compare belongings between SFCM and SGFCM

SFCMMaps <- mapClusters(object = SFCM_result, undecided = 0.45)
SGFCMMaps <- mapClusters(object = SGFCM_result, undecided = 0.45)

require(gridExtra)
grid.arrange(SFCMMaps$ProbaMaps[[1]],SGFCMMaps$ProbaMaps[[1]],
             ncol = 2)
grid.arrange(SFCMMaps$ProbaMaps[[2]],SGFCMMaps$ProbaMaps[[2]],
             ncol = 2)

#----more finished looking maps and comparing with FCM----

# Compare to FCM 
FCM_result_pmrc_poli <- CMeans(dataset_pmrc_poli, k = 8, m = 1.625, 
                               standardize = FALSE, seed = 6891, 
                               tol = 0.001)

maps_fcm <- mapClusters(object = FCM_result_pmrc_poli, undecided = 0.2)
maps_fcm$ClusterPlot + theme(legend.position = "bottom") + 
  scale_fill_brewer(palette = "Set3")

# Spatial Diagnostic
diagSGFCM <- spatialDiag(SGFCM_result, window = matrix(1, nrow = 3, ncol = 3), nrep = 5)

knitr::kable(diagSGFCM$MoranValues, digits = 3, row.names = FALSE)

# Shannon entropy index
df_all$entropyidx  <- calcUncertaintyIndex(SGFCM_result$Belongings)

ggplot(df_all, aes(x = x, y = y)) + 
  geom_point(aes(colour = entropyidx))

# calculating the local Moran I values
loc_moran1 <- calc_local_moran_raster(SGFCM_result$rasters$group1,w1)
loc_moran2 <- calc_local_moran_raster(SGFCM_result$rasters$group2,w1)
loc_moran3 <- calc_local_moran_raster(SGFCM_result$rasters$group3,w1)
loc_moran4 <- calc_local_moran_raster(SGFCM_result$rasters$group4,w1)
loc_moran5 <- calc_local_moran_raster(SGFCM_result$rasters$group5,w1)
loc_moran6 <- calc_local_moran_raster(SGFCM_result$rasters$group6,w1)
loc_moran7 <- calc_local_moran_raster(SGFCM_result$rasters$group7,w1)
loc_moran8 <- calc_local_moran_raster(SGFCM_result$rasters$group8,w1)

# mapping the values
cols <- rev(RColorBrewer::brewer.pal(n = 8, "Spectral"))
vals <- terra::values(loc_moran8, mat = FALSE) 
limits <- classIntervals(vals,  n = 8, style = "kmeans") 
plot(loc_moran8, col = cols, breaks = limits$brks)
#sgfcm_lm1 <- plot(loc_moran1, col = cols, breaks = limits$brks)

# mapping the values using ELSA
cols <- RColorBrewer::brewer.pal(n = 7, "Greys")
vals <- terra::values(diagSGFCM$Elsa, mat = FALSE)
limits <- classIntervals(vals[!is.na(vals)],  n = 7, style = "kmeans") 
plot(diagSGFCM$Elsa, col = cols, breaks = limits$brks)

# Fuzzy ELSA
fuzzy_elsa_rast <- calcFuzzyELSA(SGFCM_result,window = matrix(1,nrow = 3, ncol = 3))

cols <- RColorBrewer::brewer.pal(n = 7, "Greys")
vals <- terra::values(fuzzy_elsa_rast, mat = FALSE)
limits <- classIntervals(vals[!is.na(vals)],  n = 7, style = "kmeans") 
plot(fuzzy_elsa_rast, col = cols, breaks = limits$brks)

# Load the data
fs_nf <- st_read("data/original/S_USA.AdministrativeForest.shp")
fs_reg <- st_read("data/original/S_USA.AdministrativeRegion.shp")
projection <- "epsg: 5070"

fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.crop <- st_crop(fs_reg.proj, ext(rst_fcm_pmrc_poli_sc))


fcm.pmrc.poli.df <- FCM_result_pmrc_poli$Groups %>% as.data.frame(xy = TRUE)

pmrc_poli_rg_nf_map <- ggplot() +
  geom_raster(aes(x = fcm.pmrc.poli.df$x, y = fcm.pmrc.poli.df$y, fill = as.factor(fcm.pmrc.poli.df$Groups))) +
  geom_sf(data = fs_nf.proj, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "PMRC with AIP: k=8, m=1.625", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

pmrc_poli_rg_nf_map

sgfcm.pmrc.poli.df <- SGFCM_result$Groups %>% as.data.frame(xy = TRUE)

sgfcm_nfreg_map <- ggplot() +
  geom_raster(aes(x = sgfcm.pmrc.poli.df$x, y = sgfcm.pmrc.poli.df$y, fill = as.factor(sgfcm.pmrc.poli.df$Groups))) +
  geom_sf(data = fs_nf.proj, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "PMRC with AIP: k=8, m=1.625", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

sgfcm_nfreg_map




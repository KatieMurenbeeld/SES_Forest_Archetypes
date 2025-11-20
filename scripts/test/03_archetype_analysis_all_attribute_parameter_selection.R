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

options(future.globals.maxSize = 8000 * 1024^2)

#----Load the data----
rst_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")
rst <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")


# Use terra::k_means() to make elbow plots?
# or stats::kmeans() -- I need to use this in order to get the R2 and inertia

df_nat <- as.data.frame(rst_sc, na.rm=FALSE) # Extract values including NAs
df_nat_complete <- na.omit(df_nat)           # Remove rows with any NA values

R2s <- sapply(2:100, function(k){
  Clust <- k_means(df_nat_complete, centers=k, iter.max = 150)
  R2 <- Clust$betweenss / Clust$totss
  return(R2)
})

Df_r2 <- data.frame(K=2:100,
                    R2 = R2s)

k_r2 <- ggplot(Df_r2)+
  geom_line(aes(x=K,y=R2s))+
  geom_point(aes(x=K,y=R2s),color="red")+
  xlab("Number of groups")+
  ylab("R2 of classification")
k_r2
ggsave(here::here(paste0("outputs/plots/national_level_all_param_selection_kmeans_r2_k100_", 
                         Sys.Date(), ".jpeg")), 
       plot = k_r2, height = 6, width = 10, dpi = 300)

INERTs <- sapply(2:100,function(k){
  Clust <- kmeans(df_nat_complete, centers=k, iter.max = 150)
  INERT <- Clust$tot.withinss
  return(INERT)
})

Df_INERT <- data.frame(K=2:100,
                       INERT = INERTs)

k_inert <- ggplot(Df_INERT)+
  geom_line(aes(x=K,y=INERTs))+
  geom_point(aes(x=K,y=INERTs),color="red")+
  xlab("Number of groups")+
  ylab("Inertia (within cluster sum-of-squares) of classification")
k_inert
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_kmeans_inertia_k100_", 
                         Sys.Date(), ".jpeg")), 
       plot = k_inert, height = 6, width = 10, dpi = 300)


# Format for use in geocmeans
dataset <- lapply(names(rst_sc), function(n){
  aband <- rst_sc[[n]]
  return(aband)
})
names(dataset) <- names(rst_sc)

#----Use a non spatial and non generalized c-means to determine number of k 
## set m = 1
future::plan(future::multisession(workers = 2))
FCMvalues_m1 <- select_parameters.mc(algo = "FCM", data = dataset, standardize = FALSE,
                                  k = 2:100, m = 1, spconsist = FALSE, 
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Negentropy.index", "Silhouette.index"),
                                  seed = 1234, verbose = TRUE) 

write_csv(FCMvalues_m1, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/cm_all_attri_param_indices_k2_100_",
                            Sys.Date(), ".csv"), append = FALSE)

# plotting the silhouette index
fcm_si <- ggplot(FCMvalues_m1) + 
  geom_raster(aes(x = k, y = m, fill = Silhouette.index)) + 
  geom_text(aes(x = k, y = m, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=2)
fcm_si
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_cm_si_k2_100_m1_", 
                         Sys.Date(), ".jpeg")), 
       plot = fcm_si, height = 6, width = 10, dpi = 300)
# plotting the Xie Beni index
fcm_xb <- ggplot(FCMvalues_m1) + 
  geom_raster(aes(x = k, y = m, fill = XieBeni.index)) + 
  geom_text(aes(x = k, y = m, label = round(XieBeni.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=2)
fcm_xb
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_cm_xb_k2_100_m1_", 
                         Sys.Date(), ".jpeg")), 
       plot = fcm_xb, height = 6, width = 10, dpi = 300)
# plotting the Explained Inertia
fcm_ei <- ggplot(FCMvalues_m1) + 
  geom_raster(aes(x = k, y = m, fill = Explained.inertia)) + 
  geom_text(aes(x = k, y = m, label = round(Explained.inertia,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=2)
fcm_ei
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_cm_ei_k2_100_m1_", 
                         Sys.Date(), ".jpeg")), 
       plot = fcm_ei, height = 6, width = 10, dpi = 300)

#----Use a non spatial and non generalized fuzzy c-means to determine number of k and value for m
future::plan(future::multisession(workers = 2))
FCMvalues <- select_parameters.mc(algo = "FCM", data = dataset, standardize = FALSE,
                                  k = 2:20, m = seq(1.1,2,0.1), spconsist = FALSE, 
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Negentropy.index", "Silhouette.index"),
                                  seed = 1234, verbose = TRUE) 

write_csv(FCMvalues, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/fcm_all_attri_param_indices_k2_20_",
                            Sys.Date(), ".csv"), append = FALSE)

# plotting the silhouette index
fcm_si <- ggplot(FCMvalues) + 
  geom_raster(aes(x = k, y = m, fill = Silhouette.index)) + 
  geom_text(aes(x = k, y = m, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=2)
fcm_si
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_fcm_si_k2_20_", 
                  Sys.Date(), ".jpeg")), 
       plot = fcm_si, height = 6, width = 10, dpi = 300)
# plotting the Xie Beni index
fcm_xb <- ggplot(FCMvalues) + 
  geom_raster(aes(x = k, y = m, fill = XieBeni.index)) + 
  geom_text(aes(x = k, y = m, label = round(XieBeni.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=2)
fcm_xb
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_fcm_xb_k2_20_", 
                  Sys.Date(), ".jpeg")), 
       plot = fcm_xb, height = 6, width = 10, dpi = 300)
# plotting the Explained Inertia
fcm_ei <- ggplot(FCMvalues) + 
  geom_raster(aes(x = k, y = m, fill = Explained.inertia)) + 
  geom_text(aes(x = k, y = m, label = round(Explained.inertia,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=2)
fcm_ei
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_fcm_ei_k2_20_", 
                         Sys.Date(), ".jpeg")), 
       plot = fcm_ei, height = 6, width = 10, dpi = 300)

# Test out up to 50 clusters
FCMvalues_k21_50 <- select_parameters.mc(algo = "FCM", data = dataset, standardize = FALSE,
                                  k = 21:50, m = seq(1.1,2,0.1), spconsist = FALSE, 
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Negentropy.index", "Silhouette.index"),
                                  seed = 1234, verbose = TRUE) 

write_csv(FCMvalues_k21_50, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/fcm_all_attri_param_indices_k21_50_",
                            Sys.Date(), ".csv"), append = FALSE)

# plotting the silhouette index
fcm_si <- ggplot(FCMvalues_k21_50) + 
  geom_raster(aes(x = k, y = m, fill = Silhouette.index)) + 
  geom_text(aes(x = k, y = m, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=2)
fcm_si
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_fcm_si_k21_50_", 
                         Sys.Date(), ".jpeg")), 
       plot = fcm_si, height = 6, width = 10, dpi = 300)

# plotting the Explained Inertia
fcm_ei <- ggplot(FCMvalues_k21_50) + 
  geom_raster(aes(x = k, y = m, fill = Explained.inertia)) + 
  geom_text(aes(x = k, y = m, label = round(Explained.inertia,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=2)
fcm_ei
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_fcm_ei_k21_50_", 
                         Sys.Date(), ".jpeg")), 
       plot = fcm_ei, height = 6, width = 10, dpi = 300)

# Some options
# k = 6, m = 1.6, SI = 0.44
# k = 7, m = 1.7, SI = 0.44
# k = 15, m = 1.8, SI = 0.48 relatively high XB
# k = 15, m = 1.9, SI = 0.44 relatively high XB
# k = 24, m = 1.9, SI = 0.44

#----Use a generalized fuzzy c-means to determine the value for and beta

GFCMvalues <- select_parameters.mc(algo = "GFCM", data = dataset, 
                                   standardize = FALSE, seed = 6891,
                                   k = c(6, 7, 15, 15, 24), 
                                   m = c(1.6, 1.7, 1.8, 1.9, 1.9), 
                                   beta = seq(0.1,0.9,0.1),
                                   spconsist = FALSE, verbose = TRUE, 
                                   init = "kpp",
                                   indices = c("XieBeni.index", 
                                               "Explained.inertia",
                                               "Negentropy.index", 
                                               "Silhouette.index"))  

write_csv(GFCMvalues, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/gfcm_all_attri_param_indices_",
                            Sys.Date(), ".csv"), append = FALSE)


# plotting the silhouette index
gfcm_si <- ggplot(GFCMvalues) + 
  geom_raster(aes(x = m, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = beta, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
gfcm_si
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_gfcm_si_", 
                         Sys.Date(), ".jpeg")), 
       plot = gfcm_si, height = 6, width = 10, dpi = 300)
# plotting the Xie Beni
gfcm_xb <- ggplot(GFCMvalues) + 
  geom_raster(aes(x = m, y = beta, fill = XieBeni.index)) + 
  geom_text(aes(x = m, y = beta, label = round(XieBeni.index, 2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
gfcm_xb
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_gfcm_xb_", 
                         Sys.Date(), ".jpeg")), 
       plot = gfcm_xb, height = 6, width = 10, dpi = 300)
# plotting the Explained Inertia
gfcm_ei <- ggplot(GFCMvalues) + 
  geom_raster(aes(x = m, y = beta, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = beta, label = round(Explained.inertia, 2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
gfcm_ei
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_gfcm_ei_", 
                         Sys.Date(), ".jpeg")), 
       plot = gfcm_ei, height = 6, width = 10, dpi = 300)


# Spatial FCM
w1 <- matrix(1, nrow = 3, ncol = 3)
w2 <- matrix(1, nrow = 5, ncol = 5)
w3 <- matrix(1, nrow = 7, ncol = 7)

# k = 6
SFCMvalues_k6 <- select_parameters.mc(algo = "SFCM", data = dataset, 
                                      standardize = FALSE,
                                      k = 6, m = c(1.8, 1.9),
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


# k = 7
SFCMvalues_k7 <- select_parameters.mc(algo = "SFCM", data = dataset, 
                                      standardize = FALSE, 
                                      k = 7, m = 1.9,
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

SFCMvalues_k7$window <- dict$window[match(SFCMvalues_k7$window,dict$w)]
write_csv(SFCMvalues_k7, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/sfcm_all_attri_param_indices_k8_", 
                                Sys.Date(), ".csv"), append = FALSE)


# plotting the silhouette index
ggplot(SFCMvalues_k7) + 
  geom_raster(aes(x = alpha, y = window, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = window, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.5)

# plotting the Xie Beni
ggplot(SFCMvalues_k7) + 
  geom_raster(aes(x = alpha, y = window, fill = XieBeni.index)) + 
  geom_text(aes(x = alpha, y = window, label = round(XieBeni.index, 2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.5)

# k = 15
SFCMvalues_k15 <- select_parameters.mc(algo = "SFCM", data = dataset, 
                                      standardize = FALSE, 
                                      k = 15, m = 1.8,
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

SFCMvalues_k15$window <- dict$window[match(SFCMvalues_k15$window,dict$w)]
write_csv(SFCMvalues_k15, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/outputs/sfcm_all_attri_param_indices_k15_", 
                                Sys.Date(), ".csv"), append = FALSE)


# plotting the silhouette index
ggplot(SFCMvalues_k15) + 
  geom_raster(aes(x = alpha, y = window, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = window, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.5)

# plotting the Xie Beni
ggplot(SFCMvalues_k15) + 
  geom_raster(aes(x = alpha, y = window, fill = XieBeni.index)) + 
  geom_text(aes(x = alpha, y = window, label = round(XieBeni.index, 2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.5)

# Spatial GFCM
#-------------------------------------------------------------------------------
future::plan(future::multisession(workers = 2))
# k = 6 and 7, window = w1 (3x3)
SGFCMvalues_w1 <- select_parameters.mc(algo = "SGFCM", data = dataset,
                                       standardize = FALSE, 
                                    k = 6:7, m = 1.8:1.9,
                                    beta = seq(0.1,0.9,0.1), alpha = seq(0.5,2,0.1),
                                    window = w1,
                                    spconsist = TRUE, nrep = 5, 
                                    verbose = TRUE, chunk_size = 4,
                                    seed = 456, init = "kpp",
                                    indices = c("XieBeni.index", "Explained.inertia",
                                                "Negentropy.index", "Silhouette.index"))



write_csv(SGFCMvalues_w1, here::here(paste0("outputs/sgfcm_all_attri_param_indices_k6-7_w1_", 
                                            Sys.Date(), ".csv")), append = FALSE)

# showing the silhouette index
sgfcm_si <- ggplot(SGFCMvalues_w1) + 
  geom_raster(aes(x = alpha, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(Silhouette.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
sgfcm_si
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_sgfcm_si_", 
                         Sys.Date(), ".jpeg")), 
       plot = sgfcm_si, height = 6, width = 10, dpi = 300)
# showing the spatial inconsistency
ggplot(SGFCMvalues_w1) + 
  geom_raster(aes(x = alpha, y = beta, fill = spConsistency)) + 
  geom_text(aes(x = alpha, y = beta, label = round(spConsistency,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# showing the Xie Beni index
sgfcm_xb <- ggplot(SGFCMvalues_w1) + 
  geom_raster(aes(x = alpha, y = beta, fill = XieBeni.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(XieBeni.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
sgfcm_xb 
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_sgfcm_xb_", 
                         Sys.Date(), ".jpeg")), 
       plot = sgfcm_xb, height = 6, width = 10, dpi = 300)

# k = 6 and 7, window = w2 (5x5)
SGFCMvalues_w2 <- select_parameters.mc(algo = "SGFCM", data = dataset,
                                       standardize = FALSE, 
                                       k = 6:7, m = 1.8:1.9,
                                       beta = seq(0.1,0.9,0.1), alpha = seq(0.5,2,0.1),
                                       window = w2,
                                       spconsist = TRUE, nrep = 5, 
                                       verbose = TRUE, chunk_size = 4,
                                       seed = 456, init = "kpp",
                                       indices = c("XieBeni.index", "Explained.inertia",
                                                   "Negentropy.index", "Silhouette.index"))



write_csv(SGFCMvalues_w2, here::here(paste0("outputs/sgfcm_all_attri_param_indices_k6-7_w2_", 
                                            Sys.Date(), ".csv")), append = FALSE)

# showing the silhouette index
sgfcm_si <- ggplot(SGFCMvalues_w2) + 
  geom_raster(aes(x = alpha, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(Silhouette.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
sgfcm_si
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_sgfcm_si_", 
                         Sys.Date(), ".jpeg")), 
       plot = sgfcm_si, height = 6, width = 10, dpi = 300)
# showing the spatial inconsistency
ggplot(SGFCMvalues_w2) + 
  geom_raster(aes(x = alpha, y = beta, fill = spConsistency)) + 
  geom_text(aes(x = alpha, y = beta, label = round(spConsistency,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# showing the Xie Beni index
sgfcm_xb <- ggplot(SGFCMvalues_w2) + 
  geom_raster(aes(x = alpha, y = beta, fill = XieBeni.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(XieBeni.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
sgfcm_xb 
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_sgfcm_xb_", 
                         Sys.Date(), ".jpeg")), 
       plot = sgfcm_xb, height = 6, width = 10, dpi = 300)

# k = 6 and 7, window = w3 (7x7)
SGFCMvalues_w3 <- select_parameters.mc(algo = "SGFCM", data = dataset,
                                       standardize = FALSE, 
                                       k = 6:7, m = 1.8:1.9,
                                       beta = seq(0.1,0.9,0.1), alpha = seq(0.5,2,0.1),
                                       window = w3,
                                       spconsist = TRUE, nrep = 5, 
                                       verbose = TRUE, chunk_size = 4,
                                       seed = 456, init = "kpp",
                                       indices = c("XieBeni.index", "Explained.inertia",
                                                   "Negentropy.index", "Silhouette.index"))



write_csv(SGFCMvalues_w3, here::here(paste0("outputs/sgfcm_all_attri_param_indices_k6-7_w3_", 
                                            Sys.Date(), ".csv")), append = FALSE)

# showing the silhouette index
sgfcm_si <- ggplot(SGFCMvalues_w3) + 
  geom_raster(aes(x = alpha, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(Silhouette.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
sgfcm_si
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_sgfcm_si_", 
                         Sys.Date(), ".jpeg")), 
       plot = sgfcm_si, height = 6, width = 10, dpi = 300)
# showing the spatial inconsistency
ggplot(SGFCMvalues_w3) + 
  geom_raster(aes(x = alpha, y = beta, fill = spConsistency)) + 
  geom_text(aes(x = alpha, y = beta, label = round(spConsistency,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# showing the Xie Beni index
sgfcm_xb <- ggplot(SGFCMvalues_w3) + 
  geom_raster(aes(x = alpha, y = beta, fill = XieBeni.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(XieBeni.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
sgfcm_xb 
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_sgfcm_xb_", 
                         Sys.Date(), ".jpeg")), 
       plot = sgfcm_xb, height = 6, width = 10, dpi = 300)


# k = 15 window = w1 (3x3)
SGFCMvalues_k15_w1 <- select_parameters.mc(algo = "SGFCM", data = dataset,
                                       standardize = FALSE, 
                                       k = 15, m = 1.8,
                                       beta = seq(0.1,0.9,0.1), alpha = seq(0.5,2,0.1),
                                       window = w1,
                                       spconsist = TRUE, nrep = 5, 
                                       verbose = TRUE, chunk_size = 4,
                                       seed = 456, init = "kpp",
                                       indices = c("XieBeni.index", "Explained.inertia",
                                                   "Negentropy.index", "Silhouette.index"))



write_csv(SGFCMvalues_k15_w1, here::here(paste0("outputs/sgfcm_all_attri_param_indices_k15_w1_", 
                                            Sys.Date(), ".csv")), append = FALSE)

# showing the silhouette index
sgfcm_si <- ggplot(SGFCMvalues_k15_w1) + 
  geom_raster(aes(x = alpha, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(Silhouette.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
sgfcm_si
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_sgfcm_si_k15_w1_", 
                         Sys.Date(), ".jpeg")), 
       plot = sgfcm_si, height = 6, width = 10, dpi = 300)
# showing the spatial inconsistency
ggplot(SGFCMvalues_k15_w1) + 
  geom_raster(aes(x = alpha, y = beta, fill = spConsistency)) + 
  geom_text(aes(x = alpha, y = beta, label = round(spConsistency,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# showing the Xie Beni index
sgfcm_xb <- ggplot(SGFCMvalues_k15_w1) + 
  geom_raster(aes(x = alpha, y = beta, fill = XieBeni.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(XieBeni.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
sgfcm_xb 
ggsave(here::here(paste0("outputs/plots/appen_a_param_selection_sgfcm_xb_k15_w1_", 
                         Sys.Date(), ".jpeg")), 
       plot = sgfcm_xb, height = 6, width = 10, dpi = 300)

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




# code from google AI response to "R terra::kmeans na.omit not working"
library(tidyverse)
library(terra)
library(geocmeans)
library(sf)
library(here)
library(viridis)

# Example SpatRaster with NAs (replace with your data)
r <- rast(here::here("data/processed/nf_buffers_all_attributes_cropped_then_scaled_2025-11-06.tif"))

#plot(r$aip)

# 1. Extract values to a data frame and remove NAs
## but maybe I don't need to do this? According to the geocmeans reference
## pixels with NAs are not considered in the classification.
df <- as.data.frame(r, na.rm=FALSE) # Extract values including NAs
df_complete <- na.omit(df)           # Remove rows with any NA values

# 2. Run k-means on the complete data frame
#k_clusters <- kmeans(df_complete, centers = 5, iter.max = 100, nstart = 10)
R2s <- sapply(2:154, function(k){
  Clust <- kmeans(df_complete, centers=k, iter.max = 150)
  R2 <- Clust$betweenss / Clust$totss
  return(R2)
})

Df_r2 <- data.frame(K=2:154,
                    R2 = R2s)

k_r2 <- ggplot(Df_r2)+
  geom_line(aes(x=K,y=R2s))+
  geom_point(aes(x=K,y=R2s),color="red")+
  xlab("Number of groups")+
  ylab("R2 of classification")
k_r2
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_kmeans_r2_k154_no_na_", 
                             Sys.Date(), ".jpeg")), 
           plot = k_r2, height = 6, width = 10, dpi = 300)

INERTs <- sapply(2:154,function(k){
  Clust <- kmeans(na.omit(df_complete), centers=k, iter.max = 150)
  INERT <- Clust$tot.withinss
  return(INERT)
})

Df_INERT <- data.frame(K=2:154,
                       INERT = INERTs)

k_inert <- ggplot(Df_INERT)+
  geom_line(aes(x=K,y=INERTs))+
  geom_point(aes(x=K,y=INERTs),color="red")+
  xlab("Number of groups")+
  ylab("Inertia (within cluster sum-of-squares) of classification")
k_inert
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_kmeans_inertia_k154_no_na_", 
                         Sys.Date(), ".jpeg")), 
       plot = k_inert, height = 6, width = 10, dpi = 300)

k_clusters <- kmeans(df_complete, centers = 20, iter.max = 100, nstart = 10)
# 3. Associate clusters back to the original data frame (this is the key step)
# Create a vector for all cells, initialized to NA
clusters_all <- rep(NA, nrow(df))
# Fill in the cluster assignments for the non-NA cases
clusters_all[as.numeric(rownames(df_complete))] <- k_clusters$cluster

# 4. Create a new SpatRaster with the cluster results
r_clusters <- rast(r, nlyr=1) # Create an empty SpatRaster with same dimensions
values(r_clusters) <- clusters_all
names(r_clusters) <- "cluster"

# Plot the results
plot(r_clusters, main="K-means Clusters (NA values removed)")

#----But will this work for geocmeans?------------------------------------------
dataset_test <- lapply(names(df_complete), function(n){
  aband <- df_complete[[n]]
  return(aband)
})
names(dataset_test) <- names(df_complete)

dataset <- lapply(names(r), function(n){
  aband <- r[[n]]
  return(aband)
})
names(dataset) <- names(r)

## I don't need to do the above ^^ can use df_complete ##


## just a quick test with k = 2:5 and m = seq(1.1, 2, 0.5)
#----Use a non spatial and non generalized fuzzy c-means to determine number of k and value for m
future::plan(future::multisession(workers = 2))
FCMvalues <- select_parameters.mc(algo = "FCM", data = dataset, standardize = FALSE,
                                  k = 2:50, m = seq(1.1,2,0.1), spconsist = FALSE, 
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Silhouette.index",
                                              "Negentropy.index", "DaviesBoulin.index"),
                                  seed = 1234, verbose = TRUE) 

# could not use Silhouette.index with a dataframe
# warning("impossible to calculate Silhouette index with fclust::SIL.F, This is
# most likely due to a large dataset. We use here an approximation by subsampling..."
# see lines 17-25 https://github.com/JeremyGelb/geocmeans/blob/master/R/clustering_evaluation.R

write_csv(FCMvalues, here::here(paste0("outputs/fcm_nfbuffers_all_attri_param_indices_crop_then_scale_no_na_",
                                       Sys.Date(), ".csv")), append = FALSE)

fcm_ei <- ggplot(FCMvalues) + 
  geom_raster(aes(x = k, y = m, fill = Explained.inertia)) + 
  geom_text(aes(x = k, y = m, label = round(Explained.inertia,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=15)
fcm_ei
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_fcm_ei_no_na_test_", 
                         Sys.Date(), ".jpeg")), 
       plot = fcm_ei, height = 8, width = 15, dpi = 300)

fcm_db <- ggplot(FCMvalues) + 
  geom_raster(aes(x = k, y = m, fill = DaviesBoulin.index)) + 
  geom_text(aes(x = k, y = m, label = round(DaviesBoulin.index,0)), size = 1.75)+
  scale_fill_viridis() +
  coord_fixed(ratio=18)
fcm_db
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_fcm_db_no_na_test_", 
                         Sys.Date(), ".jpeg")), 
       plot = fcm_db, height = 8, width = 15, dpi = 300)

fcm_ne <- ggplot(FCMvalues) + 
  geom_raster(aes(x = k, y = m, fill = Negentropy.index)) + 
  geom_text(aes(x = k, y = m, label = round(Negentropy.index,0)), size = 1.75)+
  scale_fill_viridis() +
  coord_fixed(ratio=18)
fcm_ne
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_fcm_ne_no_na_test_", 
                         Sys.Date(), ".jpeg")), 
       plot = fcm_ne, height = 8, width = 15, dpi = 300)

fcm_xb <- ggplot(FCMvalues) + 
  geom_raster(aes(x = k, y = m, fill = XieBeni.index)) + 
  geom_text(aes(x = k, y = m, label = round(XieBeni.index, 0)), size = 1.75)+
  scale_fill_viridis() +
  coord_fixed(ratio=18)
fcm_xb
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_fcm_xb_no_na_test_", 
                         Sys.Date(), ".jpeg")), 
       plot = fcm_xb, height = 8, width = 15, dpi = 300)

fcm_si <- ggplot(FCMvalues) + 
  geom_raster(aes(x = k, y = m, fill = Silhouette.index)) + 
  geom_text(aes(x = k, y = m, label = round(Silhouette.index, 2)), size = 1.75)+
  scale_fill_viridis() +
  coord_fixed(ratio=18)
fcm_si
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_fcm_si_no_na_test_", 
                         Sys.Date(), ".jpeg")), 
       plot = fcm_si, height = 8, width = 15, dpi = 300)

# For looking at the Xie-Beni Index set m = 2 and test for k

future::plan(future::multisession(workers = 2))
FCMvalues_m2 <- select_parameters.mc(algo = "FCM", data = df_complete, standardize = FALSE,
                                  k = 2:50, m = 2, spconsist = FALSE, 
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Negentropy.index", "DaviesBoulin.index"),
                                  seed = 1234, verbose = TRUE)

fcm_m2_xb <- ggplot(FCMvalues_m2) + 
  geom_raster(aes(x = k, y = m, fill = XieBeni.index)) + 
  geom_text(aes(x = k, y = m, label = round(XieBeni.index, 0)), size = 1.75)+
  scale_fill_viridis() +
  coord_fixed(ratio=8)
fcm_m2_xb
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_fcm_xb_m2_no_na_test_", 
                         Sys.Date(), ".jpeg")), 
       plot = fcm_xb, height = 8, width = 15, dpi = 300)

# test out mapping the clusters back to the raster
FCM_result <- CMeans(df_complete, k = 18, m = 1.2, standardize = FALSE,
                      seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")

# Create a vector for all cells, initialized to NA
fcm_clusters_all <- rep(NA, nrow(df))
# Fill in the cluster assignments for the non-NA cases
fcm_clusters_all[as.numeric(rownames(df_complete))] <- FCM_result$Groups

# 4. Create a new SpatRaster with the cluster results
r_fcm_clusters <- rast(r, nlyr=1) # Create an empty SpatRaster with same dimensions
values(r_fcm_clusters) <- fcm_clusters_all
names(r_fcm_clusters) <- "cluster"

# Plot the results
fcm_plot <- plot(r_fcm_clusters, main="Fuzzy C-Means Clusters (NA values removed)")

## IT WORKED!! ##

# Try a spatial fcm
#-------------------------------------------------------------------------------
## if using a dataframe instead of the raster, the window needs to be 
## a list.w object from the spdep package
w1 <- matrix(1, nrow = 3, ncol = 3)
w2 <- matrix(1, nrow = 5, ncol = 5)
w3 <- matrix(1, nrow = 7, ncol = 7)

SFCMvalues_k2_20_w1_m1.5 <- select_parameters.mc(algo = "SFCM", data = dataset, 
                                         standardize = FALSE,
                                         k = 2:20, m = 1.5,
                                         alpha = seq(0.1,2,0.5),
                                         window = w1,
                                         spconsist = TRUE, nrep = 5, 
                                         verbose = TRUE, chunk_size = 4,
                                         seed = 6891, init = "kpp",
                                         indices = c("XieBeni.index", 
                                                     "Explained.inertia",
                                                     "Silhouette.index",
                                                     "Negentropy.index", 
                                                     "DaviesBoulin.index"))


dict <- data.frame(
  w = c(1),
  window = c("3x3")
)

SFCMvalues_k2_20_w1_m1.5$window <- dict$window[match(SFCMvalues_k2_20_w1_m1.5$window,dict$w)]
write_csv(SFCMvalues_k2_20_w1_m1.5, here::here(paste0("outputs/nfbuffers_sfcm_all_attri_param_indices_k2_20_w1_m15_", 
                                           Sys.Date(), ".csv")), append = FALSE)

sfcm_si <- ggplot(SFCMvalues_k2_20_w1_m1.5) + 
  geom_raster(aes(x = k, y = alpha, fill = Silhouette.index)) + 
  geom_text(aes(x = k, y = alpha, label = round(Silhouette.index, 2)), size = 1.75)+
  scale_fill_viridis() +
  coord_fixed(ratio=18)
sfcm_si
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_sfcm_si_k2_20_w1_m15_", 
                         Sys.Date(), ".jpeg")), 
       plot = fcm_si, height = 8, width = 15, dpi = 300)

SFCMvalues_k2_20_w3_m1.5 <- select_parameters.mc(algo = "SFCM", data = dataset, 
                                                 standardize = FALSE,
                                                 k = 2:20, m = 1.5,
                                                 alpha = seq(0.1,2,0.5),
                                                 window = w3,
                                                 spconsist = TRUE, nrep = 5, 
                                                 verbose = TRUE, chunk_size = 4,
                                                 seed = 6891, init = "kpp",
                                                 indices = c("XieBeni.index", 
                                                             "Explained.inertia",
                                                             "Silhouette.index",
                                                             "Negentropy.index", 
                                                             "DaviesBoulin.index"))

sfcm_si <- ggplot(SFCMvalues_k2_20_w3_m1.5) + 
  geom_raster(aes(x = k, y = alpha, fill = Silhouette.index)) + 
  geom_text(aes(x = k, y = alpha, label = round(Silhouette.index, 2)), size = 1.75)+
  scale_fill_viridis() +
  coord_fixed(ratio=18)
sfcm_si
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_sfcm_si_k2_20_w3_m15_", 
                         Sys.Date(), ".jpeg")), 
       plot = fcm_si, height = 8, width = 15, dpi = 300)

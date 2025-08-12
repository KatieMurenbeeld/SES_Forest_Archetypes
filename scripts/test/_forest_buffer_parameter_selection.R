library(tidyverse)
library(terra)
library(geocmeans)
library(sf)
library(here)
library(viridis)

# 1. Load the attribute raster stack and forest + buffer shape file

## Attribute raster stack
df_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")

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
fs_reg.crop <- st_crop(fs_reg.proj, ext(df_attri))

## create a new nation forest buffers shape using st_intersection
nf_buffers <- st_intersection(nf_sf, fs_reg.crop)

# 2.Crop the raster to the nf_buffers

attri_crop <- crop(df_attri, nf_buffers, mask = TRUE)

# 3. Format for use in geocmeans
dataset <- lapply(names(attri_crop), function(n){
  aband <- attri_crop[[n]]
  return(aband)
})
names(dataset) <- names(attri_crop)

#----Use a non spatial and non generalized fuzzy c-means to determine number of k and value for m
future::plan(future::multisession(workers = 2))
FCMvalues <- select_parameters.mc(algo = "FCM", data = dataset, 
                                  k = 2:10, m = seq(1.1,2,0.1), spconsist = FALSE, 
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Negentropy.index", "Silhouette.index"),
                                  seed = 1234, verbose = TRUE) 

write_csv(FCMvalues, here::here(paste0("outputs/fcm_nfbuffers_all_attri_param_indices_",
                            Sys.Date(), ".csv")), append = FALSE)

# plotting the silhouette index
fcm_si <- ggplot(FCMvalues) + 
  geom_raster(aes(x = k, y = m, fill = Silhouette.index)) + 
  geom_text(aes(x = k, y = m, label = round(Silhouette.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=2)
fcm_si
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_fcm_si_", 
                         Sys.Date(), ".jpeg")), 
       plot = fcm_si, height = 6, width = 10, dpi = 300)
# plotting the Xie Beni index
fcm_xb <- ggplot(FCMvalues) + 
  geom_raster(aes(x = k, y = m, fill = XieBeni.index)) + 
  geom_text(aes(x = k, y = m, label = round(XieBeni.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=2)
fcm_xb
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_fcm_xb_", 
                         Sys.Date(), ".jpeg")), 
       plot = fcm_xb, height = 6, width = 10, dpi = 300)

# FCM seed 1234 SI = 0.32, k = 4, m = 1.5. SI = 0.31, k = 3, m = 1.9

#----Use a generalized fuzzy c-means to determine the value for m and beta
# k = 4
GFCMvalues_k4 <- select_parameters.mc(algo = "GFCM", data = dataset, seed = 6891,
                                      k = 4, m = seq(1.1,2,0.1), beta = seq(0.1,0.9,0.1),
                                      spconsist = FALSE, verbose = TRUE, init = "kpp",
                                      indices = c("XieBeni.index", "Explained.inertia",
                                                  "Negentropy.index", "Silhouette.index"))  

write_csv(GFCMvalues_k4, here::here(paste0("outputs/nfbuffers_gfcm_all_attri_param_indices_k4_",
                                Sys.Date(), ".csv")), append = FALSE)


# plotting the silhouette index
gfcm_si <- ggplot(GFCMvalues_k4) + 
  geom_raster(aes(x = m, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = beta, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
gfcm_si
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_gfcm_k4_si_", 
                         Sys.Date(), ".jpeg")), 
       plot = gfcm_si, height = 6, width = 10, dpi = 300)
# plotting the Xie Beni
gfcm_xb <- ggplot(GFCMvalues_k4) + 
  geom_raster(aes(x = m, y = beta, fill = XieBeni.index)) + 
  geom_text(aes(x = m, y = beta, label = round(XieBeni.index, 2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
gfcm_xb
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_gfcm_k4_xb_", 
                         Sys.Date(), ".jpeg")), 
       plot = gfcm_xb, height = 6, width = 10, dpi = 300)
# Seed = 6891, Silhouette index = 0.33, k = 4, m = 1.7, beta = 0.3 (XB = 24.13)
# Seed = 6891, Silhouette index = 0.32, k = 4, m = 1.5, beta = 0.1 (XB = 12.22)

#--------------------------------
# k = 3
GFCMvalues_k3 <- select_parameters.mc(algo = "GFCM", data = dataset, seed = 6891,
                                      k = 3, m = seq(1.1,2,0.1), beta = seq(0.1,0.9,0.1),
                                      spconsist = FALSE, verbose = TRUE, init = "kpp",
                                      indices = c("XieBeni.index", "Explained.inertia",
                                                  "Negentropy.index", "Silhouette.index"))  

write_csv(GFCMvalues_k3, here::here(paste0("outputs/nfbuffers_gfcm_all_attri_param_indices_k3_",
                                           Sys.Date(), ".csv")), append = FALSE)


# plotting the silhouette index
gfcm_si <- ggplot(GFCMvalues_k3) + 
  geom_raster(aes(x = m, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = beta, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
gfcm_si
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_gfcm_k3_si_", 
                         Sys.Date(), ".jpeg")), 
       plot = gfcm_si, height = 6, width = 10, dpi = 300)
# plotting the Xie Beni
gfcm_xb <- ggplot(GFCMvalues_k3) + 
  geom_raster(aes(x = m, y = beta, fill = XieBeni.index)) + 
  geom_text(aes(x = m, y = beta, label = round(XieBeni.index, 2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
gfcm_xb
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_gfcm_k3_xb_", 
                         Sys.Date(), ".jpeg")), 
       plot = gfcm_xb, height = 6, width = 10, dpi = 300)
# Seed = 6891, Silhouette index = 0.74, k = 3, m = 1.2, beta = 0.3 (XB = 0.22)
# Seed = 6891, Silhouette index = 0.72, k = 3, m = 1.3, beta = 0.7  (XB = 0.2)


#--------------Spatial FCM-------------------
w1 <- matrix(1, nrow = 3, ncol = 3)
w2 <- matrix(1, nrow = 5, ncol = 5)
w3 <- matrix(1, nrow = 7, ncol = 7)

# k = 4, m = 1.7
SFCMvalues_k4 <- select_parameters.mc(algo = "SFCM", data = dataset, 
                                      k = 4, m = 1.7,
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

SFCMvalues_k4$window <- dict$window[match(SFCMvalues_k4$window,dict$w)]
write_csv(SFCMvalues_k4, here::here(paste0("outputs/nfbuffers_sfcm_all_attri_param_indices_k4_", 
                                Sys.Date(), ".csv")), append = FALSE)


# plotting the silhouette index
sfcm_si <- ggplot(SFCMvalues_k4) + 
  geom_raster(aes(x = alpha, y = window, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = window, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.5)
sfcm_si
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_sfcm_si_k4_", 
                         Sys.Date(), ".jpeg")), 
       plot = sfcm_si, height = 6, width = 10, dpi = 300)
# plotting the Xie Beni
sfcm_xb <- ggplot(SFCMvalues_k4) + 
  geom_raster(aes(x = alpha, y = window, fill = XieBeni.index)) + 
  geom_text(aes(x = alpha, y = window, label = round(XieBeni.index, 2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.5)
sfcm_xb
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_sfcm_xb_k4_", 
                         Sys.Date(), ".jpeg")), 
       plot = sfcm_xb, height = 6, width = 10, dpi = 300)

# Seed = 6891, Silhouette index = 0.32, k = 4, m = 1.7, window = 7x7, alpha = 0.1 (XB = 817.95)

#-------------------

# k = 3, m = 1.2

SFCMvalues_k3 <- select_parameters.mc(algo = "SFCM", data = dataset, 
                                      k = 3, m = 1.2,
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

SFCMvalues_k3$window <- dict$window[match(SFCMvalues_k3$window,dict$w)]
write_csv(SFCMvalues_k3, here::here(paste0("outputs/nfbuffers_sfcm_all_attri_param_indices_k3_", 
                                           Sys.Date(), ".csv")), append = FALSE)


# plotting the silhouette index
sfcm_si <- ggplot(SFCMvalues_k3) + 
  geom_raster(aes(x = alpha, y = window, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = window, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.5)
sfcm_si
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_sfcm_si_k3_", 
                         Sys.Date(), ".jpeg")), 
       plot = sfcm_si, height = 6, width = 10, dpi = 300)
# plotting the Xie Beni
sfcm_xb <- ggplot(SFCMvalues_k3) + 
  geom_raster(aes(x = alpha, y = window, fill = XieBeni.index)) + 
  geom_text(aes(x = alpha, y = window, label = round(XieBeni.index, 2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.5)
sfcm_xb
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_sfcm_xb_k3_", 
                         Sys.Date(), ".jpeg")), 
       plot = sfcm_xb, height = 6, width = 10, dpi = 300)

# Seed = 6891, Silhouette index = 0.28, k = 3, m = 1.2, window = 7x7, alpha = 0.1 (XB = 2.16)

#---------Spatial GFCM-----------------
# Start here when you get home. 
future::plan(future::multisession(workers = 2))

# k = 4, m = 1.7
SGFCMvalues_k4 <- select_parameters.mc(algo = "SGFCM", data = dataset,
                                       k = 4, m = 1.7,
                                       beta = seq(0.1,0.9,0.1), alpha = seq(0.5,2,0.1),
                                       window = w3,
                                       spconsist = TRUE, nrep = 5, 
                                       verbose = TRUE, chunk_size = 4,
                                       seed = 456, init = "kpp",
                                       indices = c("XieBeni.index", "Explained.inertia",
                                                   "Negentropy.index", "Silhouette.index"))


write_csv(SGFCMvalues_k4, here::here(paste0("outputs/nfbuffers_sgfcm_all_attri_param_indices_k4_", 
                                            Sys.Date(), ".csv")), append = FALSE)

# showing the silhouette index
sgfcm_si <- ggplot(SGFCMvalues_k4) + 
  geom_raster(aes(x = alpha, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(Silhouette.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
sgfcm_si
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_sgfcm_si_k4_", 
                         Sys.Date(), ".jpeg")), 
       plot = sgfcm_si, height = 6, width = 10, dpi = 300)
# showing the spatial inconsistency
ggplot(SGFCMvalues_k4) + 
  geom_raster(aes(x = alpha, y = beta, fill = spConsistency)) + 
  geom_text(aes(x = alpha, y = beta, label = round(spConsistency,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# showing the Xie Beni index
sgfcm_xb <- ggplot(SGFCMvalues_k4) + 
  geom_raster(aes(x = alpha, y = beta, fill = XieBeni.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(XieBeni.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
sgfcm_xb 
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_sgfcm_xb_k4_", 
                         Sys.Date(), ".jpeg")), 
       plot = sgfcm_xb, height = 6, width = 10, dpi = 300)

# Seed = 456, Silhouette index = 0.34, k = 4, m = 1.7, beta = 0.8, alpha = 1.6, w = 7x7 (w3)

#----------------------------------------------

# k = 3, m = 1.2

SGFCMvalues_k3 <- select_parameters.mc(algo = "SGFCM", data = dataset,
                                       k = 3, m = 1.2,
                                       beta = seq(0.1,0.9,0.1), alpha = seq(0.5,2,0.1),
                                       window = w3,
                                       spconsist = TRUE, nrep = 5, 
                                       verbose = TRUE, chunk_size = 4,
                                       seed = 456, init = "kpp",
                                       indices = c("XieBeni.index", "Explained.inertia",
                                                   "Negentropy.index", "Silhouette.index"))


write_csv(SGFCMvalues_k3, here::here(paste0("outputs/nfbuffers_sgfcm_all_attri_param_indices_k3_", 
                                            Sys.Date(), ".csv")), append = FALSE)

# showing the silhouette index
sgfcm_si <- ggplot(SGFCMvalues_k3) + 
  geom_raster(aes(x = alpha, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(Silhouette.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
sgfcm_si
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_sgfcm_si_k3_", 
                         Sys.Date(), ".jpeg")), 
       plot = sgfcm_si, height = 6, width = 10, dpi = 300)

# showing the spatial inconsistency
ggplot(SGFCMvalues_k3) + 
  geom_raster(aes(x = alpha, y = beta, fill = spConsistency)) + 
  geom_text(aes(x = alpha, y = beta, label = round(spConsistency,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# showing the Xie Beni index
sgfcm_xb <- ggplot(SGFCMvalues_k3) + 
  geom_raster(aes(x = alpha, y = beta, fill = XieBeni.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(XieBeni.index,2)), size = 2.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
sgfcm_xb 
ggsave(here::here(paste0("outputs/plots/nfbuffers_all_param_selection_sgfcm_xb_k3_", 
                         Sys.Date(), ".jpeg")), 
       plot = sgfcm_xb, height = 6, width = 10, dpi = 300)

# Seed = 456, Silhouette index = , k = 3, m = 1.2, beta = , alpha = , w = 7x7 (w3)




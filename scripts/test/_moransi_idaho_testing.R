library(sf)
library(raster)
library(terra)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(exactextractr)
library(tidyverse)
library(scales)
library(stringr)
library(cowplot)
library(spdep)

# load the attribute data
rst_sc <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_2024-10-08.tif")
rst <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")

# load the entropy rasters
ent_all <- rast(here::here("outputs/SGFCM_all_k6_entropy_2024-11-01.tif"))
ent_eco_k3 <- rast(here::here("outputs/SGFCM_eco_k3_entropy_2024-11-01.tif"))
ent_eco_k6 <- rast(here::here("outputs/SGFCM_eco_k6_entropy_2024-11-01.tif"))
ent_soc_k3 <- rast(here::here("outputs/SGFCM_soc_k3_entropy_2024-11-01.tif"))
ent_soc_k6 <- rast(here::here("outputs/SGFCM_soc_k6_entropy_2024-11-01.tif"))

# Load the USFS boundaries
fs_nf <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeForest.shp")
fs_reg <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeRegion.shp")

projection <- "epsg: 5070"

fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_nf.crop <- st_crop(fs_nf.proj, ext(rst_sc))
fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.crop <- st_crop(fs_reg.proj, ext(rst_sc))

# read in the 50km buffer shape

nf_buffers <- read_sf(here::here("data/processed/nf_buffers_50k_2024-10-22.shp"))

# read in the entropies for each forest

ent_df <- read_csv(here::here("outputs/nf_entropy_2024-11-01.csv"))
ent_sf <- read_sf(here::here("data/processed/ent_nf_2024-11-01.shp"))





# with forest polygons
# Scatter plot using nearest neighbors
# create a neighbor matrix
ent_pt <- st_centroid(ent_sf)
nb <- knn2nb(knearneigh(ent_pt, 10))
nbw <- nb2listw(nb, style = "W")
mp <- moran.plot(ent_sf$ent_all, nbw)

# Scatter plot using "queen" neighbors
moran.plot(ent_sf$ent_all, nbw)

## Local Moran's I
lmoran <- localmoran(ent_sf$ent_all, nbw, alternative = "greater", zero.policy = TRUE)

ent_sf$lmI <- lmoran[, "Ii"] # local Moran's I
ent_sf$lmZ <- lmoran[, "Z.Ii"] # z-scores
# p-values corresponding to alternative greater
ent_sf$lmp <- lmoran[, "Pr(z > E(Ii))"]

## Make H-H, H-L, L-H, and L-L clusters from moran plot (variable and lagged variable)
## and p-values from local Moran's I
## variable of interest and it's lagged values must be scaled!
mp <- moran.plot(as.vector(scale(ent_sf$ent_all)), nbw)

ent_sf$lmp <- lmoran[, 5] # p-values are in column 5

ent_sf$quadrant <- NA
# high-high
ent_sf[(mp$x >= 0 & mp$wx >= 0) & (ent_sf$lmp <= 0.05), "quadrant"]<- 1
# low-low
ent_sf[(mp$x <= 0 & mp$wx <= 0) & (ent_sf$lmp <= 0.05), "quadrant"]<- 2
# high-low
ent_sf[(mp$x >= 0 & mp$wx <= 0) & (ent_sf$lmp <= 0.05), "quadrant"]<- 3
# low-high
ent_sf[(mp$x <= 0 & mp$wx >= 0) & (ent_sf$lmp <= 0.05), "quadrant"]<- 4
# non-significant
ent_sf[(ent_sf$lmp > 0.05), "quadrant"] <- 5

library(tmap)
tmap_mode("plot")
tm_shape(ent_sf) + tm_fill(col = "quadrant", title = "",
                             breaks = c(1, 2, 3, 4, 5, 6),
                             palette =  c("red", "blue", "lightpink", "skyblue2", "white"),
                             labels = c("High-High", "Low-Low", "High-Low",
                                        "Low-High", "Non-significant")) +
  tm_legend(text.size = 1)  + tm_borders(alpha = 0.5) +
  tm_layout(frame = FALSE,  title = "Clusters")  +
  tm_layout(legend.outside = TRUE)

# save the high-low quadrant as shapefile
write_sf(ent_sf, here::here(paste0("data/processed/nf_ent_moran_quads_", Sys.Date(), ".shp")))


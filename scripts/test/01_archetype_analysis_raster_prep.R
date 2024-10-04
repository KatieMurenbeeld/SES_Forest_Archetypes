library(stringr)
library(sf)
library(terra)
library(tidyverse)
library(haven)
library(tigris)
library(readxl)
library(spdep)
library(gstat)
library(stars)

# Set the projection
projection <- "epsg:5070"

# Load the data
fed_rich <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/conus_fed_rich_2024-06-12.tif")
tree_cover <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/nlcd_tcc_conus_2016_v2021-4.tif")
tree_age <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/NA_TreeAge_1096/data/conus_age06_1km.tif")

# Load the reference raster and reproject
ref_rast <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/merged/conus_whp_3km_agg_interp_crop_2024-09-27.tif")
ref_rast_proj <- project(ref_rast, projection)

# For fed_rich fill NAs with 0, then aling and crop to reference raster
fed_rich[is.na(fed_rich)] <- 0
fed_rich_align <- resample(fed_rich, ref_rast_proj, "near")
fed_rich_crop <- crop(fed_rich_align, ref_rast_proj, mask = TRUE)
writeRaster(fed_rich_crop, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/conus_fed_rich_crop_", 
                                  Sys.Date(), ".tif"))

# For the tree cover
tree_cover_proj <- project(tree_cover, projection)
tree_cover_proj_subs <- subst(tree_cover_proj, 254:255, 0)
tree_cover_proj_resamp_ave <- resample(tree_cover_proj_subs, ref_rast_proj, "average", threads = TRUE)
tree_cover_proj_resamp_ave[is.na(tree_cover_proj_resamp_ave)] <- 0
tree_cover_proj_crop <- crop(tree_cover_proj_resamp_ave, ref_rast_proj, mask = TRUE)
plot(tree_cover_proj_crop)
writeRaster(tree_cover_proj_crop, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/conus_tree_cover_crop_", 
                                         Sys.Date(), ".tif"))

# For the stand age
tree_age_proj <- project(tree_age, projection)
#tree_age_agg_proj <- aggregate(tree_age_proj, fact = 3, fun = "mean", na.rm = TRUE)
tree_age_resamp <- resample(tree_age_proj, ref_rast_proj, "bilinear")
tree_age_resamp[is.na(tree_age_resamp)] <- 0
tree_age_resamp_crop <- crop(tree_age_resamp, ref_rast_proj, mask = TRUE)
plot(tree_age_resamp_crop)
writeRaster(tree_age_resamp_crop, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/conus_tree_age_crop_", 
                                    Sys.Date(), ".tif"))


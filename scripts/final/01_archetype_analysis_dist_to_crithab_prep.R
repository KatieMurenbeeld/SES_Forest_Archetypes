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

# Set projection 
projection = "epsg:5070"

# Load the reference raster and reproject
ref_rast <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/merged/conus_whp_3km_agg_interp_crop_2024-09-27.tif")
ref_rast_proj <- project(ref_rast, projection)

# Load the Critical Habitat shapefile
crithab <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/crithab_poly.shp")

## check for validity, remove empty geometries, and reproject 
if (!all(st_is_valid(crithab)))
  crithab <- st_make_valid(crithab)

crithab <- crithab %>%
  filter(!st_is_empty(.))

crithab_proj <- crithab %>%
  st_transform(projection)

## Create a template raster for the shapefiles
XMIN <- ext(ref_rast_proj)$xmin
XMAX <- ext(ref_rast_proj)$xmax
YMIN <- ext(ref_rast_proj)$ymin
YMAX <- ext(ref_rast_proj)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 3000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
templateRas <- rast(ncol=NCOLS, nrow=NROWS, 
                    xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                    vals=1, crs=crs(ref_rast_proj))

# Rasterize the wilderness areas shapefile
crithab_rast <- rasterize(vect(crithab_proj), templateRas)

# Calculate the distance from wilderness areas
crithab_dist_rast <- terra::distance(crithab_rast)

# Crop to the reference raster and update variable names
crithab_dist_crop <- crop(crithab_dist_rast, ref_rast_proj, mask = TRUE)
plot(crithab_dist_crop)
names(crithab_dist_crop) <- "distance_to_crithab_m"

# Save the raster
writeRaster(crithab_dist_crop, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/dist_to_crithab_3km_pred_crop_", 
                                 Sys.Date(), ".tif"), overwrite = TRUE)



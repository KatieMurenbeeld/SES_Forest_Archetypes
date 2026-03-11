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

# Load the Wilderness Areas shapefile
wild <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.Wilderness.shp")

## check for validity, remove empty geometries, and reproject 
if (!all(st_is_valid(wild)))
  wild <- st_make_valid(wild)

wild <- wild %>%
  filter(!st_is_empty(.))

wild_proj <- wild %>%
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
wild_rast <- rasterize(vect(wild_proj), templateRas)

# Calculate the distance from wilderness areas
wild_dist_rast <- terra::distance(wild_rast)

# Crop to the reference raster and update variable names
wild_dist_crop <- crop(wild_dist_rast, ref_rast_proj, mask = TRUE)
plot(wild_dist_crop)
names(wild_dist_crop) <- "distance_to_wilderness_m"

# Save the raster
writeRaster(wild_dist_crop, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/dist_to_wild_3km_pred_crop_", 
                                 Sys.Date(), ".tif"), overwrite = TRUE)



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
cejst <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/usa.shp")

# Load the reference raster and reproject
ref_rast <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/merged/conus_whp_3km_agg_interp_crop_2024-09-27.tif")
ref_rast_proj <- project(ref_rast, projection)

# Filter states for CONUS
filt_cejst <- cejst %>%
  filter(SF != c("Hawaii", "Alaska", "Puerto Rico",
                 "Northern Mariana Islands", "Guam", "American Samoa"))

## Select variables: less high school, housing burden, energy burden, and PM2.5
cejst_vars <- filt_cejst %>% 
  dplyr::select(geometry, HSEF, HBF_PFS, EBF_PFS, PM25F_PFS)


## check for validity, remove empty geometries, and reproject 
if (!all(st_is_valid(cejst_vars)))
  cejst_vars <- st_make_valid(cejst_vars)

cejst_vars <- cejst_vars %>%
  filter(!st_is_empty(.))

cejst_vars_proj <- cejst_vars %>%
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

grd <- st_as_stars(templateRas)

# function to rasterize variable, make points, make predictions
# raster_grid is like a reference raster
idw_preds <- function(data_proj, ref_raster, lay, empty_grid){
  var.rst <- rasterize(data_proj, ref_raster, field = lay, fun = "mean")
  var.pt <- as.points(var.rst) %>%
    st_as_sf(.)
  var.pred <- idw(var.pt[[1]]~1, var.pt, empty_grid)
  var.pred.rst <- rasterize(st_as_sf(var.pred), ref_raster, field = "var1.pred")
  names(var.pred.rst) <- paste0(lay, ".pred")
  return(c(orig.rst = var.rst, pred.rst = var.pred.rst))
}

# Less high school
lesshs.preds <- idw_preds(cejst_vars_proj, templateRas, "HSEF", grd)
plot(lesshs.preds$orig.rst)
lesshs_crop <- crop(lesshs.preds$pred.rst, ref_rast_proj, mask = TRUE)
plot(lesshs_crop)
writeRaster(lesshs_crop, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/cejst_lesshs_3km_pred_crop_", 
                                         Sys.Date(), ".tif"), overwrite = TRUE)

# Housing burden 
hsburd.preds <- idw_preds(cejst_vars_proj, templateRas, "HBF_PFS", grd)
plot(hsburd.preds$orig.rst)
houseburd_crop <- crop(hsburd.preds$pred.rst, ref_rast_proj, mask = TRUE)
plot(houseburd_crop)
writeRaster(houseburd_crop, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/cejst_houseburd_3km_pred_crop_", 
                                Sys.Date(), ".tif"), overwrite = TRUE)

# Energy burden 
engburd.preds <- idw_preds(cejst_vars_proj, templateRas, "EBF_PFS", grd)
plot(engburd.preds$orig.rst)
engburd_crop <- crop(engburd.preds$pred.rst, ref_rast_proj, mask = TRUE)
plot(engburd_crop)
writeRaster(engburd_crop, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/cejst_engburd_3km_pred_crop_", 
                                   Sys.Date(), ".tif"), overwrite = TRUE)

# PM2.5 exposure 
pm25.preds <- idw_preds(cejst_vars_proj, templateRas, "PM25F_PFS", grd)
plot(pm25.preds$orig.rst)
pm25_crop <- crop(pm25.preds$pred.rst, ref_rast_proj, mask = TRUE)
plot(pm25_crop)
writeRaster(pm25_crop, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/cejst_pm25_3km_pred_crop_", 
                                 Sys.Date(), ".tif"), overwrite = TRUE)


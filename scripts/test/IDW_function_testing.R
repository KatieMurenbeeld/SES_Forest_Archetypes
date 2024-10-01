library(tidyverse)
library(sf)
library(terra)
library(raster)
library(tmap)
library(ggplot2)
library(viridis)
library(spdep)
library(gstat)
library(stars)

# Load the data and set projection
cejst <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/usa.shp")
projection <- "epsg:5070"

# filter 
#filt_cejst <- cejst %>%
#  filter(SF == "Florida")

filt_cejst <- cejst %>%
  filter(SF != c("Hawaii", "Alaska", "Puerto Rico",
                 "Northern Mariana Islands", "Guam", "American Samoa"))
  
## check for validity, remove empty geometries, and reproject 
if (!all(st_is_valid(filt_cejst)))
  filt_cejst <- st_make_valid(filt_cejst)

filt_cejst <- filt_cejst %>%
  filter(!st_is_empty(.))

filt_proj <- filt_cejst %>%
  st_transform(projection)

## create 3km grid and get center points (may not be needed if using ref raster)
grd.3km <- st_bbox(filt_proj) %>%
  st_as_stars(dx = 3000) %>%
  st_crop(filt_proj) 

grd.3km.pt <- grd.3km %>% # grid of points for predictions
  st_as_sf(.) %>%
  st_centroid(.)

# rasterize grid and the cejst
grd.3km.rst <- rast(grd.3km)

# function to rasterize variable, make points, make predictions
# raster_grid is like a reference raster
idw_preds <- function(data_proj, raster_grid, lay, empty_grid){
  var.rst <- rasterize(data_proj, raster_grid, field = lay, fun = "mean")
  var.pt <- as.points(var.rst) %>%
    st_as_sf(.)
  var.pred <- idw(var.pt[[1]]~1, var.pt, empty_grid)
  var.pred.rst <- rasterize(st_as_sf(var.pred), raster_grid, field = "var1.pred")
  names(var.pred.rst) <- paste0(lay, ".pred")
  #return(var.pred.rst)
  return(c(orig.rst = var.rst, pred.rst = var.pred.rst))
}

hsef.preds <- idw_preds(filt_proj, grd.3km.rst, "HSEF", grd.3km)
plot(hsef.preds$orig.rst)
plot(hsef.preds$pred.rst)

hsef3km_cor_3 <- focalPairs(c(hsef.preds$orig.rst, hsef.preds$pred.rst), w = 3, "pearson", na.rm = TRUE) 
plot(hsef3km_cor_3)


# test for all of conus
ref_rast <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/merged/conus_whp_3km_agg_2024-08-09.tif")
ref_rast_proj <- project(ref_rast, projection)

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


# will this match the reference raster?
# no, need to use the reference raster, make templateRas from ref_rast and then st_as_stars()
# need to fill in the missing data in the ref_rast too!!!
#---Load reference raster----
ref_rast <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/merged/conus_whp_3km_agg_2024-08-09.tif")
ref_rast_proj <- project(ref_rast, projection)
plot(ref_rast_proj)

#idw_preds(filt_proj, grd.3km.rst, "HSEF", grd.3km)
conus.preds <- idw_preds(filt_proj, templateRas, "HSEF", grd)

plot(conus.preds$pred.rst)
writeRaster(conus.preds$pred.rst, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/hsef_3km_pred_", 
                                         Sys.Date(), ".tif"))
hsef_pred_crop <- crop(conus.preds$pred.rst, ref_rast_proj, mask = TRUE)
plot(hsef_pred_crop)
test_stk <- c(ref_rast_proj, conus.preds$pred.rst) # works when not cropped
test_stk <- c(ref_rast_proj, hsef_pred_crop) # works when cropped


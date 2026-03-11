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
forest_depend <- read_csv("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/Data/County-Forest_Dep_Comm_Capital.csv")

# Load the reference raster and reproject
ref_rast <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/merged/conus_whp_3km_agg_interp_crop_2024-09-27.tif")
ref_rast_proj <- project(ref_rast, projection)

## Load county boundaries from tigris
### Get Continental US list
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]
us.fips <- unique(fips_codes$state_code)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name, us.fips))
colnames(us.states) <- c("state", "STATENAME", "FIPS")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI",] #only CONUS

### Set the year to account for changes to FIPS codes
counties_2020 <- tigris::counties(year = 2020) 
counties_2020 <- counties_2020 %>%
  filter(STATEFP %in% continental.states$FIPS) %>%
  dplyr::select(GEOID, geometry)

## Forest Dependence Variables: select the % forest pay
fordep <- forest_depend %>% # needs 2020 counties
  dplyr::select(fips, pct.pay) %>%
  rename("FIPS" = "fips", "pct_forpay" = "pct.pay")

## Make sure all FIPS codes are padded with 0s for a total of 5 characters
update_fips <- function(data_set) {
  data_set$FIPS <- as.character(data_set$FIPS)
  data_set$FIPS <- str_pad(data_set$FIPS, 5, side="left", pad="0")
  return(data_set)
}

fordep_fips <- update_fips(fordep)

## join to the 2020 counties
fordep_county <- left_join(counties_2020, fordep_fips,
                      by = c("GEOID" = "FIPS"))

## check for validity, remove empty geometries, and reproject 
if (!all(st_is_valid(fordep_county)))
  fordep_county <- st_make_valid(fordep_county)

fordep_county <- fordep_county %>%
  filter(!st_is_empty(.))

fordep_proj <- fordep_county %>%
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
  var.pred.rst.crop <- crop(var.pred.rst, ref_raster, mask = TRUE)
  #return(var.pred.rst)
  return(c(orig.rst = var.rst, pred.rst = var.pred.rst.crop))
}

fordep.preds <- idw_preds(fordep_proj, templateRas, "pct_forpay", grd)
plot(ref_rast_proj)
plot(fordep.preds$orig.rst)
plot(fordep.preds$pred.rst)
# didn't crop properly in function
forpred_crop <- crop(fordep.preds$pred.rst, ref_rast_proj, mask = TRUE)
plot(forpred_crop)
writeRaster(forpred_crop, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/pct_forpay_3km_pred_crop_", 
                                         Sys.Date(), ".tif"), overwrite = TRUE)


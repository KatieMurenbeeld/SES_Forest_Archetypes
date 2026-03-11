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
del_pop_cen <- read_csv("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/census_2023_comp_est.csv")

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
counties_2023 <- tigris::counties(year = 2023) 
counties_2023 <- counties_2023 %>%
  filter(STATEFP %in% continental.states$FIPS) %>%
  dplyr::select(GEOID, geometry)

## Net and ave change in population and % change in migration (rate?), using census data
delpop_cen <- del_pop_cen %>% # needs 2023 counties
  mutate(pct_mig = ((NPOPCHG2023 - NPOPCHG2020)/NPOPCHG2020) * 100,
         ave_del_pop = ((NPOPCHG2020 + NPOPCHG2021 + NPOPCHG2022 + NPOPCHG2023)/4)) %>%
  dplyr::select(STATE, COUNTY, NPOPCHG2023, NETMIG2022, ave_del_pop, pct_mig)

delpop_cen <- delpop_cen %>%
  mutate(STATE = as.character(STATE), 
         COUNTY = as.character(COUNTY)) %>%
  mutate(FIPS = paste(STATE, COUNTY, sep = ""))

## Make sure all FIPS codes are padded with 0s for a total of 5 characters
update_fips <- function(data_set) {
  data_set$FIPS <- as.character(data_set$FIPS)
  data_set$FIPS <- str_pad(data_set$FIPS, 5, side="left", pad="0")
  return(data_set)
}

delpop_fips <- update_fips(delpop_cen)

## join to the 2020 counties
delpop_county <- left_join(counties_2023, delpop_fips,
                      by = c("GEOID" = "FIPS"))

## check for validity, remove empty geometries, and reproject 
if (!all(st_is_valid(delpop_county)))
  delpop_county <- st_make_valid(delpop_county)

delpop_county <- delpop_county %>%
  filter(!st_is_empty(.))

delpop_proj <- delpop_county %>%
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

delpop.preds <- idw_preds(delpop_proj, templateRas, "NETMIG2022", grd)
plot(delpop.preds$orig.rst)
# Crop to reference raster
delpop_crop <- crop(delpop.preds$pred.rst, ref_rast_proj, mask = TRUE)
plot(delpop_crop)

# Save raster 
writeRaster(delpop_crop, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/net_mig_2023_3km_pred_crop_", 
                                         Sys.Date(), ".tif"))

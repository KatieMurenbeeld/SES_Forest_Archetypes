## Download and process raster data of temperature and precipitation
## Download and process travel time data
## Download elevation data and calculate a topographic complexity index

# 0. Load libraries and set projection
library(sf) # for working with vector data
library(raster) # for working with rasters
library(sp) # for working with spatial (vector) data
library(geodata) # this package removed from CRAN May 27, 2024...
library(terra) # for working with rasters
library(tigris) # needed for state/CONUS boundaries
library(dplyr) # for manipulating dataframes 
library(dismo) # needed to calculate biovars

projection <- "epsg:5070"

# Steps:
# 0. Load libraries and set projection
# 1. Download the rasters (vars = "bio")
# 3. Aggregate or disaggregate to 1.5km and 3km resolution
# 2. Crop to contiguous US
# 4. Set to analysis projection

# 1. Download the data using the geodata package

#r_prec <- geodata::worldclim_country(country = "USA", var = "prec", res = 0.5, path = here::here("data/original/"))
#r_tmin <- geodata::worldclim_country(country = "USA", var = "tmin", res = 0.5, path = here::here("data/original"))
#r_tmax <- geodata::worldclim_country(country = "USA", var = "tmax", res = 0.5, path = here::here("data/original"))
#r_ele <- geodata::elevation_30s(country = "US", path = here::here("data/original/"))
#r_tt <- geodata::travel_time(to = "city", size = 7, up = TRUE, path = here::here("data/original/"))

r_prec <- rast(here::here("data/original/climate/wc2.1_country/USA_wc2.1_30s_prec.tif"))
r_tmin <- rast(here::here("data/original/climate/wc2.1_country/USA_wc2.1_30s_tmin.tif"))
r_tmax <- rast(here::here("data/original/climate/wc2.1_country/USA_wc2.1_30s_tmax.tif"))
r_ele <- rast(here::here("data/original/elevation/USA_elv_msk.tif"))
r_tt <- rast(here::here("data/original/travel/travel_time_to_cities_u7.tif"))

# Load the reference raster and reproject
ref_rast <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/merged/conus_whp_3km_agg_interp_crop_2024-09-27.tif")
ref_rast_proj <- project(ref_rast, projection)

# 2. Crop to CONUS, because the files are global, it is more efficient to crop to the extant of conus states first
## Using tigris, download the state boundaries
states <- tigris::states(cb = TRUE)

## Filter out Alaska, Hawaii, DC, and territories
states <- states %>%
  filter(STUSPS != "AK" & STUSPS != "HI" & STUSPS != "DC") %>%
  filter(GEOID < 60)

# Set projection for states
states_proj <- st_transform(states, projection)

# create a function to crop to conus, reproject, aling and crop to ref raster
crop_project <- function(raster, states, ref_raster){
  r_crop <- crop(raster, ext(states)) # First, crop to the extents of states
  r_proj <- project(r_crop, projection) # Then, reproject the raster to crs of states
  r_align <- resample(r_proj, ref_raster) # align to the reference raster 
  r_align[is.na(r_align)] <- 0 # fill in NAs with 0, only way to get correct number of pixels with values
  r_conus <- crop(r_align, ref_raster, mask = TRUE) # Finally, crop again with mask = TRUE
  return(r_conus)
}

r_tt_crop <- crop(r_tt, ext(states))
r_tt_crop_proj <- project(r_tt_crop, projection)
r_tt_align <- resample(r_tt_crop_proj, ref_rast_proj)
r_tt_align[is.na(r_tt_align)] <- 0
r_tt_align_crop <- crop(r_tt_align, ref_rast_proj, mask = TRUE)
nrow(as.data.frame(r_tt_align))
nrow(as.data.frame(r_tt_align_crop))
plot(r_tt_align_crop)
plot(r_tt_align)
plot(r_tt_crop_proj)

r_tt_conus <- crop_project(r_tt, states, ref_rast_proj)
plot(r_tt_conus)
nrow(as.data.frame(r_tt_conus))
#plot(ref_rast_proj)
r_prec_conus <- crop_project(r_prec, states, ref_rast_proj)
plot(r_prec_conus$USA_wc2.1_30s_prec_1)
r_tmin_conus <- crop_project(r_tmin, states, ref_rast_proj)
plot(r_tmin_conus$USA_wc2.1_30s_tmin_1)
r_tmax_conus <- crop_project(r_tmax, states, ref_rast_proj)
plot(r_tmax_conus$USA_wc2.1_30s_tmax_1)
roughness <- terrain(r_ele, v = "roughness") # calculate roughness first
r_elev_conus <- crop_project(roughness, states, ref_rast_proj)
plot(r_elev_conus)
nrow(as.data.frame(r_elev_conus))


# calculate elevation roughness and biovars

bio_conus <- biovars(brick(r_prec_conus), brick(r_tmin_conus), brick(r_tmax_conus))

r_temp_seas_conus <- rast(bio_conus$bio4)
r_prec_seas_conus <- rast(bio_conus$bio15)
nrow(as.data.frame(r_temp_seas_conus))
nrow(as.data.frame(r_temp_seas_conus))
plot(r_temp_seas_conus)
plot(r_prec_seas_conus)

# save the rasters (during the resample they were aggregated to 3km)
writeRaster(r_tt_conus, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/trav_time_3000m_", 
                               Sys.Date(), ".tif"), overwrite = TRUE)
writeRaster(r_elev_conus, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/roughness_3000m_", 
                               Sys.Date(), ".tif"), overwrite = TRUE)
writeRaster(r_prec_seas_conus, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/prec_seas_3000m_", 
                               Sys.Date(), ".tif"), overwrite = TRUE)
writeRaster(r_temp_seas_conus, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/temp_seas_3000m_", 
                               Sys.Date(), ".tif"), overwrite = TRUE)








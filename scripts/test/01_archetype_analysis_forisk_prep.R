library(tidyverse)
library(terra)
library(sf)
library(readr)
library(ggplot2)
library(ggmap)
library(tigris)
library(tmap)
library(patchwork)
library(units)
library(spdep)
library(spatstat)
library(gstat)

#----Distance to mills and Mill Capacity "hotspots"
# Pseudocode:
# 1. Load the data (FORISK and county boundaries)
# 2. Align the data
# 3. Rasterize data
# 4. Aggregate to 1.5km and 3km
# 5. Calculate distance from mill points
# 6. Mill capacity metric

# 1. Load the data
mill_sf <- read_sf("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/2024_Q1_Forisk_North_American_Ind_Cap_DB_Shape/Forisk_NA_FI_Capacity_DB_2024_Q1.shp")

# Set the projection
projection <- "epsg:5070"
# load and reproject the raster data
ref_rast <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/merged/conus_whp_3km_agg_interp_crop_2024-09-27.tif")
ref_rast_proj <- project(ref_rast, projection)

##Get Continental US list
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name))
colnames(us.states) <- c("state","STATENAME")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI" & us.states$state != "DC",] #only CONUS

counties <- tigris::counties(state = continental.states$state, cb = TRUE, year = 2022)

conus_mills <- mill_sf %>%
  filter(Region != "Canada West") %>%
  filter(Region != "Canada East") %>%
  filter(State_Prov %in% continental.states$state) #%>% # need to keep all mills in order to calculate change in mill capacity for mills that have closed
#filter(Status == "Open")

# What is the distribution of total wood capacity?

hist(conus_mills$Total_Wood, main = "Distribution of Total Wood Capacity", xlab = "Total Wood Capacity", ylab = "Frequency")

# 2. Align the data
st_crs(conus_mills)
st_crs(counties)

identical(st_crs(conus_mills), st_crs(counties))

# Create a mill count variable for each county
counties$mill_count <- lengths(st_intersects(counties, conus_mills))

# Calculate the change in mill capacity from 2019-2023 (5 years)
## For mills that came "online" set the CPT_2019 to 1.0

conus_mills$CPT_2019[conus_mills$CPT_2019 == 0.0] <- 1.0 

conus_mills <- conus_mills %>%
  mutate(millcap_5yr = (CPT_2023 - CPT_2019) / CPT_2019)

### Make categorical. Could also make this only 3 categories (decrease, no change, increase in order to protect identities)
conus_mills <- conus_mills %>%
  mutate(change_millcap = case_when(millcap_5yr == -1.0 ~ "mill closed", 
                                    millcap_5yr > -1.0 & millcap_5yr < 0.0 ~ "decrease",
                                    millcap_5yr == 0.0 ~ "no change",
                                    millcap_5yr > 0.0 & CPT_2019 != 1.0 ~ "increase",
                                    millcap_5yr > 0.0 & CPT_2019 == 1.0 ~"new mill"))

# Check for missing data
print ("Row and Col positions of NA values") 
which(is.na(conus_mills$millcap_5yr), arr.ind = TRUE)

## Interpolate del mill capacity
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

#nodes <- st_make_grid(counties,n = c(50,50), what = "centers")
#nodes2 <- st_make_grid(conus_mills,n = c(50,50),what = "centers")

nodes3 <- st_make_grid(grd,
                       n = c(100, 100),
                       what = "centers")

# remove mills that have the same lat, lon
conus_mills_nodeupe <- conus_mills[!duplicated(conus_mills[,22:23]),]
conus_mills_nodeupe_proj <- st_transform(conus_mills_nodeupe, projection)

dist <- distance(vect(nodes3), vect(conus_mills_nodeupe_proj))
nearest_conus <- apply(dist, 1, function(x) which(x == min(x)))
millcap5.nn <- conus_mills_nodeupe_proj$millcap_5yr[nearest_conus]
currcap.nn <- conus_mills_nodeupe_proj$Current_Ca[nearest_conus]
totwood.nn <- conus_mills_nodeupe_proj$Total_Wood[nearest_conus]
preds <- st_as_sf(nodes3)
preds$millcap5 <- millcap5.nn
preds$currcap <- currcap.nn
preds$totwood <- totwood.nn
preds <- as(preds, "Spatial")
sp::gridded(preds) <- TRUE
preds.rast <- rast(preds)
plot(preds.rast$millcap5)

mc5sf05 <- gstat(id = "millcap_5yr", formula = millcap_5yr~1, data=conus_mills_nodeupe_proj,  nmax=7, set=list(idp = 0.5))
mc5sf1 <- gstat(id = "millcap_5yr", formula = millcap_5yr~1, data=conus_mills_nodeupe_proj,  nmax=7, set=list(idp = 1))
mc5sf2 <- gstat(id = "millcap_5yr", formula = millcap_5yr~1, data=conus_mills_nodeupe_proj,  nmax=7, set=list(idp = 2))

interpolate_gstat <- function(model, x, crs, ...) {
  v <- st_as_sf(x, coords=c("x", "y"), crs=crs)
  p <- predict(model, v, ...)
  as.data.frame(p)[,1:2]
}

zmc5sf05 <- interpolate(preds.rast, mc5sf05, debug.level=0, fun=interpolate_gstat, crs=crs(projection), index=1)
zmc5sf1 <- interpolate(preds.rast, mc5sf1, debug.level=0, fun=interpolate_gstat, crs=crs(projection), index=1)
zmc5sf2 <- interpolate(preds.rast, mc5sf2, debug.level=0, fun=interpolate_gstat, crs=crs(projection), index=1)

# 3. Rasterize the data using the raster created in the 00_archetype-analysis_download-prep-fire.R
# Resample and crop the mill capacity change predictions to the reference raster.
# created in the 00_archetype-analysis_download-prep-fire.R


#zmc5sf05_proj <- project(zmc5sf05, projection)
zmc5sf05_resamp <- resample(zmc5sf05, ref_rast_proj, "bilinear")
plot(zmc5sf05_resamp)
zmc5sf05_crop <- crop(zmc5sf05_resamp, ref_rast_proj, mask = TRUE) 
plot(zmc5sf05_crop)
nrow(as.data.frame(zmc5sf05_crop))


#zmc5sf1_proj <- project(zmc5sf1, projection)
zmc5sf1_resamp <- resample(zmc5sf1, ref_rast_proj, "bilinear")
#zmc5sf1_resamp[is.na(zmc5sf1_resamp)] <- 0 
zmc5sf1_crop <- crop(zmc5sf1_resamp, ref_rast_proj, mask = TRUE)
plot(zmc5sf1_crop)
nrow(as.data.frame(zmc5sf1_crop))

#zmc5sf2_proj <- project(zmc5sf2, projection)
zmc5sf2_resamp <- resample(zmc5sf2, ref_rast_proj, "bilinear")
#zmc5sf2_resamp[is.na(zmc5sf2_resamp)] <- 0 
zmc5sf2_crop <- crop(zmc5sf2_resamp, ref_rast_proj, mask = TRUE)
plot(zmc5sf2_crop)
nrow(as.data.frame(zmc5sf2_crop))


mill_proj <- conus_mills %>% st_transform(., crs = crs(ref_rast_proj))
millchange_rast <- rasterize(vect(mill_proj), ref_rast_proj, field = "change_millcap")

# 3.5 Write rasters

writeRaster(millchange_rast, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/millchange_cap_",
                                    Sys.Date(), ".tif"), overwrite = TRUE)
writeRaster(zmc5sf05_crop, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/millchange_interp-05_",
                                  Sys.Date(), ".tif"), overwrite = TRUE)
writeRaster(zmc5sf1_crop, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/millchange_interp-1_",
                                 Sys.Date(), ".tif"), overwrite = TRUE)
writeRaster(zmc5sf2_crop, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/millchange_interp-2_",
                                 Sys.Date(), ".tif"), overwrite = TRUE)


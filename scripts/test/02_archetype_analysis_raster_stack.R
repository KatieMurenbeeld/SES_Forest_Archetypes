library(tidyverse)
library(terra)


# Load the rasters
aip_crop <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/aip_3km_pred_crop_2024-09-30.tif")
bric_crop <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/bric_commcap_3km_pred_crop_2024-09-30.tif")
netmig_crop <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/net_mig_2023_3km_pred_crop_2024-09-30.tif")
forpay_crop <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/pct_forpay_3km_pred_crop_2024-09-30.tif")
forprod_crop <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/fia_for_prod_3km_pred_crop_2024-09-30.tif")
lesshs_crop <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/cejst_lesshs_3km_pred_crop_2024-09-30.tif")
hsburd_crop <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/cejst_houseburd_3km_pred_crop_2024-09-30.tif")
engburd_crop <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/cejst_engburd_3km_pred_crop_2024-09-30.tif")
pm25_crop <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/cejst_pm25_3km_pred_crop_2024-09-30.tif")
travtime_crop <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/trav_time_3000m_2024-10-03.tif")
rough_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/roughness_3000m_2024-10-03.tif")
precseas_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/prec_seas_3000m_2024-10-03.tif")
tempseas_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/temp_seas_3000m_2024-10-03.tif")
distwild_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/dist_to_wild_3km_pred_crop_2024-10-01.tif")
distcrit_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/dist_to_crithab_3km_pred_crop_2024-10-01.tif")
millchng_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/millchange_interp-2_2024-10-03.tif")
fedrich_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/conus_fed_rich_crop_2024-10-01.tif")
treecov_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/conus_tree_cover_crop_2024-10-03.tif")
treeage_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/conus_tree_age_crop_2024-10-01.tif")
forgain_crop <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/forest_gain_3km_crop_2024-10-01.tif")
whp_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/merged/conus_whp_3km_agg_interp_crop_2024-09-27.tif")

#----check for NAs or unequal lengths of data----

nrow(as.data.frame(aip_crop)) #874789
nrow(as.data.frame(bric_crop)) #874789
nrow(as.data.frame(forpay_crop)) #874789
nrow(as.data.frame(forgain_crop)) #874789
nrow(as.data.frame(forprod_crop)) #874789
nrow(as.data.frame(netmig_crop)) #874789
nrow(as.data.frame(lesshs_crop)) #874789
nrow(as.data.frame(treeage_crop)) #874789
nrow(as.data.frame(treecov_crop)) #874789
nrow(as.data.frame(hsburd_crop)) #874789
nrow(as.data.frame(engburd_crop)) #874789
nrow(as.data.frame(millchng_crop)) #!!!874387!!!
nrow(as.data.frame(whp_crop)) #874789
nrow(as.data.frame(rough_crop)) #!!!866508!!!
nrow(as.data.frame(travtime_crop)) #!!!868546!!!
nrow(as.data.frame(precseas_crop)) #!!!868550!!!
nrow(as.data.frame(tempseas_crop)) #!!!868550!!!
nrow(as.data.frame(fedrich_crop)) #874789
nrow(as.data.frame(distwild_crop)) #874789
nrow(as.data.frame(distcrit_crop)) #874789
nrow(as.data.frame(pm25_crop)) #874789

# Check alignment and stack the rasters
rast_stack <- c(aip_crop, bric_crop, forpay_crop, forprod_crop, 
                lesshs_crop, hsburd_crop, engburd_crop, pm25_crop,
                travtime_crop, rough_crop, precseas_crop, tempseas_crop,
                distwild_crop, distcrit_crop, millchng_crop, fedrich_crop,
                treecov_crop, treeage_crop, forgain_crop, whp_crop, 
                netmig_crop)



## Save the raster
writeRaster(x = rast_stack, filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_", 
                                              Sys.Date(), ".tif"), overwrite = TRUE)
## Scale the data from 0-1 and save the raster
rast_stack_sc <- (rast_stack - global(rast_stack, "min", na.rm=TRUE)[,1])/(global(rast_stack, "max", na.rm=TRUE)[,1] - global(rast_stack, "min", na.rm=TRUE)[,1])
writeRaster(x = rast_stack_sc, filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_scaled_", 
                                              Sys.Date(), ".tif"), overwrite = TRUE)


rast_stack[is.na(rast_stack)] <- 0
plot(rast_stack$mrp_ideology.pred)

### Load the states from tigris
states <- tigris::states(cb = TRUE)
### Get Continental US list
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]
us.fips <- unique(fips_codes$state_code)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name, us.fips))
colnames(us.states) <- c("state", "STATENAME", "FIPS")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI",] #only CONUS

### Filter tigris states for conus states and set crs to crs of raster
conus_states <- states %>%
  filter(STUSPS %in% continental.states$state) %>%
  dplyr::select(STUSPS, GEOID, geometry) %>%
  st_transform(., crs = crs(rast_stack))

rast_stack_crop <- crop(rast_stack, conus_states, mask = TRUE)
plot(rast_stack_crop$mrp_ideology.pred, colNA="pink")
writeRaster(x = rast_stack_crop, filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_crop_all_attributes_", 
                                              Sys.Date(), ".tif"), overwrite = TRUE)

rast_stack_sc[is.na(rast_stack_sc)] <- 0
rast_stack_sc_crop <- crop(rast_stack_sc, conus_states, mask = TRUE)
plot(rast_stack_sc_crop$mrp_ideology.pred, colNA="pink")
writeRaster(x = rast_stack_sc_crop, filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_crop_all_attributes_scaled_", 
                                                 Sys.Date(), ".tif"), overwrite = TRUE)
## Stack the ecological variables, scale, and save the rasters
eco_rast_stack <- c(forprod_crop, rough_crop, 
                    precseas_crop, tempseas_crop,
                    treecov_crop, treeage_crop,
                    forgain_crop, whp_crop)

## Save the raster
writeRaster(x = eco_rast_stack, filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_eco_attributes_", 
                                              Sys.Date(), ".tif"), overwrite = TRUE)

## Scale the data from 0-1 and save the raster
eco_rast_stack_sc <- (eco_rast_stack - global(eco_rast_stack, "min", na.rm=TRUE)[,1])/(global(eco_rast_stack, "max", na.rm=TRUE)[,1] - global(eco_rast_stack, "min", na.rm=TRUE)[,1])
writeRaster(x = eco_rast_stack_sc, filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_eco_attributes_scaled_", 
                                                 Sys.Date(), ".tif"), overwrite = TRUE)

eco_rast_stack[is.na(eco_rast_stack)] <- 0
eco_rast_stack_crop <- crop(eco_rast_stack, conus_states, mask = TRUE)
writeRaster(x = eco_rast_stack_crop, filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_eco_crop_attributes_", 
                                                  Sys.Date(), ".tif"), overwrite = TRUE)

eco_rast_stack_sc[is.na(eco_rast_stack_sc)] <- 0
eco_rast_stack_sc_crop <- crop(eco_rast_stack_sc, conus_states, mask = TRUE)
writeRaster(x = eco_rast_stack_sc_crop, filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_eco_sc_crop_attributes_", 
                                                       Sys.Date(), ".tif"), overwrite = TRUE)


## Stack the social variables, scale, and save the rasters
soc_rast_stack <- c(aip_crop, bric_crop,
                    forpay_crop, lesshs_crop, 
                    hsburd_crop, engburd_crop,
                    pm25_crop, travtime_crop,
                    distwild_crop, distcrit_crop, 
                    millchng_crop, fedrich_crop,
                    netmig_crop)

## Save the raster
writeRaster(x = soc_rast_stack, filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_soc_attributes_", 
                                                  Sys.Date(), ".tif"), overwrite = TRUE)

## Scale the data from 0-1 and save the raster
soc_rast_stack_sc <- (soc_rast_stack - global(soc_rast_stack, "min", na.rm=TRUE)[,1])/(global(soc_rast_stack, "max", na.rm=TRUE)[,1] - global(soc_rast_stack, "min", na.rm=TRUE)[,1])
writeRaster(x = soc_rast_stack_sc, filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_soc_attributes_scaled_", 
                                                     Sys.Date(), ".tif"), overwrite = TRUE)

soc_rast_stack_crop <- crop(soc_rast_stack, conus_states, mask = TRUE)
writeRaster(x = soc_rast_stack_crop, filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_soc_crop_attributes_", 
                                                       Sys.Date(), ".tif"), overwrite = TRUE)


soc_rast_stack_sc_crop <- crop(soc_rast_stack_sc, conus_states, mask = TRUE)
writeRaster(x = soc_rast_stack_sc_crop, filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_soc_sc_crop_attributes_", 
                                                          Sys.Date(), ".tif"), overwrite = TRUE)



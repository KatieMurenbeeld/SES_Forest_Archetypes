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
rough_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/roughness_3000m_2024-10-07.tif")
precseas_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/prec_seas_3000m_2024-10-07.tif")
tempseas_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/temp_seas_3000m_2024-10-07.tif")
distwild_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/dist_to_wild_3km_pred_crop_2024-10-01.tif")
distcrit_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/dist_to_crithab_3km_pred_crop_2024-10-01.tif")
fedrich_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/conus_fed_rich_crop_2024-10-01.tif")
treecov_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/conus_tree_cover_crop_2024-10-08.tif")
treeage_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/conus_tree_age_crop_2024-10-08.tif")
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
nrow(as.data.frame(whp_crop)) #874789
nrow(as.data.frame(rough_crop)) #874789
nrow(as.data.frame(travtime_crop)) #874789
nrow(as.data.frame(precseas_crop)) #874789
nrow(as.data.frame(tempseas_crop)) #874789
nrow(as.data.frame(fedrich_crop)) #874789
nrow(as.data.frame(distwild_crop)) #874789
nrow(as.data.frame(distcrit_crop)) #874789
nrow(as.data.frame(pm25_crop)) #874789

# Check alignment and stack the rasters
rast_stack <- c(aip_crop, bric_crop, forpay_crop, forprod_crop, 
                lesshs_crop, hsburd_crop, engburd_crop, pm25_crop,
                travtime_crop, rough_crop, precseas_crop, tempseas_crop,
                distwild_crop, distcrit_crop, fedrich_crop,
                treecov_crop, treeage_crop, forgain_crop, whp_crop, 
                netmig_crop)

names(rast_stack) <- c("aip", "comm_cap", "pct_forpay", "forprod", 
                       "lesshs", "hsbrd", "engbrd", "pm25", "travtime",
                       "rough", "precseas", "tempseas", "distwild", "distcrit", 
                       "fedrich", "treecov", "treeage", "forgain",
                       "whp", "netmig")

## Save the raster
writeRaster(x = rast_stack, filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_no_forisk_", 
                                              Sys.Date(), ".tif"), overwrite = TRUE)
## Scale the data from 0-1 and save the raster
rast_stack_sc <- (rast_stack - global(rast_stack, "min", na.rm=TRUE)[,1])/(global(rast_stack, "max", na.rm=TRUE)[,1] - global(rast_stack, "min", na.rm=TRUE)[,1])
writeRaster(x = rast_stack_sc, filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_no_forisk_scaled_", 
                                              Sys.Date(), ".tif"), overwrite = TRUE)


## Stack the ecological variables, scale, and save the rasters
eco_rast_stack <- c(forprod_crop, rough_crop, 
                    precseas_crop, tempseas_crop,
                    treecov_crop, treeage_crop,
                    forgain_crop, whp_crop)

names(eco_rast_stack) <- c("forproc", "rough", "precseas", "tempseas",
                           "treecov", "treeage", "forgain", "whp")

## Save the raster
writeRaster(x = eco_rast_stack, filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_eco_attributes_", 
                                              Sys.Date(), ".tif"), overwrite = TRUE)

## Scale the data from 0-1 and save the raster
eco_rast_stack_sc <- (eco_rast_stack - global(eco_rast_stack, "min", na.rm=TRUE)[,1])/(global(eco_rast_stack, "max", na.rm=TRUE)[,1] - global(eco_rast_stack, "min", na.rm=TRUE)[,1])
writeRaster(x = eco_rast_stack_sc, filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_eco_attributes_scaled_", 
                                                 Sys.Date(), ".tif"), overwrite = TRUE)

## Stack the social variables, scale, and save the rasters
soc_rast_stack <- c(aip_crop, bric_crop,
                    forpay_crop, lesshs_crop, 
                    hsburd_crop, engburd_crop,
                    pm25_crop, travtime_crop,
                    distwild_crop, distcrit_crop, 
                    fedrich_crop, netmig_crop)

names(soc_rast_stack) <- c("aip", "comm_cap", 
                           "pct_forpay",  "lesshs", 
                           "hsbrd", "engbrd", 
                           "pm25",  "travtime", 
                           "distwild", "distcrit", 
                           "fedrich", "netmig")

## Save the raster
writeRaster(x = soc_rast_stack, filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_soc_attributes_no_forisk_", 
                                                  Sys.Date(), ".tif"), overwrite = TRUE)

## Scale the data from 0-1 and save the raster
soc_rast_stack_sc <- (soc_rast_stack - global(soc_rast_stack, "min", na.rm=TRUE)[,1])/(global(soc_rast_stack, "max", na.rm=TRUE)[,1] - global(soc_rast_stack, "min", na.rm=TRUE)[,1])
writeRaster(x = soc_rast_stack_sc, filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_soc_attributes_no_forisk_scaled_", 
                                                     Sys.Date(), ".tif"), overwrite = TRUE)



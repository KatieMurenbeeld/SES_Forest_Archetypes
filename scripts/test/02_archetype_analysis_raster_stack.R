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
travtime_crop <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/trav_time_3km_2024-10-01.tif")
rough_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/roughness_3km_2024-10-01.tif")
precseas_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/prec_seas_3km_2024-10-01.tif")
tempseas_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/temp_seas_3km_2024-10-01.tif")
distwild_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/dist_to_wild_3km_pred_crop_2024-10-01.tif")
distcrit_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/dist_to_crithab_3km_pred_crop_2024-10-01.tif")
millchng_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/millchange_interp-2_2024-10-01.tif")
fedrich_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/conus_fed_rich_crop_2024-10-01.tif")
treecov_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/conus_tree_cover_crop_2024-10-01.tif")
treeage_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/conus_tree_age_crop_2024-10-01.tif")
forgain_crop <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/forest_gain_3km_crop_2024-10-01.tif")
whp_crop <-  rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/merged/conus_whp_3km_agg_interp_crop_2024-09-27.tif")


#Check alignment and stack the rasters
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



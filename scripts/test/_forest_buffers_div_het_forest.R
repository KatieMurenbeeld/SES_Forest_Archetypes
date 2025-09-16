library(tidyverse)
library(terra)
library(sf)



# Load the shapefiles with the uncertainty and heterogeneity for each forest and region

## heterogeneity
shan_h_sf <- read_sf(here::here("data/processed/nfbuffers_shan_h_nf_2025-08-28.shp"))

## uncertainty
ent_sf <- read_sf(here::here("data/processed/nfbuffers_ent_nf_2025-08-28.shp"))

# Join all the forest values into one table
shan_h_all <- shan_h_sf %>%
  dplyr::select(FORESTO, shan_dv, shn_dv_) %>%
  rename(FORESTORGC = FORESTO, 
         shan_dv_all = shan_dv, 
         shan_dv_sc_all = shn_dv_)

shan_h_all_df <- as.data.frame(st_drop_geometry(shan_h_all))

ent_df <- as.data.frame(st_drop_geometry(ent_sf))

uncert_het_nf_df <- ent_df %>%
  left_join(shan_h_all_df, by='FORESTORGC')

# save the csv
write_csv(uncert_het_nf_df, here::here(paste0("outputs/nfbuffers_div_ent_nf_", Sys.Date(), ".csv")))



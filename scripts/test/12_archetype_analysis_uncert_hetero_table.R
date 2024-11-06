library(tidyverse)
library(terra)
library(sf)



# Load the shapefiles with the uncertainty and heterogeneity for each forest and region

## heterogeneity
shan_h_sf <- read_sf(here::here("data/processed/shan_h_nf_2024-11-05.shp"))
shan_h_reg_sf <- read_sf(here::here("data/processed/shan_h_reg_2024-11-05.shp"))

shan_h_eco_sf <- read_sf(here::here("data/processed/shan_h_eco_nf_2024-11-06.shp"))
shan_h_eco_reg_sf <- read_sf(here::here("data/processed/shan_h_eco_reg_2024-11-06.shp"))

shan_h_soc_sf <- read_sf(here::here("data/processed/shan_h_soc_nf_2024-11-06.shp"))
shan_h_soc_reg_sf <- read_sf(here::here("data/processed/shan_h_soc_reg_2024-11-06.shp"))

## uncertainty
ent_sf <- read_sf(here::here("data/processed/ent_nf_2024-11-01.shp"))
ent_reg_sf <- read_sf(here::here("data/processed/ent_reg_2024-11-01.shp"))

# Join all into one table
shan_h_all <- shan_h_sf %>%
  dplyr::select(FORESTO, shan_dv, shn_dv_) %>%
  rename(FORESTORGC = FORESTO, 
         shan_dv_all = shan_dv, 
         shan_dv_sc_all = shn_dv_)

shan_h_all_df <- as.data.frame(st_drop_geometry(shan_h_all))

shan_h_eco <- shan_h_eco_sf %>%
  dplyr::select(FORESTO, shn_d_3, shn___3, shn_d_6, shn___6) %>%
  rename(FORESTORGC = FORESTO, 
         shan_dv_eco_k3 = shn_d_3, 
         shan_dv_sc_eco_k3 = shn___3,
         shan_dv_eco_k6 = shn_d_6, 
         shan_dv_sc_eco_k6 = shn___6)

shan_h_eco_df <- as.data.frame(st_drop_geometry(shan_h_eco))

shan_h_soc <- shan_h_soc_sf %>%
  dplyr::select(FORESTO, shn_d_3, shn___3, shn_d_6, shn___6) %>%
  rename(FORESTORGC = FORESTO, 
         shan_dv_soc_k3 = shn_d_3, 
         shan_dv_sc_soc_k3 = shn___3,
         shan_dv_soc_k6 = shn_d_6, 
         shan_dv_sc_soc_k6 = shn___6)

shan_h_soc_df <- as.data.frame(st_drop_geometry(shan_h_soc))




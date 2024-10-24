library(tidyverse)
library(terra)
library(raster)
library(sf)
library(ggplot2)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(future)
library(classInt)
library(elsa)



#----Load the felsa rasters----
fuzzy_elsa_all <- rast(here::here("outputs/SGFCM_k6_felsa_2024-10-22.tif"))
fuzzy_elsa_eco <- rast(here::here("outputs/SGFCM_eco_felsa_2024-10-22.tif"))
fuzzy_elsa_soc <- rast(here::here("outputs/SGFCM_soc_k3_felsa_2024-10-22.tif"))
fuzzy_elsa_soc_k6 <- rast(here::here("outputs/SGFCM_soc_k6_felsa_2024-10-22.tif"))

# create a raster stack of the fuzzy elsas
felsa_all_eco <- c(fuzzy_elsa_all, fuzzy_elsa_eco)
felsa_all_soc <- c(fuzzy_elsa_all, fuzzy_elsa_soc)
felsa_eco_sco <- c(fuzzy_elsa_eco, fuzzy_elsa_soc)

names(felsa_all_eco) <- c("f_elsa_all", "f_elsa_eco")
names(felsa_all_soc) <- c("f_elsa_all", "f_elsa_soc")
names(felsa_eco_sco) <- c("f_elsa_eco", "f_elsa_soc")


felsa_all_eco_cor_5 <- focalPairs(felsa_all_eco, w = 5, "pearson") 
felsa_all_eco_cor_5
plot(felsa_all_eco_cor_5$lyr1)

felsa_all_soc_cor_5 <- focalPairs(felsa_all_soc, w = 5, "pearson") 
felsa_all_soc_cor_5
plot(felsa_all_soc_cor_5$lyr1)

felsa_eco_soc_cor_5 <- focalPairs(felsa_eco_sco, w = 5, "pearson") 
felsa_eco_soc_cor_5
plot(felsa_eco_soc_cor_5$lyr1)

stk <- c(fuzzy_elsa_all, fuzzy_elsa_eco, fuzzy_elsa_soc)
plot(stk)
names(stk) <- c("all", "eco", "soc")
ts2 <- lm(all ~ eco, data=stk)
ts2
summary(ts2)

ts3 <- lm(all ~ soc, data=stk)
summary(ts3)
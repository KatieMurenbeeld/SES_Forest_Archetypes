#===============================================================================
# PARAMETER SELCTION FOR FUZZY C-MEANS CLUSTERING
# Within this script we will select the appropriate k, m, beta, and alpha
# values for a spatial (generalized) fuzzy c-means model. Following the workflow
# of Jeremy Gelb (https://jeremygelb.github.io/geocmeans/articles/introduction.html)
# we will:
# 1. Determine an appropriate range of k values by investigating an elbow plot
#    from a classical k-means model using stats::kmeans(). We will need to remove 
#    any NAs from the data. 
# 2. Determine appropriate ranges or values of k - m combinations using the 
#    geocmeans::select_parameters.mc(algo = "FCM"). Here the dataset is 
#    reformatted for use in geocmeans.
# 3. Determine the appropriate ranges or values of k-m-beta combinations 
#    combinations using the geocmeans::select_parameters.mc(algo = "GFCM") and
#    the reformatted dataset.
# 4. Determine the appropriate ranges or values of k-m-beta-window size-alpha
#    combinations using the geocmeans::select_parameter.mc(algo = "SGFCM") and
#    the reformatted dataset.
#===============================================================================

# Load required packages
library(tidyverse)
library(terra)
library(raster)
library(ggplot2)

## update the future.globals.maxSize to avoid issues working with the future 
## package
options(future.globals.maxSize = 1000 * 1024^2)

source(here::here("scripts/functions/parameterization_functions.R"))

# 1. Load the scaled data created in name_of_script.R
# ------------------------------------------------------------------------------
nfbuff_sc <- rast(here::here("data/processed/nf_buffers_all_attributes_cropped_then_scaled_2025-11-06.tif"))
nfbuff <- rast(here::here("data/processed/nf_buffers_all_attributes_2025-11-06.tif"))

# 2. Reformat the raster as a data frame and (as a precaution) remove any NAs
# ------------------------------------------------------------------------------
df_na <- as.data.frame(nfbuff_sc, na.rm=FALSE) # Extract values including NAs
df_na_complete <- na.omit(df_na)           # Remove rows with any NA values

## Optional: Select different sets of attributes
df_na_bio <- df_na_complete %>%
  dplyr::select(treecov, forprod, forgain, treeage, tempseas, precseas, rough, whp)

df_na_comm <- df_na_complete %>%
  dplyr::select(lesshs, hsbrd, engbrd, travtime, pm25, aip, netmig, comm_cap, 
                pct_forpay, pct_delmill)

df_na_rules <- df_na_complete %>%
  dplyr::select(distcrit, distwild, fedrich)


# 3. Use the elbow_plots() function from parameterization_functions.R
# ------------------------------------------------------------------------------
## elbow_plots(data, num_k, "file_name", option = "R2" or "INERT")
## This function will take the raster data as a data frame (data), the 
## maximum number of cluster centers, k, to test. k = 2:(num_k). 
## The function will print the plot and save the plot based on the file_name 
## and num_k. 
## The options let you select elbow plots based on:
##            r2 values (between cluster sum of squares / total sum of scares) 
##            inertia values (total within cluster sum of squares)

elbow_plots(df_nat_rules, 10, "test_elbow_plots_func", option = "R2")
elbow_plots(df_nat_rules, 10, "test_elbow_plots_func", option = "INERT")
elbow_plots(df_nat_rules, 10, "test_elbow_plots_func", option = "cat")


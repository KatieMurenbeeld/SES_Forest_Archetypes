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


# Autodetect elbow? ### Doesn't really work. Will cut later.##########
#-------------------------------------------------------------------------------
seg_threshold <- 0.95 #Set this to your desired target

#The angle between three points
segments_gain <- function(p1, v, p2){
  vp1 <- sqrt(sum((p1 - v)^2))
  vp2 <- sqrt(sum((p2 - v)^2))
  p1p2 <- sqrt(sum((p1 - p2)^2))
  
  angle <- acos((vp1^2 + vp2^2 - p1p2^2) / (2 * vp1 * vp2)) / pi
  return(angle)
}

#Normalize the data
criterion <- R2s
criterion <- (criterion - min(criterion) / max(criterion) - min(criterion))

K <- 2:50
#Compute the angles
seg_gains <- c(
  0,
  sapply(2:(length(K) - 1), function(i) {
    p1 <- c(K[i - 1], criterion[i - 1])
    v  <- c(K[i],     criterion[i])
    p2 <- c(K[i + 1], criterion[i + 1])
    
    segments_gain(p1, v, p2)
  }),
  NA
)

#Get the first index satisfying the threshold
kIdx <- which(seg_gains > seg_threshold)[1]
kIdx



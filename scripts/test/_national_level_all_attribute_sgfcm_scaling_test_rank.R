################################################################################
# REVIEW FUZYY C-MEANS PARAMETER SELECTION RESULTS WITH RANK SCALED DATA      ##
# 1. Download the original (unscaled) raster stack                            ##
#     1.1 Rank scale and normalize the data                                   ##
# 2. Quickly review a few variables distributions and plots pre and post scale##
# 2. Run a SGFCM with the same parameters that were optimized using min-max   ##
# scaling                                                                     ##
# 3. Plot the resulting 32 clusters                                           ##
# 4. Save the SGFCM as a RDS and save the figure                              ##
# 5. Use the select_params.mc() to review the Silhouette index, Explained     ##
# inertia and Spatial consistency                                             ##
#                                                                             ##
################################################################################

# 0. Load libraries
library(tidyverse)
library(terra)
library(raster)
library(sf)
library(ggplot2)
library(geocmeans)
library(RColorBrewer)
library(viridis)


# 1. Download the original (unscaled) raster stack
#-------------------------------------------------------------------------------
rst <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")

## 1.1 Rank scale the data
# Code from the AI results from a google search 
# "apply rank scaling to all layers in a raster R"

# Define a rank scaling function (values range from 0 to 1)
rank_scale_function <- function(x) {
  # na.last = "keep" ensures NAs remain NA
  ranked_values <- rank(x, na.last = "keep")
  # Scale the ranks to a 0-1 range
  scaled_ranks <- (ranked_values - min(ranked_values, na.rm = TRUE)) / 
    (max(ranked_values, na.rm = TRUE) - min(ranked_values, na.rm = TRUE))
  return(scaled_ranks)
}

# Apply the rank scaling function to all layers
rst_rank <- app(rst, fun = rank_scale_function)

# 2. Check the distributions and rasters for a few variables
#-------------------------------------------------------------------------------

## Unscaled
hist(rst$aip)
plot(rst$aip)
## Rank scaled and normalized
hist(rst_rank$aip)
plot(rst_rank$aip)

## Unscaled
hist(rst$pct_forpay)
plot(rst$pct_forpay)
## Rank scaled and normalized
hist(rst_rank$pct_forpay)
plot(rst_rank$pct_forpay)

## Unscaled
hist(rst$travtime)
plot(rst$travtime)
## Rank scaled and normalized
hist(rst_rank$travtime)
plot(rst_rank$travtime)

## Unscaled
hist(rst$treeage)
plot(rst$treeage)
## Rank scaled and normalized
hist(rst_rank$treeage)
plot(rst_rank$treeage)


# 3. Run a SGFCM with the "final" parameters found during the optimization 
# using min-max scaled data
#-------------------------------------------------------------------------------

## Format for use in geocmeans
dataset <- lapply(names(rst_rank), function(n){
  aband <- rst_rank[[n]]
  return(aband)
})
names(dataset) <- names(rst_rank)

## Run a SGFCM with the "final" parameter values

w1 <- matrix(1, nrow = 3, ncol = 3)

## alpha = 0.3
SGFCM_all_result_alpha03_rank_scale <- SGFCMeans(dataset, 
                                                 k = 32, 
                                                 m = 1.3, 
                                                 standardize = FALSE, # make sure this is false
                                                 lag_method = "mean",
                                                 window = w1, 
                                                 alpha = 0.3, 
                                                 beta = 0.1,
                                                 seed = 6891, # make sure to set a seed
                                                 tol = 0.001, 
                                                 verbose = TRUE, 
                                                 init = "kpp")

# 4. Plot the resulting clusters as crisp clusters
#-------------------------------------------------------------------------------

## Retrieve the cluster rasters from the SGFCM result
map_SGFCM_all_result_alpha03_rank_scale <- rast(SGFCM_all_result_alpha03_rank_scale$rasters)
## Plot the clusters
plot(map_SGFCM_all_result_alpha03_rank_scale[["Groups"]])



# 5. Save the SGFCM model results as a RDS and save the rasters from the SGFCM
# result
#-------------------------------------------------------------------------------

## Save the SGFCM model result
saveRDS(SGFCM_all_result_alpha03_rank_scale, 
        paste0("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/outputs/national_level/national_level_sgfcm_cluster_results_alpha03_rank_scale_",
               Sys.Date(), ".rds"))

## Save the cluster raster from the SGFCM model result
writeRaster(map_SGFCM_all_result_alpha03_rank_scale[["Groups"]], 
            filename = paste0("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/outputs/national_level/national_level_sgfcm_cluster_results_alpha03_rank_scale_", 
                                                            Sys.Date(), ".tif"), overwrite=TRUE)

# 6. Calculate the evaluation metrics using select_parameters.mc
# Here we use this instead of the calcSilhouetteIdx() in geocmeans
# because the raster is too large and will draw an error
# Using select_parameters.mc() will still draw a warning and the Silhouette 
# index is calculated on a subset of the data 
#-------------------------------------------------------------------------------

## You may need to adjust the future.globals to run this
options(future.globals.maxSize = 768 * 1024^2)

## Run the select.parameters.mc() with the same parameter combination used in the 
## SGFCM in #3. 
SGFCMvalues_rank_scale <- select_parameters.mc(algo = "SGFCM", 
                                               data = dataset, 
                                               standardize = FALSE, # make sure this is set to FALSE
                                               k = 32, 
                                               m = 1.3,
                                               beta = 0.1, 
                                               alpha = 0.3,
                                               window = w1, 
                                               lag_method = "mean",
                                               spconsist = TRUE, # make sure this is true since we are running a spatial model
                                               indices = c("XieBeni.index", 
                                                           "Explained.inertia",
                                                           "Negentropy.index", 
                                                           "Silhouette.index"),
                                               seed = 1234, # make sure to set a seed
                                               verbose = TRUE) 

## Review the results
print(SGFCMvalues_rank_scale)


# 7. Check that the evaluation metrics using the spConsistency(), 
# calcexplainedInertia(), and calcSilhouetteIdx()
# are close to those calculated in the select_parameters.mc() in #6
#-------------------------------------------------------------------------------

## Check the spatial consistency values
consistIndex <- spConsistency(SGFCM_all_result_alpha03_rank_scale, window = w1, nrep = 100)
## Review the results
print(consistIndex)
## Plot the results (The spatial consistency from the select_parameters.mc = 2.25)
ggplot() +
  geom_histogram(aes(x = consistIndex$samples),
                 bins = 30, fill = "white", color = "black") +
  geom_vline(aes(xintercept = consistIndex$Mean),
             color = "red", linetype="dashed", size = 1) +
  geom_text(aes(x = consistIndex$Mean+0.0015, y = 43,
                label = round(consistIndex$Mean,2))) +
  labs(x = "Spatial Inconsistency Index", y = "")

## The resulting values are close, but not the same. 
## select_parameters.mc = 2.25 vs spConsistency = 2.08 

## Check the calcexplainedInertia() 
calcEI <- calcexplainedInertia(as.matrix(as.data.frame(rst_rank, xy = FALSE)), SGFCM_all_result_alpha03_rank_scale$Belongings)
print(calcEI)
# 0.67 for both

## And check the calcSilhouetteIdx() 
calcSI <- calcSilhouetteIdx(as.matrix(as.data.frame(rst_rank, xy = FALSE)), SGFCM_all_result_alpha03_rank_scale$Belongings)
print(calcSI)
# 0.39 for both
#===============================================================================
# PARAMETER SELCTION FOR FUZZY C-MEANS CLUSTERING
# Within this script we will select the appropriate k, m, beta, and alpha
# values for a spatial (generalized) fuzzy c-means model. Following the workflow
# of Jeremy Gelb (https://jeremygelb.github.io/geocmeans/articles/introduction.html)
# we will:
# 1. Determine an appropriate range of k values by investigating an elbow plot
#    from a classical k-means model using stats::kmeans(). We will need to remove 
#    any NAs from the data. 
#===============================================================================

# 0. Load libraries
library(tidyverse)
library(terra)
library(raster)
library(sf)
library(ggplot2)
library(geocmeans)
library(RColorBrewer)
library(viridis)


# 1. Download the original rank scaled raster stack
#-------------------------------------------------------------------------------
rst <- rast("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/processed/rast_stack_all_attributes_rank_scale_2026-02-10.tif")

# 2. Use stats::kmeans() to determine an appropriate range of k values
# ------------------------------------------------------------------------------

## Reformat the raster as a data frame and (as a precaution) remove any NAs
df_nat <- as.data.frame(rst, na.rm=FALSE) # Extract values including NAs
df_nat_complete <- na.omit(df_nat)           # Remove rows with any NA values

## Calculate the R^2 from the classical k-means clustering for a range of 
## k values. 
R2s <- sapply(2:150, function(k){
  Clust <- k_means(df_nat_complete, centers=k, iter.max = 150)
  R2 <- Clust$betweenss / Clust$totss
  return(R2)
})

## Calculate the Inertia from the classical k-means clustering for a range of 
## k values. 
INERTs <- sapply(2:150,function(k){
  Clust <- kmeans(df_nat_complete, centers=k, iter.max = 150)
  INERT <- Clust$tot.withinss
  return(INERT)
})

# 3. Create a data frame of the R^2 and K values and of the inertia and K values
# ------------------------------------------------------------------------------

## Data frame for the R^2 and K values
Df_r2 <- data.frame(K=2:150,
                    R2 = R2s)

## Create a data frame of the Inertia and K values.
Df_INERT <- data.frame(K=2:150,
                       INERT = INERTs)


# 4. Generate the elbow plots for R^2 x K and Inertia x K. Save the plots
# ------------------------------------------------------------------------------

## Generate the elbow plot for R^2 values and save.
k_r2 <- ggplot(Df_r2)+
  geom_line(aes(x=K,y=R2s))+
  geom_point(aes(x=K,y=R2s),color="red")+
  xlab("Number of groups")+
  ylab("R2 of classification")
k_r2

## save the plot
ggsave(here::here(paste0("outputs/plots/national_level_all_param_selection_kmeans_rank_sc_", 
                         Sys.Date(), ".jpeg")), 
       plot = k_r2, height = 6, width = 10, dpi = 300)

## Generate the elbow plot and save.
k_inert <- ggplot(Df_INERT)+
  geom_line(aes(x=K,y=INERTs))+
  geom_point(aes(x=K,y=INERTs),color="red")+
  xlab("Number of groups")+
  ylab("Inertia (within cluster sum-of-squares) of classification")
k_inert

## save the plot
ggsave(here::here(paste0("outputs/plots/national_level_all_param_selection_kmeans_inertia_rank_sc_", 
                         Sys.Date(), ".jpeg")), 
       plot = k_inert, height = 6, width = 10, dpi = 300)




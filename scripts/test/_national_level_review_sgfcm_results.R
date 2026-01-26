################################################################################
# REVIEW FUZYY C-MEANS PARAMETER SELECTION RESULTS FROM NATIONAL LEVEL DATA   ##
# 1. Download and combine all sgfmc_all_attri_*.csv results                   ##
# 1.1 Note that the xxx.R script was run on a high performance computer       ##
# 2. Filter for parameter combinations with explained inertia >= 0.5          ##
# 3. Plot the evaluation metrics by k with a facet wrap by m.                 ##
# 4. Save the figure                                                          ##
#                                                                             ##
################################################################################

# 0. Load libraries
library(tidyverse)
library(here)
library(ggplot2)
library(patchwork)
library(scales)

# 1. Download and combine the parameter selection results
#-------------------------------------------------------------------------------
## Get a list of all the files in the directory
file_list <- list.files(path = here::here("outputs/national_level/sgfcm/"), 
                        pattern = "*.csv", 
                        full.names = TRUE)


## Read all files and combine into a dataframe
sgfcm_df <- map_dfr(file_list, read_csv, .id = "source_file")


# 2. Filter for Spatial (In)Consistency <= 0.70 (rounded)
#-------------------------------------------------------------------------------

df_filt <- sgfcm_df %>%
  mutate(spConsistency = round(spConsistency, digits = 2),
         Silhouette.index = round(Silhouette.index, digits = 2)) %>%
  filter(spConsistency <= 0.7 & Silhouette.index >= 0.3)

## Select the evaluation metrics of interest and the parameters
df_filt <- df_filt %>%
  select(Explained.inertia, Silhouette.index, spConsistency, k, m, beta, alpha, window)

## Review the table.
## These are parameter combinations to test for the final clusters.
print(df_filt)

# 3. Save the table to a csv
#-------------------------------------------------------------------------------

write_csv(df_filt, here::here(paste0("outputs/national_level/tables/national_level_potential_sgfcm_parameters_",
                                     Sys.Date(), ".csv")))




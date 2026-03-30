################################################################################
# REVIEW FUZYY C-MEANS PARAMETER SELECTION RESULTS FROM NATIONAL LEVEL DATA   ##
# 1. Download and combine all sfmc_all_attri_*.csv results                    ##
# 1.1 Note that the xxx.R script was run on a high performance computer       ##
# 2. Filter for parameter combinations with explained inertia >= 0.5          ##
# 3. Save the filtered combinations as a table.                               ##
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
file_list <- list.files(path = here::here("outputs/national_level/sfcm/"), 
                        pattern = "*.csv", 
                        full.names = TRUE)

## Read all files and combine into a dataframe
sfcm_df <- map_dfr(file_list, read_csv, .id = "source_file")

# 2. Filter for Explained Inertia >= 0.50 (rounded)
#-------------------------------------------------------------------------------

df_ei05 <- sfcm_df %>%
  mutate(Explained.inertia = round(Explained.inertia, digits = 2)) %>%
  filter(Explained.inertia >= 0.5)

df_ei05_si03 <- sfcm_df %>%
  mutate(Explained.inertia = round(Explained.inertia, digits = 2),
         Silhouette.index = round(Silhouette.index, digits = 2)) %>%
  filter(Explained.inertia >= 0.5 & Silhouette.index >= 0.3)

df_ei05_si03_sp <- sfcm_df %>%
  mutate(Explained.inertia = round(Explained.inertia, digits = 2),
         Silhouette.index = round(Silhouette.index, digits = 2), 
         spConsistency = round(spConsistency, digits = 2)) %>%
  filter(Explained.inertia >= 0.5 & Silhouette.index >= 0.3 & spConsistency < 1.0)

df_ei06_si03 <- sfcm_df %>%
  mutate(Explained.inertia = round(Explained.inertia, digits = 2),
         Silhouette.index = round(Silhouette.index, digits = 2)) %>%
  filter(Explained.inertia >= 0.6 & Silhouette.index >= 0.3)

## Select the evaluation metrics of interest and the parameters
df_filt <- df_ei05_si03_sp %>%
  select(Explained.inertia, Silhouette.index, spConsistency, k, m, alpha, window)

## Review the table.
## These are parameter combinations to test for the final clusters.
print(df_filt)

# 3. Save the table to a csv
#-------------------------------------------------------------------------------

write_csv(df_filt, here::here(paste0("outputs/national_level/tables/national_level_potential_sfcm_para_results_",
                                     Sys.Date(), ".csv")))



################################################################################
# REVIEW FUZYY C-MEANS PARAMETER SELECTION RESULTS FROM NATIONAL LEVEL DATA   ##
# 1. Download and combine all gfmc_all_attri_*.csv results                     ##
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


# 1. Download and combine the parameter selection results
#-------------------------------------------------------------------------------
## Get a list of all the files in the directory
file_list <- list.files(path = here::here("outputs/national_level/gfcm/"), 
                        pattern = "*.csv", 
                        full.names = TRUE)

## Read all files and combine into a dataframe
gfcm_df <- map_dfr(file_list, read_csv, .id = "source_file")

# 2. Filter for Explained Inertia >= 0.50 (rounded)
#-------------------------------------------------------------------------------

df_ei05 <- gfcm_df %>%
  mutate(Explained.inertia = round(Explained.inertia, digits = 2)) %>%
  filter(Explained.inertia >= 0.5)

# 3. Plot Explained Inertia, Silhouette Index, and Xie Beni Index on y with k 
# on x and facet wrapped by m
#-------------------------------------------------------------------------------

df <- df_ei05

ei <- ggplot(df, aes(k, Explained.inertia)) + 
  geom_line() +
  facet_grid(rows = vars(beta), cols = vars(m)) +
  labs(title = "National Level GFCM Explained Inertia: Explained Inertia >= 0.5")

si <- ggplot(df, aes(k, Silhouette.index)) + 
  geom_line() +
  facet_grid(rows = vars(beta), cols = vars(m)) +
  labs(title = "National Level GFCM Silhouette Index: Explained Inertia >= 0.5")

ei_si <- ggplot(df, aes(k)) + 
  geom_line(aes(y=Silhouette.index, colour = "red")) +
  geom_line(aes(y=Explained.inertia, color = "blue")) + 
  facet_grid(rows = vars(beta), cols = vars(m)) +
  labs(title = "National Level GFCM: Explained Inertia >= 0.5")
ei_si

xb <- ggplot(df, aes(k, XieBeni.index)) + 
  geom_line() +
  facet_grid(rows = vars(beta), cols = vars(m)) +
  labs(title = "National Level GFCM Xie Beni Index: Explained Inertia >= 0.5")

# 4. Save the figures
#-------------------------------------------------------------------------------

ggsave(here::here(paste0("outputs/national_level/plots/national_level_gfcm_params_ei05_ei_", 
                         Sys.Date(), ".jpeg")), plot = ei,
         width = 6, height = 6, dpi = 300)

ggsave(here::here(paste0("outputs/national_level/plots/national_level_gfcm_params_ei05_si_", 
                         Sys.Date(), ".jpeg")), plot = si,
       width = 6, height = 6, dpi = 300)

ggsave(here::here(paste0("outputs/national_level/plots/national_level_gfcm_params_ei05_xb_", 
                         Sys.Date(), ".jpeg")), plot = xb,
       width = 6, height = 6, dpi = 300)












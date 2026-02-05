################################################################################
# COMPARE THE NATIONAL AND NATIONAL FOREST LEVEL DATA                         ##
# 1. Load the unscaled raster stacks for the national and national forest     ##
# level data                                                                  ##
# 2. Reformat the rasters into dataframes                                     ##
# 2.1 Reforamt the dataframes to make the boxplots or violin plots.           ##
# 3. Create box plots or violin plots that compare the variables between each ##
# df                                                                          ##
# 4. Save the figure                                                          ##  
# 5. Create a table that compares the means and medians for the variables     ##
# between each df                                                             ##
################################################################################

# 0. Load libraries
library(tidyverse)
library(dplyr)
library(here)
library(ggplot2)
library(terra)
library(raster)

# 1. Load the unscaled national and national forest level raster stacks
#-------------------------------------------------------------------------------

nat_rst <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")
nf_rst <- rast(here::here("data/processed/nf_buffers_all_attributes_2025-11-06.tif"))

# 2. Reformat the rasters into dataframes
#-------------------------------------------------------------------------------

nat_df <- as.data.frame(nat_rst, xy=FALSE)
nf_df <- as.data.frame(nf_rst, xy = FALSE)

## 2.1 Reformat the dataframes to create boxplots and violin plots

### Paste a "v_" in front of each column name
colnames(nat_df) <- paste("v_", colnames(nat_df), sep = "")
colnames(nf_df) <- paste("v_", colnames(nf_df), sep = "")

### Create a new column to show where each data point came from
nat_df$level <- "national"
nf_df$level <- "forest"

### Combine the dataframes vertically
df <- bind_rows(nat_df, nf_df)

### Reshape the dataframe to long form
long_df <- df %>%
  pivot_longer(cols = starts_with("v_"), 
               names_to = "variable_name", 
               values_to = "value")

# 3. Create box plots that compare the variables between each level 
# (i.e. national or national forest level)
#-------------------------------------------------------------------------------

ggplot(long_df, aes(x = variable_name, y = value, fill = level)) +
  geom_boxplot(position = position_dodge(0.8)) + # Dodge position for side-by-side boxes
  labs(title = "Comparison of Variables Across Data Frames",
       x = "Variable Name",
       y = "Value",
       fill = "Source Data") +
  #facet_wrap(~variable_name) +
  theme_minimal()

ggplot(df, aes(x = level, y = v_aip, fill = level)) +
  geom_boxplot() +
  labs(title = "Comparison of AIP",
       x = "Level",
       y = "AIP",
       fill = "Level") +
  theme_minimal()

box_compare <- ggplot(long_df, aes(x = level, y = value, fill = level)) +
  geom_boxplot() +
  facet_wrap(~ variable_name, scales = "free_y") + # Facet by variable names in columns
  labs(title = "Boxplot Comparison for Each Variable",
       x = "Level",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Adjust x-axis labels for readability

box_compare_nooutliers <- ggplot(long_df, aes(x = level, y = value, fill = level)) +
  geom_boxplot(outliers = FALSE) +
  facet_wrap(~ variable_name, scales = "free_y") + # Facet by variable names in columns
  labs(title = "Boxplot Comparison for Each Variable",
       x = "Level",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Adjust x-axis labels for readability

# 4. Save the boxplot figure
#-------------------------------------------------------------------------------

ggsave(here::here(paste0("outputs/plots/nat_nf_compare_boxplots_",
                         Sys.Date(), ".png")),
       box_compare, width = 10, height = 10, dpi = 300)

ggsave(here::here(paste0("outputs/plots/nat_nf_compare_boxplots_nooutliers_",
                        Sys.Date(), ".png")),
      box_compare_nooutliers, width = 10, height = 10, dpi = 300)

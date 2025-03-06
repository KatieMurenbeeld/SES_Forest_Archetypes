library(tidyverse)
library(terra)
library(raster)
library(tibble)

rst <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")

means <- global(rst, mean, na.rm=TRUE)
medians <- global(rst, median, na.rm=TRUE)
sds <- global(rst, sd, na.rm = TRUE)

raster_summ <- cbind(means, medians, sds)

raster_summ <- raster_summ %>%
  rename(median = global)

raster_summ_df <- tibble::rownames_to_column(raster_summ, "attribute")

write_csv(raster_summ_df, here::here(paste0("outputs/tables/raster_summary_stats_", 
                                         Sys.Date(), ".csv")))

library(tidyverse)
library(terra)
library(geocmeans)
library(sf)
library(here)
library(viridis)
library(raster)
library(ggcorrplot)
library(reshape2)


# 1. Load the cropped and cropped and scaled attributes
#-------------------------------------------------------------------------------
nf_crop <- rast(here::here("data/processed/nf_buffers_all_attributes_2025-11-06.tif"))
nf_crop_sc <- rast(here::here("data/processed/nf_buffers_all_attributes_cropped_then_scaled_2025-11-06.tif"))

## Make the rasters into dataframes for the gridded histograms and correlation
## plot
nf_crop_df <- as.data.frame(nf_crop, xy=FALSE)
nf_crop_sc_df <- as.data.frame(nf_crop_sc, xy = FALSE)

nf_crop_df_coord <- as.data.frame(nf_crop, xy = TRUE)
anyNA(nf_crop_df_coord)
#my_raster <- rasterFromXYZ(nf_crop_df_coord)
test_rst <- rast(nf_crop_df_coord)

layerCor.test <- layerCor(rast(nf_crop_df_coord), fun = "pearson", w=3, asSample=TRUE, use="everything")
layerCor.test$correlation

x <- xapp(test_rst$rough, test_rst$tempseas, fun=cor, use="na.or.complete")
x_fp <- focalPairs(test_rst, w = 3, fun = "pearson")
plot(x_fp)
# Plot the result
#plot(my_raster$aip)

# 2. Look at the distributions off all the variables
#-------------------------------------------------------------------------------
d <- melt(nf_crop_df)
ggplot(d,aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x") + 
  geom_histogram()

# 3. Look at the correlations between all of the variables
#-------------------------------------------------------------------------------
df_corr <- cor(nf_crop_df, method = "pearson")

df_corrplot <- ggcorrplot(df_corr, hc.order = TRUE, type = "lower", lab = TRUE) +
  theme_bw() + 
  theme(axis.text.x=element_text(size=14, angle=45, vjust=0.9, hjust=1, 
                                 margin=margin(-3,0,0,0)),
        axis.text.y=element_text(size=12),
        axis.title.x = element_blank(), axis.title.y = element_blank())

df_corrplot

ggsave(here::here(paste0("outputs/nf_level/plots/nf_level_attribute_correlation_plot_", Sys.Date(), ".png")), 
       plot = df_corrplot, width = 20, height = 20, dpi = 300)


# 4. Create plots of the distribution of the variables
#-------------------------------------------------------------------------------
d <- melt(nf_crop_df)
nf_level_hist <- ggplot(d, aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x") + 
  geom_histogram() + 
  labs(
    title = "N. Forest Level Histograms",
    subtitle = "bins = 30"
  )

ggsave(here::here(paste0("outputs/nf_level/plots/nf_level_attribute_histograms_", Sys.Date(), ".png")), 
       plot = nf_level_hist, width = 20, height = 20, dpi = 300)

# 5. Look at the spatial correlations between the variables
#-------------------------------------------------------------------------------

## example using rough, tempseas, and precseas

plot(nf_crop$rough)
plot(nf_crop$tempseas)
plot(nf_crop$precseas)

# Create subsets of pairs of raster layers
## e.g., rough and tempseas, rough and precseas, tempseas and precseas
## use focalPairs to calculate the pearson correlation between the two
## raster layers
## check the plots

subset_01_rst <- subset(nf_crop, subset = c("rough", "tempseas"))
subset_01_cor_rst <- focalPairs(subset_01_rst, w = 5, fun = "pearson", na.rm = TRUE)
plot(subset_01_cor_rst)

subset_02_rst <- subset(nf_crop, subset = c("rough", "precseas"))
subset_02_cor_rst <- focalPairs(subset_02_rst, w = 5, fun = "pearson", na.rm = TRUE)
plot(subset_02_cor_rst)

subset_03_rst <- subset(nf_crop, subset = c("tempseas", "precseas"))
subset_03_cor_rst <- focalPairs(subset_03_rst, w = 5, fun = "pearson", na.rm = TRUE)
plot(subset_03_cor_rst)

# I keep getting weird edge effects when I try to use focalPairs on data that
# was rasterized from vector data
subset_04_rst <- subset(nf_crop, subset = c("aip", "comm_cap"))
summary(subset_04_rst)
subset_04_cor_rst <- focalPairs(subset_04_rst, w = 5, fun = "pearson", na.rm = TRUE, expand = TRUE)
plot(subset_04_cor_rst)
plot(nf_crop$aip)
plot(nf_crop$comm_cap)
plot(subset_04_rst)
# could make a custom function to find a relationship...
# this is the example from r doc, but how could I find where x is high and y is 
# high?
subset_05_sc_rst <- subset(nf_crop_sc, subset = c("aip", "rough"))
subset_05_scz_rst <- subset(scale(nf_crop), subset = c("aip", "rough"))
plot(subset_05_scz_rst)
z <- focalPairs(subset_05_scz_rst, w=3, function(x, y) mean(x) - mean(y)) 
plot(z)
plot(subset_05_sc_rst$aip)
plot(subset_05_sc_rst$rough)
plot(subset_05_sc_rst)
# try with spearman rank correlation
spearman_fun <- function(x, y) {
  cor(x, y, method = "spearman", use = "pairwise.complete.obs")
}

# I'm back to thinking I need to look at values that are above or below 
# +/- 1 or +/- 2 standard deviations...maybe reclassify as quantiles?


spear <- focalPairs(subset_04_rst, w=3, fun = spearman_fun)
plot(spear)

kendall_fun <- function(x, y) {
  cor(x, y, method = "kendall", use = "pairwise.complete.obs")
}
kendall <- focalPairs(subset_04_rst, w=3, fun = kendall_fun)
plot(kendall)


## check that the rasters are correct. Add a small number to values and 
## then calculate the cor.
subset_04_rst_tiny <- subset_04_rst + (init(subset_04_rst, runif) * 1e-6)
plot(subset_04_rst_tiny)

subset_04_rst_tiny_corr <- focalPairs(subset_04_rst_tiny, w = 3, fun = "pearson", na.rm = FALSE)
plot(subset_04_rst_tiny_corr)

## the data is all aligned properly. The problem is in trying to use 
## pearson correlation for the rasterized vectors because the rasters have a
## uniform value.


subset_04_focalcor_rst <- focal(subset_04_rst, w=3)
plot(subset_04_focalcor_rst)

test_df <- as.data.frame(subset_04_rst, xy = TRUE)
test_df$corr_coef <- cor(test_df$aip, test_df$comm_cap, method = "pearson")
test_corr_coef_rst <- rasterize(test_df, nf_crop, field = "corr_coef", fun = "mean")
plot(test_corr_coef_rst)

subset_05_rst <- subset(nf_crop, subset = c("aip", "rough"))
subset_05_cor_rst <- focalPairs(subset_05_rst, w = 9, fun = "pearson", na.rm = TRUE, directions = 8)
plot(subset_05_cor_rst)
plot(nf_crop$aip, colNA="red")
plot(nf_crop$rough, colNA="red")

# from 
#https://statnmap.com/2024-12-06-spatial-correlation-between-rasters-using-terra/
subset_05_rst_nb <- rast(subset_05_rst, 1)
values(subset_05_rst_nb) <- 1:ncell(subset_05_rst)

matrix_subset_05_rst <- values(subset_05_rst) # stack as raster [MW]

focal_cor <- focal(
  x = subset_05_rst_nb,
  w = matrix(1, 5, 5),
  fun = function(x, y = matrix_subset_05_rst) {
    cor(y[x, 1], y[x, 2],
        use = "na.or.complete"
    )
  }
)
plot(focal_cor)

sum(is.na(nf_crop$aip[]))
adj <- adjacent(nf_crop$aip, cells=which(!is.na(nf_crop$aip[])), directions=8)


## try using xapp
aip.rst <- subset(nf_crop, subset = "aip")
comm_cap.rst <- subset(nf_crop, subset = "comm_cap")
rough.rst <- subset(nf_crop, subset = "rough")

x <- xapp(r_poly_crop, rough.rst, fun=cor)
x
plot(x)

my.cor <- function(x, y) {
  if (any(is.na(x)) || any(is.na(y))) return(c(NA, NA))
  unlist(cor.test(x, y)[c("estimate", "p.value")])
}

z <- xapp(aip.rst, comm_cap.rst, my.cor)

z

library(spatialEco)

test_spateco <- rasterCorrelation(aip.rst, comm_cap.rst, s = 3, type = "pearson")
test_spateco
plot(test_spateco)

# Set the projection
projection <- "epsg:5070"

# Load the data
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/BQKU4M
aip <- read_dta("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/aip_files/aip_counties_ideology_v2022a.dta")
# see 01_arch...aip_prep.R to get the aip_proj object
# Load the reference raster and reproject
ref_rast <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/merged/conus_whp_3km_agg_interp_crop_2024-09-27.tif")
ref_rast_proj <- project(ref_rast, projection)

# 2. Rasterize as lines/cover (captures edges)
aip_line <- st_cast(aip_proj, "MULTILINESTRING")
r_line <- rasterize(aip_line, ref_rast_proj, field = "mrp_ideology")
r_poly <- rasterize(aip_proj, ref_rast_proj, field = "mrp_ideology", touches = TRUE, update = TRUE)
plot(r_poly)

r_poly_crop <- crop(r_poly, nf_crop$aip, mask = TRUE)
plot(r_poly_crop)
plot(nf_crop$aip)

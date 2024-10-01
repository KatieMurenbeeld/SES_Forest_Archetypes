library(tidyverse)
library(sf)
library(terra)
library(raster)
library(tmap)
library(ggplot2)
library(viridis)
library(spdep)
library(gstat)
library(stars)

# This script is to test out inverse distance weighting
# to fill in missing data in the CEJST data and rasterize

#----Load the CEJST data----
cejst <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/usa.shp")


#----Set the projection
projection <- "epsg:5070"

# Filter the data WA
#pnw <-c("Idaho", "Oregon", "Washington")

# Find states with the most missing data
for (i in unique(cejst$SF)){
  print(i)
  tmp <- cejst %>%
    filter(SF == i)
  print(length(which(is.na(tmp$EBF_PFS))))
}

# Filter the for the states with lots of missing data for HSEF
miss <-c("Florida", "Georgia", "Alabama")
miss_cejst <- cejst %>%
  filter(SF %in% miss)

pnw_cejst <- cejst %>%
  filter(SF == "Georgia")

## check for validity, remove empty geometries, and reproject 
if (!all(st_is_valid(miss_cejst)))
  miss_cejst <- st_make_valid(miss_cejst)

miss_cejst <- miss_cejst %>%
  filter(!st_is_empty(.))

miss_proj <- miss_cejst %>%
  st_transform(projection)

st_is_longlat(miss_cejst)
st_is_longlat(miss_proj)

hsef.orig.map <- ggplot() + 
  geom_sf(data = miss_proj, aes(fill = HSEF)) +
  scale_fill_viridis_c(limits = c(0.0, 1.0)) + 
  theme_bw()
plot(hsef.orig.map)
#ggsave(filename = here::here("outputs/plots/hsef_orig_map.png"), hsef.orig.map,
#       width = 4, height = 2, dpi = 300)

# get centroids from polygons and grids
miss.pt <- miss_proj %>%
  st_centroid(.) %>%
  drop_na(HSEF)

grd.3km <- st_bbox(miss_proj) %>%
  st_as_stars(dx = 3000) %>%
  st_crop(miss_proj) 

grd.10km <- st_bbox(miss_proj) %>%
  st_as_stars(dx = 10000) %>%
  st_crop(miss_proj) 

# rasterize vector data to grd
grd.10km.rst <- rast(grd.10km) # make stars grid into raster
hsef.10km.rst <- rasterize(miss_proj, grd.10km.rst, field = "HSEF", fun = "mean") #rasterize HSEF 
plot(hsef.10km.rst)

grd.3km.rst <- rast(grd.3km) # make stars grid into raster
hsef.3km.rst <- rasterize(miss_proj, grd.3km.rst, field = "HSEF", fun = "mean") #rasterize HSEF 
plot(hsef.3km.rst)

# the grid of points for predictions
grd.3km.pt <- grd.3km %>%
  st_as_sf(.) %>%
  st_centroid(.)

grd.10km.pt <- grd.10km %>%
  st_as_sf(.) %>%
  st_centroid(.)

# the points with values
miss.3km.pt <- as.points(hsef.3km.rst) %>%
  st_as_sf(.)

miss.10km.pt <- as.points(hsef.10km.rst) %>%
  st_as_sf(.)

# IDW from https://r-spatial.org/book/12-Interpolation.html

ggplot() + 
  geom_sf(data = miss.pt, aes(color = HSEF))

i.poly.10km <- idw(HSEF~1, miss.pt, grd.10km)
i.poly.3km <- idw(HSEF~1, miss.pt, grd.3km)
i.grd.3km <- idw(HSEF~1, miss.3km.pt, grd.3km)
i.grd.10km <- idw(HSEF~1, miss.10km.pt, grd.10km)

i.poly.10km.rst <- rasterize(st_as_sf(i.poly.10km), grd.10km.rst, field = "var1.pred")
i.poly.3km.rst <- rasterize(st_as_sf(i.poly.3km), grd.3km.rst, field = "var1.pred")
i.grd.3km.rst <- rasterize(st_as_sf(i.grd.3km), grd.3km.rst, field = "var1.pred")
i.grd.10km.rst <- rasterize(st_as_sf(i.grd.10km), grd.10km.rst, field = "var1.pred")

plot(hsef.3km.rst)
plot(i.poly.3km.rst)
plot(i.poly.10km.rst)
plot(i.grd.3km.rst)
plot(i.grd.10km.rst)

hsef.3km.df <- as.data.frame(hsef.3km.rst, xy = TRUE)
i.poly.3km.df <- as.data.frame(i.poly.3km.rst, xy = TRUE) %>%
  rename(poly.idw.pred = var1.pred)
i.poly.10km.df <- as.data.frame(i.poly.10km.rst, xy = TRUE)
i.grd.3km.df <- as.data.frame(i.grd.3km.rst, xy = TRUE)
i.grd.10km.df <- as.data.frame(i.grd.10km.rst, xy = TRUE)

idw.3km <- ggplot() +
  geom_raster(data = i.grd.3km.df, aes(x = x, y = y, fill = var1.pred)) + 
  geom_sf(data = pnw_proj, fill = NA) +
  scale_fill_viridis_c(limits = c(0.0, 1.0)) + 
  theme_bw()
plot(idw.3km)
#ggsave(filename = here::here("outputs/plots/idw_3km_wa_hsef_testing.png"), idw.3km,
#       width = 4, height = 2, dpi = 300)

idw.10km <- ggplot() +
  geom_raster(data = i.grd.10km.df, aes(x = x, y = y, fill = var1.pred)) + 
  geom_sf(data = pnw_proj, fill = NA) +
  scale_fill_viridis_c(limits = c(0.0, 1.0)) + 
  theme_bw()
plot(idw.10km)
#ggsave(filename = here::here("outputs/plots/idw_10km_wa_hsef_testing.png"), idw.10km,
#       width = 4, height = 2, dpi = 300)

idw.poly <- ggplot() +
  geom_raster(data = i.poly.10km.df, aes(x = x, y = y, fill = var1.pred)) + 
  geom_sf(data = pnw_proj, fill = NA) +
  scale_fill_viridis_c(limits = c(0.0, 1.0)) + 
  theme_bw()
ggsave(filename = here::here("outputs/plots/idw_poly_10km_wa_hsef_testing.png"), idw.poly,
       width = 4, height = 2, dpi = 300)

#----old plots----
ggplot() + 
  geom_stars(data = i.poly.10km, 
                      aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  geom_sf(data = st_cast(pnw_proj, "MULTILINESTRING"))

ggplot() + 
  geom_stars(data = i.grd.3km, 
                      aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  geom_sf(data = st_cast(pnw_proj, "MULTILINESTRING"))

ggplot() + geom_stars(data = i.grd.10km, 
                      aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  geom_sf(data = st_cast(pnw_proj, "MULTILINESTRING")) 

# calculate the correlations between the original raster and the interpolated rasters
## create a raster stack
#rst3km_stk <- c(hsef.3km.rst, i.grd.3km.rst, i.poly.3km.rst)
rst3km_stk <- c(hsef.3km.rst, i.poly.3km.rst)
rst10km_stk <- c(hsef.10km.rst, i.grd.10km.rst, i.poly.10km.rst)


hsef10km_cor_5 <- focalPairs(rst10km_stk, w = 5, cor) 
hsef3km_cor_3 <- focalPairs(rst3km_stk, w = 3, "pearson", na.rm = TRUE) 
hsef3km_layercor <- layerCor(rst3km_stk, fun = cor, w = 3, use = "complete.obs") 
hsef3km_cor_5

plot(hsef10km_cor_5)
plot(hsef3km_cor_3)

# try with regular cor()
test_df <- left_join(i.grd.3km.df, hsef.3km.df)
test_df <- left_join(i.poly.3km.df, test_df)
test_df$cor <- cor(test_df$HSEF, test_df$var1.pred, use = "complete.obs")
test_df$cor2 <- cor(test_df$HSEF, test_df$poly.idw.pred, use = "complete.obs")
test_df$cor3 <- cor(test_df$var1.pred, test_df$poly.idw.pred, use = "complete.obs")
test_df$no_idea <- (test_df$var1.pred / test_df$poly.idw.pred)
test_df$no_idea2 <- (test_df$HSEF / test_df$var1.pred)

plot(x = test_df$x, y = test_df$y, color = test_df$no_idea)


which(test_df$cor < 1)

test_cor <- cor(values(i.grd.3km.rst$var1.pred), values(hsef.3km.rst$HSEF), use = "complete.obs")
test_cor2 <- cor(values(i.poly.3km.rst$var1.pred), values(hsef.3km.rst$HSEF), use = "complete.obs")

test_cor_rst <- rast(test_df)
plot(test_cor_rst$HSEF)
plot(test_cor_rst$no_idea2)




r <- rast(system.file("ex/logo.tif", package="terra"))
plot(r)
set.seed(0)
r[[1]] <- flip(r[[1]], "horizontal")
r[[2]] <- flip(r[[2]], "vertical") + init(rast(r,1), runif)
r[[3]] <- init(rast(r,1), runif)

plot(r)
x <- focalPairs(r, w=5, "pearson", na.rm=TRUE)
plot(x)

z <- focalPairs(r, w=9, function(x, y) mean(x) + mean(y))
plot(z)

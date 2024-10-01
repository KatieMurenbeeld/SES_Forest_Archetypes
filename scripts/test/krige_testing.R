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

#pnw_cejst <- cejst %>%
#  filter(SF %in% pnw)

pnw_cejst <- cejst %>%
  filter(SF == "Georgia")

## check for validity, remove empty geometries, and reproject 
if (!all(st_is_valid(pnw_cejst)))
  pnw_cejst <- st_make_valid(pnw_cejst)

pnw_cejst <- pnw_cejst %>%
  filter(!st_is_empty(.))

pnw_proj <- pnw_cejst %>%
  st_transform(projection)

st_is_longlat(pnw_cejst)
st_is_longlat(pnw_proj)

ggplot() + 
  geom_sf(data = pnw_proj, aes(fill = HSEF))

# Kriging from https://r-spatial.org/book/12-Interpolation.html and
# https://www.paulamoraga.com/book-spatial/kriging.html?q=variogram#kriging

# get centroids from polygons and a grid
pnw.pt <- pnw_proj %>%
  st_centroid(.) %>%
  drop_na(HSEF)

grd.3km <- st_bbox(pnw_proj) %>%
  st_as_stars(dx = 3000) %>%
  st_crop(pnw_proj) 

grd.10km <- st_bbox(pnw_proj) %>%
  st_as_stars(dx = 10000) %>%
  st_crop(pnw_proj) 

# rasterize vector data to grd
grd.10km.rst <- rast(grd.10km) # make stars grid into raster
hsef.10km.rst <- rasterize(pnw_proj, grd.10km.rst, field = "HSEF", fun = "mean") #rasterize HSEF 
plot(hsef.10km.rst)

grd.3km.rst <- rast(grd.3km) # make stars grid into raster
hsef.3km.rst <- rasterize(pnw_proj, grd.3km.rst, field = "HSEF", fun = "mean") #rasterize HSEF 
plot(hsef.3km.rst)

# the grid of points for predictions
grd.3km.pt <- grd.3km %>%
  st_as_sf(.) %>%
  st_centroid(.)

grd.10km.pt <- grd.10km %>%
  st_as_sf(.) %>%
  st_centroid(.)

# the points with values
pnw.3km.pt <- as.points(hsef.3km.rst) %>%
  st_as_sf(.)

pnw.10km.pt <- as.points(hsef.10km.rst) %>%
  st_as_sf(.)

# view and fit variograms
## view the sample variograms to set initial model parameters
v.3km <- variogram(HSEF~1, pnw.3km.pt)
plot(v.3km, plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(v.3km$dist)))

v.10km <- variogram(HSEF~1, pnw.10km.pt)
plot(v.10km, plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(v.10km$dist)))

## fit the variograms
v.3km.m <- fit.variogram(v.3km, vgm(0.007, "Exp", 175000, 0.0018))
plot(gamma ~ dist, v.3km, 
     xlim = c(0, 1.075 * max(v.3km$dist)), ylim = c(0, 1.05 * max(v.3km$gamma)),
     xlab = "distance h [m]", ylab = expression(gamma(h)))
lines(variogramLine(v.3km.m, 1.075 * max(v.3km$dist)), lty = 1, col = 'blue')
#text(v.3km$dist, v.3km$gamma, v.3km$np, pos = 4)

v.10km.m <- fit.variogram(v.10km, vgm(0.007, "Exp", 175000, 0.0018))
plot(gamma ~ dist, v.10km, 
     xlim = c(0, 1.075 * max(v.10km$dist)), ylim = c(0, 1.05 * max(v.10km$gamma)),
     xlab = "distance h [m]", ylab = expression(gamma(h)))
lines(variogramLine(v.10km.m, 1.075 * max(v.10km$dist)), lty = 1, col = 'blue')
#text(v.10km$dist, v.10km$gamma, v.10km$np, pos = 4)

# krige using https://www.paulamoraga.com/book-spatial/kriging.html?q=variogram#kriging
k.3km <- gstat(formula = HSEF ~ 1, data = pnw.3km.pt, model = v.3km.m)
k.10km <- gstat(formula = HSEF ~ 1, data = pnw.10km.pt, model = v.10km.m)


k.3km.pred <- predict(k.3km, grd.3km.pt)
k.10km.pred <- predict(k.10km, grd.10km.pt)

k.10km.pred.rst <- rasterize(k.10km.pred, grd.10km.rst, field = "var1.pred")
plot(hsef.10km.rst)
plot(k.10km.pred.rst)

k.3km.pred.rst <- rasterize(k.3km.pred, grd.3km.rst, field = "var1.pred")
plot(hsef.3km.rst)
plot(k.3km.pred.rst)
writeRaster(k.3km.pred.rst, filename = here::here("data/processed/k_3km_hsef_testing.tif"))

# test with terra::interpolate (for next time try to figure this out)
# create a different krige model to help fix error from interpolate
p <- st_as_sf(pnw.3km.pt, coords = c("x", "y"), crs = crs(grd.3km.rst))
p$x <- st_coordinates(p)[,1]
p$y <- st_coordinates(p)[,2]

v.test <- variogram(HSEF~1, p)
v.test.m <- fit.variogram(v.3km, vgm(0.007, "Exp", 175000, 0.0018))

k.test <- gstat(formula = HSEF ~ 1, data = p, model = v.test.m)
#coordinates(p) <- ~x+y
#kk.test <- krige(formula = HSEF ~ 1, data = p, locations = p)
## with a model built with an `sf` object you need to provide custom function
interpolate_gstat <- function(model, x, crs, ...) {
  v <- st_as_sf(x, coords=c("x", "y"), crs=crs)
  p <- predict(model, v, ...)
  as.data.frame(p)[,1:2]
}

k.3km.interp <- terra::interpolate(grd.3km.rst, k.test, fun=interpolate_gstat, debug.level = 1)


hsef.3km.df <- as.data.frame(hsef.3km.rst, xy = TRUE)
hsef.10km.df <- as.data.frame(hsef.10km.rst, xy = TRUE)
k.grd.3km.df <- as.data.frame(k.3km.pred.rst, xy = TRUE)
k.grd.10km.df <- as.data.frame(k.10km.pred.rst, xy = TRUE)

hsef.10km.map <- ggplot() +
  geom_raster(data = hsef.10km.df, aes(x = x, y = y, fill = HSEF)) + 
  geom_sf(data = pnw_proj, fill = NA) +
  scale_fill_viridis_c(limits = c(0.0, 1.0)) + 
  theme_bw()
ggsave(filename = here::here("outputs/plots/10km_wa_hsef_testing.png"), hsef.10km.map,
       width = 4, height = 2, dpi = 300)

k.10km.map <- ggplot() +
  geom_raster(data = k.grd.10km.df, aes(x = x, y = y, fill = last)) + 
  geom_sf(data = pnw_proj, fill = NA) +
  scale_fill_viridis_c(limits = c(0.0, 1.0)) + 
  theme_bw()
ggsave(filename = here::here("outputs/plots/krige_10km_wa_hsef_testing.png"), k.10km.map,
       width = 4, height = 2, dpi = 300)

k.3km.map <- ggplot() +
  geom_raster(data = k.grd.3km.df, aes(x = x, y = y, fill = last)) + 
  geom_sf(data = pnw_proj, fill = NA) +
  scale_fill_viridis_c(limits = c(0.0, 1.0)) + 
  theme_bw()
ggsave(filename = here::here("outputs/plots/krige_3km_wa_hsef_testing.png"), k.3km.map,
       width = 4, height = 2, dpi = 300)

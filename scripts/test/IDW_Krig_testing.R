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
  filter(SF == "Washington")

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

# for interpolating...not sure if I want to just fill 
# in the missing polygon or fill in a grid since
# I want to end up with a raster?

# Block kriging from https://r-spatial.org/book/12-Interpolation.html

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


# rasterize vector data to grd?
grd.rst <- rast(grd.10km) # make stars grid into raster
hsef.rst <- rasterize(pnw_proj, grd.rst, field = "HSEF", fun = "mean") #rasterize HSEF 
plot(hsef.rst)

# convert back to st object where centers of cells are points with HSEF values
# or put values onto centers?

# the grid of points for predictions
grd.3km.pt <- grd.3km %>%
  st_as_sf(.) %>%
  st_centroid(.)

grd.10km.pt <- grd.10km %>%
  st_as_sf(.) %>%
  st_centroid(.)

# the points with values
pnw.3km.pt <- as.points(hsef.rst) %>%
  st_as_sf(.)

pnw.10km.pt <- as.points(hsef.rst) %>%
  st_as_sf(.)

ggplot() + 
  geom_sf(data = pnw.pt, aes(color = HSEF))

ggplot() + 
  geom_sf(data = pnw.10km.pt, aes(color = HSEF))

ggplot() + 
  geom_sf(data = grd.3km.pt)

v <- variogram(HSEF~1, pnw.pt)
plot(v, plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(v$dist)))

v.3km <- variogram(HSEF~1, pnw.3km.pt)
plot(v.3km, plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(v.3km$dist)))

v.10km <- variogram(HSEF~1, pnw.10km.pt)
plot(v.10km, plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(v.10km$dist)))

v0 <- variogram(HSEF~1, pnw.pt, cutoff = 100000, width = 10000)
plot(v0, plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(v0$dist)))

v0.3km <- variogram(HSEF~1, pnw.3km.pt, cutoff = 100000, width = 10000)
plot(v0.3km, plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(v0.3km$dist)))

v.m <- fit.variogram(v0, vgm(0.003, "Exp", 30000, 1))
plot(gamma ~ dist, v0, 
     xlim = c(0, 1.075 * max(v0$dist)), ylim = c(0, 1.05 * max(v0$gamma)),
     xlab = "distance h [m]", ylab = expression(gamma(h)))
lines(variogramLine(v.m, 1.075 * max(v0$dist)), lty = 1, col = 'blue')
text(v0$dist, v0$gamma, v0$np, pos = 4)

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

v.m.cir <- fit.variogram(v0, vgm(0.003, "Cir", 30000, 1))
plot(gamma ~ dist, v0, 
     xlim = c(0, 1.075 * max(v0$dist)), ylim = c(0, 1.05 * max(v0$gamma)),
     xlab = "distance h [m]", ylab = expression(gamma(h)))
lines(variogramLine(v.m.cir, 1.075 * max(v0$dist)), lty = 1, col = 'blue')
text(v0$dist, v0$gamma, v0$np, pos = 4)

# krige using https://www.paulamoraga.com/book-spatial/kriging.html?q=variogram#kriging
k.3km <- gstat(formula = HSEF ~ 1, data = pnw.3km.pt, model = v.3km.m)
k.10km <- gstat(formula = HSEF ~ 1, data = pnw.10km.pt, model = v.10km.m)

k.3km.pred <- predict(k.3km, grd.3km.pt)
k.10km.pred <- predict(k.10km, grd.10km.pt)

ggplot() + 
  geom_sf(data = k.10km.pred, aes(color = var1.pred)) +
  #geom_sf(data = pnw.10km.pt, aes(color = HSEF)) +
  scale_color_viridis(name = "HSEF") + 
  theme_bw()

ggplot() + 
  geom_sf(data = pnw.10km.pt, aes(color = HSEF))+
  scale_color_viridis(name = "HSEF") + 
  theme_bw()

k.10km.pred.rst <- rasterize(k.10km.pred, grd.rst, field = "var1.pred")
plot(k.10km.pred.rst)

bk <- krige(HSEF~1, pnw.pt, pnw_proj, v.m)
bk.cir <- krige(HSEF~1, pnw.pt, pnw_proj, v.m.cir)

bk$kriging <- bk$var1.pred
bk$original <- pnw_proj$HSEF
bk$krigcir <- bk.cir$var1.pred

bk2 <- bk %>% 
  dplyr::select(original, kriging) %>%
  pivot_longer(1:2, names_to = "var", values_to = "HSEF")
bk2$var <- factor(bk2$var, levels = c("original", "kriging"))

bk3 <- bk %>% 
  dplyr::select(original, kriging, krigcir) %>%
  pivot_longer(1:3, names_to = "var", values_to = "HSEF")
bk3$var <- factor(bk3$var, levels = c("original", "kriging", "krigcir"))

bk4 <- bk %>% 
  dplyr::select(kriging, krigcir) %>%
  pivot_longer(1:2, names_to = "var", values_to = "HSEF")
bk4$var <- factor(bk4$var, levels = c("kriging", "krigcir"))

bk5 <- bk %>% 
  dplyr::select(original, krigcir) %>%
  pivot_longer(1:2, names_to = "var", values_to = "HSEF")
bk5$var <- factor(bk5$var, levels = c("original", "krigcir"))

ggplot() +
  geom_sf(data = bk, mapping = aes(fill = var1.pred))

ggplot() +
  geom_sf(data = bk, mapping = aes(fill = krigcir))

ggplot() +
  geom_sf(data = bk2, mapping = aes(fill = HSEF)) + 
  facet_wrap(~var)

ggplot() +
  geom_sf(data = bk3, mapping = aes(fill = HSEF)) + 
  facet_wrap(~var)

ggplot() +
  geom_sf(data = bk4, mapping = aes(fill = HSEF)) + 
  facet_wrap(~var)

ggplot() +
  geom_sf(data = bk5, mapping = aes(fill = HSEF)) + 
  facet_wrap(~var)

## krige to grid not tracts
### create a pnw grid (10km x 10km)
grd <- st_bbox(pnw_proj) %>%
  st_as_stars(dx = 10000) %>%
  st_crop(pnw_proj)

grd.3km <- st_bbox(pnw_proj) %>%
  st_as_stars(dx = 3000) %>%
  st_crop(pnw_proj)

k <- krige(HSEF~1, pnw.pt, grd, v.m)

ggplot() + geom_stars(data = k, aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  geom_sf(data = st_cast(pnw_proj, "MULTILINESTRING")) + 
  #geom_sf(data = pnw.pt) +
  coord_sf(lims_method = "geometry_bbox")

k.cir <- krige(HSEF~1, pnw.pt, grd, v.m.cir)

ggplot() + geom_stars(data = k.cir, aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  geom_sf(data = st_cast(pnw_proj, "MULTILINESTRING")) + 
  #geom_sf(data = pnw.pt) +
  coord_sf(lims_method = "geometry_bbox")


# IDW from https://r-spatial.org/book/12-Interpolation.html

ggplot() + 
  geom_sf(data = pnw.pt, aes(color = HSEF))

i <- idw(HSEF~1, pnw.pt, grd)

#i.3km <- idw(HSEF~1, pnw.pt, grd.3km)
i.3km <- idw(HSEF~1, pnw.3km.pt, grd.3km)

ggplot() + geom_stars(data = i, 
                      aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  geom_sf(data = st_cast(pnw_proj, "MULTILINESTRING"))

ggplot() + geom_stars(data = i.3km, 
                      aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  geom_sf(data = st_cast(pnw_proj, "MULTILINESTRING"))

# interpolation from cdc_prep on Matt's Forest Vulnerability repo
pnw.pt$x <- st_coordinates(pnw.pt)[,1]
pnw.pt$y <- st_coordinates(pnw.pt)[,2]
pnw.mod <- as(pnw.pt, "Spatial")

G1 <- gstat(id = "HSEF", formula = HSEF~1, locations = ~x+y,
            data = st_drop_geometry(pnw.pt),
            nmax = 10, set = list(idp = .975))
z <- interpolate(grd, G1, debug.level = 0, index = 1)


# Just fill in the polygon?
# Get a list of neighbors for each polygon
## queen case
nb_q <- pnw_proj %>%
  poly2nb(queen = TRUE)

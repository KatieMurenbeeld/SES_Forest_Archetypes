# Custom Data Preprocessing Functions for (name of project or publication)

#===============================================================================
# Function for padding all FIPS codes with 0s for a total of 5 characters
update_fips <- function(data_set) {
  data_set$FIPS <- as.character(data_set$FIPS)
  data_set$FIPS <- str_pad(data_set$FIPS, 5, side="left", pad="0")
  return(data_set)
}


#===============================================================================
# Function for rasterizing your variable.
# Making the raster into points in order to use inverse distance weighting 
# to predict missing values
idw_preds <- function(data_proj, ref_raster, lay, empty_grid){
  # data_proj = the data of interest at the correct projection
  # ref_raster = used in rasterize to set the projection and resolution
  # lay = the variable name
  # empty_grid = an empty grid at the correct resolution, needed to run the idw
  
  # First, use the reference raster to rasterize the vector variable
  var.rst <- rasterize(data_proj, ref_raster, field = lay, fun = "mean") 
  # Second, make the new raster into points
  var.pt <- as.points(var.rst) %>%
    st_as_sf(.)
  # Third, fill in the empty grid with the values and the interpolated values
  var.pred <- idw(var.pt[[1]]~1, var.pt, empty_grid)
  # Fourth, re-rasterize the data with ref_raster and the field = to the predicted values
  var.pred.rst <- rasterize(st_as_sf(var.pred), ref_raster, field = "var1.pred")
  # Fifth, change the name of the raster layer to the name of variable.pred
  names(var.pred.rst) <- paste0(lay, ".pred")
  # Return the raster
  return(c(orig.rst = var.rst, pred.rst = var.pred.rst))
}
################################################################################
# SCRIPT USED TO TEST CREATING SF POLYGONS FROM RADARCHART AND DETERMINING    ##
# THE % AREA OVERLAPPING FOR EACH POLYGON                                     ##
# 1. Create the SGFCM model and clusters following the geocmeans vignette.    ##
# 2. From the SGFCM belongings matrix, define the variables needed for the    ##
#    function                                                                 ##
# 3. Create a function that will return a sf polygon from the radarchart polygon
# 4. Use the function to create the sf polygons for each group/cluster        ##
# 5. Calculate the overlapping areas for the polygons                         ##
################################################################################


# Load libraries
library(geocmeans)
library(dplyr)
library(sf)

spdep::set.mcOption(FALSE)
spdep::set.coresOption(1L)

# 1. Create the SGFCM model and clusters
#----------------------------------------------------

# load the data
data(LyonIris)

# selecting the columns for the analysis
AnalysisFields <-c("Lden","NO2","PM25","VegHautPrt","Pct0_14",
                   "Pct_65","Pct_Img","TxChom1564","Pct_brevet","NivVieMed")

# rescaling the columns
Data <- sf::st_drop_geometry(LyonIris[AnalysisFields])
for (Col in names(Data)){
  Data[[Col]] <- scale(Data[[Col]])
}


# SGFCM clusters
#------------------------------------------------------
Neighbours <- poly2nb(LyonIris,queen = TRUE)
WMat <- nb2listw(Neighbours,style="W",zero.policy = TRUE)



SGFCM <- SGFCMeans(Data,WMat,k = 4,m=1.5, alpha=0.95, beta = 0.65,
                   tol=0.0001, standardize = FALSE, verbose = FALSE, seed = 456)



# 2. From the SGFCM belongings matrix, define the variables needed for the function
#---------------------------------------------------
belongmatrix <- SGFCM$Belongings
data <- Data
Groups <- ncol(belongmatrix)

Values <- do.call(rbind, lapply(1:Groups, function(i) {
  W <- belongmatrix[,i]
  return(apply(data,2, function(row){return(weighted.mean(row,W))}))
}))

Mins <- apply(Values, 2, min)
Maxs <- apply(Values, 2, max)

# 3. Create the function
#-------------------------------------------------
# This needs more commenting/documentation

create_poly <- function(group_number) {
  # This creates the data frame for use in the radarchart code
  # which requires the min, max, and weighted average score 
  # for each variable for each cluster/group
  Scores <- Values[group_number,]
  names(Scores) <- names(data)
  datam <- data.frame(rbind(Maxs, Mins, Scores))
  
  # code for getting the points for the radarchart
  n <- length(datam)
  theta <- seq(90, 450, length = n + 1)* (pi/180)
  theta <- theta[1:n]
  xx <- cos(theta)
  yy <- sin(theta)
  CGap <- ifelse(FALSE, 0, 1) #geocmeans uses centerzero=FALSE
  # This creates the guidelines for the radarchart
  # This is needed to scale the values to the radarchart
  seg <- 4 # usually 4 segments labeled 25%, 50%, 75%, 100%
  series <- length(datam[[1]])
  SX <- series-2
  #df <- datam
  # sets up the scaling
  for (i in 3:series) {
    #print(i)
    xxs <- xx
    yys <- yy
    scale <- CGap/(seg+CGap)+(datam[i,]-datam[2,])/(datam[1,]-datam[2,])*seg/(seg+CGap)
  }
  # calculate the x and y for each variable, scaled to the radarchart
  for (j in 1:n) {
    xxs[j] <- xx[j]*CGap/(seg+CGap)+xx[j]*(datam[i, j]-datam[2, j])/(datam[1, j]-datam[2, j])*seg/(seg+CGap)
    yys[j] <- yy[j]*CGap/(seg+CGap)+yy[j]*(datam[i, j]-datam[2, j])/(datam[1, j]-datam[2, j])*seg/(seg+CGap)
  }
  
  # create a matrix of the scaled coordinate points for the resulting polygon
  ply_mat <- cbind(xxs, yys)
  # need to close the polygon so add the first rows coords to the bottom
  ply_mat_closed <- rbind(ply_mat, ply_mat[1,])
  # create a list object
  ply_list <- list(ply_mat_closed)
  
  # from the polygon list object create sf polygons
  st_geom <- st_polygon(ply_list)
  st_sfc_geom <- st_sfc(st_geom)
  clusters_df <- data.frame(cluster = group_number)
  sf_polygon <- st_as_sf(clusters_df, geometry = st_sfc_geom)
  
  # return the final sf_polygon
  return(sf_polygon) 
}

# 4. Use the function to create the sf polygons
#------------------------------------------------------------

group1_ply <- create_poly(1)
group2_ply <- create_poly(2)
group3_ply <- create_poly(3)
group4_ply <- create_poly(4)

plot(group1_ply$geometry)
plot(group2_ply$geometry, add = TRUE)
plot(group3_ply$geometry, add = TRUE)
plot(group4_ply$geometry, add = TRUE)

# 5. Calculate the overlapping areas for the polygons
#  Probably a nice way to automate this to get a dataframe or matrix
#  with all of the areas for each combination
#-----------------------------------------------------------

overlap_1_2 <- st_intersection(group1_ply, group2_ply)
area_overlap_1_2 <- st_area(overlap_1_2)
area_overlap_1_2
st_area(group1_ply)
st_area(group2_ply)
area_overlap_1_2 / st_area(group1_ply) * 100
area_overlap_1_2 / st_area(group2_ply) * 100
plot(group1_ply$geometry)
plot(group2_ply$geometry, add = TRUE)

overlap_1_3 <- st_intersection(group1_ply, group3_ply)
area_overlap_1_3 <- st_area(overlap_1_3)
area_overlap_1_3

overlap_1_4 <- st_intersection(group1_ply, group4_ply)
area_overlap_1_4 <- st_area(overlap_1_4)
area_overlap_1_4

overlap_2_3 <- st_intersection(group2_ply, group3_ply)
area_overlap_2_3 <- st_area(overlap_2_3)
area_overlap_2_3

overlap_2_4 <- st_intersection(group2_ply, group4_ply)
area_overlap_2_4 <- st_area(overlap_2_4)
area_overlap_2_4

overlap_3_4 <- st_intersection(group4_ply, group3_ply)
area_overlap_3_4 <- st_area(overlap_3_4)
area_overlap_3_4
area_overlap_3_4 / st_area(group3_ply) * 100
area_overlap_3_4 / st_area(group4_ply) * 100
plot(group3_ply$geometry)
plot(group4_ply$geometry, add = TRUE)




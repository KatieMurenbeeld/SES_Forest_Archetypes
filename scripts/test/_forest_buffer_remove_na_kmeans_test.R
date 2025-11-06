# code from google AI response to "R terra::kmeans na.omit not working"
library(terra)

# Example SpatRaster with NAs (replace with your data)
r <- rast(here::here("data/processed/nf_buffers_all_attributes_cropped_then_scaled_2025-11-06.tif"))

# 1. Extract values to a data frame and remove NAs
df <- as.data.frame(r, na.rm=FALSE) # Extract values including NAs
df_complete <- na.omit(df)           # Remove rows with any NA values

# 2. Run k-means on the complete data frame
#k_clusters <- kmeans(df_complete, centers = 5, iter.max = 100, nstart = 10)
R2s <- sapply(2:50, function(k){
  Clust <- kmeans(df_complete, centers=k, iter.max = 150)
  R2 <- Clust$betweenss / Clust$totss
  return(R2)
})

Df_r2 <- data.frame(K=2:50,
                    R2 = R2s)

k_r2 <- ggplot(Df_r2)+
  geom_line(aes(x=K,y=R2s))+
  geom_point(aes(x=K,y=R2s),color="red")+
  xlab("Number of groups")+
  ylab("R2 of classification")
k_r2

INERTs <- sapply(2:50,function(k){
  Clust <- kmeans(na.omit(df_complete), centers=k, iter.max = 150)
  INERT <- Clust$tot.withinss
  return(INERT)
})

Df_INERT <- data.frame(K=2:50,
                       INERT = INERTs)

k_inert <- ggplot(Df_INERT)+
  geom_line(aes(x=K,y=INERTs))+
  geom_point(aes(x=K,y=INERTs),color="red")+
  xlab("Number of groups")+
  ylab("Inertia (within cluster sum-of-squares) of classification")
k_inert

k_clusters <- kmeans(df_complete, centers = 20, iter.max = 100, nstart = 10)
# 3. Associate clusters back to the original data frame (this is the key step)
# Create a vector for all cells, initialized to NA
clusters_all <- rep(NA, nrow(df))
# Fill in the cluster assignments for the non-NA cases
clusters_all[as.numeric(rownames(df_complete))] <- k_clusters$cluster

# 4. Create a new SpatRaster with the cluster results
r_clusters <- rast(r, nlyr=1) # Create an empty SpatRaster with same dimensions
values(r_clusters) <- clusters_all
names(r_clusters) <- "cluster"

# Plot the results
plot(r_clusters, main="K-means Clusters (NA values removed)")

# But will this work for geocmeans?
dataset <- lapply(names(df_complete), function(n){
  aband <- df_complete[[n]]
  return(aband)
})
names(dataset) <- names(df_complete)


## just a quick test with k = 2:5 and m = seq(1.1, 2, 0.5)
#----Use a non spatial and non generalized fuzzy c-means to determine number of k and value for m
future::plan(future::multisession(workers = 2))
FCMvalues <- select_parameters.mc(algo = "FCM", data = dataset, standardize = FALSE,
                                  k = 2:5, m = seq(1.1,2,0.5), spconsist = FALSE, 
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Negentropy.index", "Silhouette.index"),
                                  seed = 1234, verbose = TRUE) 
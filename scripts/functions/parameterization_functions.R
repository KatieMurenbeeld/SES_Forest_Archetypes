# Custom Parameterization Functions for (name of project or publication)

#===============================================================================
# Function for creating elbow plots with varying numbers of cluster centers (k)
# as a first step in parameterizing fuzzy cmeans. 
# This will help to identify an optimal number of k or a range of ks to test.

elbow_plots <- function(data, num_k, file_name, option = "R2") {
  ## data = the data frame to use in this function. This is the
  ## raster stack of attributes converted to a data frame and the NAs removed.
  ## num_k = the maximum number of cluster centers. This will result in a 
  ## range of cluster centers from 2:num_k
  ## file_name = prefix to help name the resulting jpeg file of the elbow plot
  ## num_k is also in the final jpeg file name
  ## option == R2 or INERT Can create an elbow plot based on r2 values or 
  ## inertia values
  
  if (option == "R2"){
  ## Calculate the R^2 from the classical k-means clustering for a range of 
  ## k values. 
    R2s <- sapply(2:num_k, function(k){
      Clust <- k_means(data, centers=k, iter.max = 150)
      R2 <- Clust$betweenss / Clust$totss
      return(R2)
      })
  
  ## Create a data frame of the R^2 and K values.
    Df_r2 <- data.frame(K=2:num_k,
                      R2 = R2s)
  
  ## Generate the elbow plot and save.
    k_r2 <- ggplot(Df_r2)+
      geom_line(aes(x=K,y=R2s))+
      geom_point(aes(x=K,y=R2s),color="red")+
      xlab("Number of groups")+
      ylab("R2 of classification")
    k_r2
    ggsave(here::here(paste0("outputs/plots/", file_name, 
                             "_param_selection_kmeans_r2_k",
                             num_k, "_", Sys.Date(), ".jpeg")), 
           plot = k_r2, height = 6, width = 10, dpi = 300)
  }
  else if (option == "INERT"){
  ## Calculate the Inertia from the classical k-means clustering for a range of 
  ## k values. 
  INERTs <- sapply(2:num_k,function(k){
    Clust <- kmeans(data, centers=k, iter.max = 150)
    INERT <- Clust$tot.withinss
    return(INERT)
  })
  
  ## Create a data frame of the Inertia and K values.
  Df_INERT <- data.frame(K=2:num_k,
                         INERT = INERTs)
  
  ## Generate the elbow plot and save.
  k_inert <- ggplot(Df_INERT)+
    geom_line(aes(x=K,y=INERTs))+
    geom_point(aes(x=K,y=INERTs),color="red")+
    xlab("Number of groups")+
    ylab("Inertia (within cluster sum-of-squares) of classification")
  k_inert
  ggsave(here::here(paste0("outputs/plots/", file_name, 
                           "_param_selection_kmeans_inertia_k",
                           num_k, "_", Sys.Date(), ".jpeg")),
         plot = k_inert, height = 6, width = 10, dpi = 300)
  }
  else {
    # Handle invalid options
    stop("Invalid option specified. Use 'R2' or 'INERT'.")
  }
}
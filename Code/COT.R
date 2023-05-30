find_outlier_measurement_clustering<-function(total,subject_name,time_name, measure) {
  #Data with the artificial outliers
  data <- total
  
  #Dissimilarity measure calculation requires the df to be in wide format
  data_wide <- data[, c(subject_name, time_name, measure)]
  data_wide <- spread(data_wide, get(time_name), get(measure))
  
  kids = data_wide[, 1]
  rownames(data_wide) <- kids
  data_wide = data_wide[, -c(1)]
  method = "EUCL"
  EUCL_dist = diss(as.matrix(data_wide), method)
  
  #Clustering tendency
  
  #fviz_dist(dist(data_wide), show_labels = FALSE) +
  #  labs(title = "get(measure)_data")
  
  #Optimal number of clusters
  #nb <- NbClust(
  #  data_wide,
  #  distance = "euclidean",
  #  min.nc = 2,
  #  max.nc = 10,
  #  method = "complete",
  #  index = "all"
  #)
  
  # Visualize the result
  #jpeg("EVAL_PLOTS/get(measure)_2_nb.jpeg")
  #print(fviz_nbclust(nb) + theme_minimal())
  #dev.off()
  
  
  # Clustering
  nk<-round(nrow(data_wide)/10)
  hc <- hclust(EUCL_dist)
  clust <- cutree(hc, nk)
  clust <- data.frame(kids, clust)
  
  n_outliers <- 0
  outliers_clust <- {
  }
  sum<-0
  for (i in 1:nk) {
    #print(paste0("Cluster ", i, ": ", nrow(clust[which(clust$clust == i), ])))
    if (nrow(clust[which(clust$clust == i), ]) < 10) {
      #print(i)
      sum<-sum+nrow(clust[which(clust$clust == i), ])
      n_outliers <- n_outliers + nrow(clust[which(clust$clust == i), ])
      outliers_clust <-
        rbind(outliers_clust, data[which(data[,subject_name] %in% clust[which(clust$clust ==
                                                                         i), ]$kids), ])
    }
  }
  #print(sum)
  outliers_c_kids<-unique(as.character(outliers_clust[,1]))
  #outliers_c_df<-data.frame(subject=outliers_c_kids, outlier_ct=rep("Y", length(outliers_c_kids)), stringsAsFactors = F)
  #outlier_df<-data.frame(subject=unique(data$subject), outlier_c=rep("N", length(unique(data$subject))), stringsAsFactors = F)
  outlier_df<-mutate(data, outlier_ct=rep("N", nrow(data)))
  for(s in outliers_c_kids) {
    outlier_df[which(outlier_df[,1]==s),]$outlier_ct<-"Y"
  }
  return(outlier_df)
}
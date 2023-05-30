find_outlier_trajectory_prototype<-function(total2, stdev=2, subject_name, time_name, measure_name) {
  #Data with the artificial outliers
  data <- total2
  
  #Dissimilarity measure calculation requires the df to be in wide format
  data_w <- data[, c(subject_name, time_name, measure_name)]
  data_w <- spread(data_w, get(time_name), get(measure_name))
  
  kids = data_w[, 1]
  rownames(data_w) <- kids
  data_w = data_w[, -c(1)]
  method = "EUCL"
  EUCL_dist = diss(as.matrix(data_w), method)
  
  #hc_wflz <- hclust(EUCL_dist)
  #clust <- cutree(hc_wflz, 2)
  clust<-kmeans(EUCL_dist, 2)
  clust <- data.frame(kids=as.character(kids), clust=clust$cluster, stringsAsFactors = F)
  #plot(hc_wflz, hang = -1)
  
  length(clust[which(clust$clust == "1"), ]$clust)
  length(clust[which(clust$clust == "2"), ]$clust)
  
  kids_clust1 <- clust[which(clust$clust == "1"), ]$kids
  clust_1 <- data_w[which(rownames(data_w) %in% kids_clust1), ]
  
  kids_clust2 <- clust[which(clust$clust == "2"), ]$kids
  clust_2 <- data_w[which(rownames(data_w) %in% kids_clust2), ]
  
  col_means_1 <- apply(clust_1, 2, mean, trim = .2)
  col_SD_1 <- apply(clust_1, 2, sd)
  col_means_2 <- apply(clust_2, 2, mean, trim = .2)
  col_SD_2 <- apply(clust_2, 2, sd)
  
  clusterViolators_1 <- {
  }
  RSScluster <- numeric(length(kids_clust1))
  for (i in 1:nrow(clust_1)) {
    error <- numeric(ncol(clust_1))
    for (j in 1:ncol(clust_1)) {
      error[j] <- as.numeric((clust_1[i, j] - col_means_1[j]) ^ 2)
      
    }
    RSScluster[i] <- sum(error)
    
  }
  meanCluster <- mean(RSScluster)
  sdCluster <- sd(RSScluster)
  
  for (k in seq_along(kids_clust1)) {
    if ((RSScluster[k] > (meanCluster + stdev * sdCluster)) ||
        (RSScluster[k] < (meanCluster - stdev * sdCluster))) {
      clusterViolators_1 <- rbind(clusterViolators_1, kids_clust1[k])
    }
  }
  
  clusterViolators_2 <- {
  }
  RSScluster <- numeric(length(kids_clust2))
  for (i in 1:nrow(clust_2)) {
    error <- numeric(ncol(clust_2))
    for (j in 1:ncol(clust_2)) {
      error[j] <- as.numeric((clust_2[i, j] - col_means_2[j]) ^ 2)
      
    }
    RSScluster[i] <- sum(error)
    
  }
  meanCluster <- mean(RSScluster)
  sdCluster <- sd(RSScluster)
  
  for (k in seq_along(kids_clust2)) {
    if ((RSScluster[k] > (meanCluster + stdev * sdCluster)) ||
        (RSScluster[k] < (meanCluster - stdev * sdCluster))) {
      clusterViolators_2 <- rbind(clusterViolators_2, kids_clust2[k])
    }
  }
  
  outliers_pt<-mutate(total2, outlier_pt=rep("N", nrow(total2)))
  
  for(s in c(clusterViolators_1, clusterViolators_2)) {
    outliers_pt[which(outliers_pt[,subject_name]==s),]$outlier_pt<-"Y"
  }
  
  return(outliers_pt)
  
}
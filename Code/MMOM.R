find_outlier_measurement_prototype<-function(total2, stdev=2, subject_name, time_name, measure_name) {
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
  
  data_p<-mutate(data, outlier_p=rep("N", nrow(data)))
  #data_p<-arrange(data_p, subject, ageinmonths)
  data_p<-arrange(data_p, .data[[subject_name]], .data[[time_name]])
  
  
  for(i in 1:nrow(clust_1)) {
    subject_data<-clust_1[i,]
    for(j in 1:length(subject_data)) {
      if((subject_data[j] < (col_means_1[j] - stdev*col_SD_1[j])) || (subject_data[j] > (col_means_1[j] + stdev*col_SD_1[j]))) {
        data_p[which(data_p[,subject_name]==(rownames(clust_1)[i]) & data_p[,time_name]==as.numeric(colnames(subject_data)[j])),]$outlier_p<-"Y"
      }
    }
  }
  
  for(i in 1:nrow(clust_2)) {
    subject_data<-clust_2[i,]
    for(j in 1:length(subject_data)) {
      if((subject_data[j] < (col_means_2[j] - stdev*col_SD_2[j])) || (subject_data[j] > (col_means_2[j] + stdev*col_SD_2[j]))) {
        data_p[which(data_p[,subject_name]==(rownames(clust_2)[i]) & data_p[,time_name]==as.numeric(colnames(subject_data)[j])),]$outlier_p<-"Y"
      }
    }
  }
  
  return(data_p)
}
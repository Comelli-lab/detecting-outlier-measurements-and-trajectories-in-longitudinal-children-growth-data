find_outlier_measurement_single_prototype<-function(total2, stdev=2, subject_name, time_name, measure_name) {
  #Data with the artificial outliers
  data <- total2
  
  data_w <- data[, c(subject_name, time_name, measure_name)]
  data_w <- spread(data_w, get(time_name), get(measure_name))
  
  kids = data_w[, 1]
  rownames(data_w) <- kids
  data_w = data_w[, -c(1)]
  
  col_means <- apply(data_w, 2, mean, trim = .2)
  col_SD <- apply(data_w, 2, sd)
  
  data_p<-mutate(data, outlier_sp=rep("N", nrow(data)))
  data_p<-arrange(data_p, .data[[subject_name]], .data[[time_name]])
  
  
  
  for(i in 1:nrow(data_w)) {
    subject_data<-data_w[i,]
    for(j in 1:length(subject_data)) {
      if((subject_data[j] < (col_means[j] - stdev*col_SD[j])) || (subject_data[j] > (col_means[j] + stdev*col_SD[j]))) {
        data_p[which(data_p[,subject_name]==(rownames(data_w)[i]) & data_p[,time_name]==as.numeric(colnames(subject_data)[j])),]$outlier_sp<-"Y"
      }
    }
  }
  
  return(data_p)
}

find_outliers <- function(whz_l,i,measure, lb, ub, id, time_name) {
  exclude <- {
  }
  flagged_measurment <- whz_l[i, measure]
  subject_data <- whz_l[which(whz_l[,id] == whz_l[[i, id]]), ]
  sorted_data <- arrange(subject_data, .data[[time_name]])
  index <- which(sorted_data[,measure] == flagged_measurment)
  confirmed <- FALSE
  allNAs <- TRUE
  if (index > 1) {
    for (j in (index - 1):1) {
      if (!is.na(sorted_data[j, measure])) {
        #if (abs(sorted_data[j, ]$time_point - sorted_data[index, ]$time_point) < 24) {
        UBzbmi <- abs(flagged_measurment - sorted_data[j, measure])
        if (UBzbmi > 2) {
          exclude <- rbind(exclude, whz_l[i, ])
          #print("z-score is > ± 2 standard deviation")
          #print(whz_l[i, ]$Subject)
          confirmed <- TRUE
        }
        allNAs <- FALSE
        break
        #}
      }
    }
  }
  if (index < nrow(sorted_data) & !confirmed) {
    for (j in (index + 1):nrow(sorted_data)) {
      if (!is.na(sorted_data[j, measure])) {
        #if (abs(sorted_data[j, ]$time_point - sorted_data[index, ]$time_point) < 24) {
        LBzbmi <- abs(flagged_measurment - sorted_data[j, measure])
        if (LBzbmi > 2) {
          exclude <- rbind(exclude, whz_l[i, ])
          #print("z-score is >= ± 2 standard deviation")
          #print(whz_l[i, ]$Subject)
          #df[i,]$PARENTWEIGHT = NA
          confirmed <- TRUE
        }
        allNAs <- FALSE
        break
        #}
      }
    }
  }
  if (!confirmed & allNAs) {
    exclude <- rbind(exclude, whz_l[i, ])
    #print("Unique flagged measurement")
    #print(whz_l[i, ]$Subject)
    #df[i,]$PARENTWEIGHT = NA
    confirmed <- TRUE
  }
  return(exclude)
}

find_BIV<-function(whz_l, measure, lb, ub, id, time_name, index_name){
  exclude_wflz <- {
  }
  for (i in 1:nrow(whz_l)) {
    if (!is.na(whz_l[i, measure]) & !is.nan(whz_l[i, measure])) {
      if (whz_l[i, measure] < lb | whz_l[i, measure]  > ub) {
        exclude_wflz <- rbind(exclude_wflz, find_outliers(whz_l, i, measure, lb, ub, id, time_name))
      }
    }
  }
  
  data_e<-mutate(whz_l, outlier_e=rep("N", nrow(whz_l)))
  
  for(i in seq_along(exclude_wflz[,index_name])) {
    data_e[which(data_e[,index_name]==(exclude_wflz[i,index_name])),]$outlier_e<-"Y"
  }
  return(data_e)
}
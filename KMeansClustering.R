# K Means Clustering

# train K Means on a data with specific columns
# returns data frame
trainKMeansOnData = function(a.data, a.result.column, a.num.iter, a.first.index, a.second.index, a.formula) {
  
  # store results
  acc.vec <- vector()
  
  # train for num.iter times
  for(i in 1:a.num.iter) {
    
    # train model
    model <- kmeans(a.data, 3)
    
    # create confusion matrix
    conf.matrix <- table(model$cluster, a.result.column)
    
    # compute all combinations for the 3 clusters
    s1 <- conf.matrix[1,1] + conf.matrix[2,2] + conf.matrix[3,3]
    s2 <- conf.matrix[1,1] + conf.matrix[3,2] + conf.matrix[2,3]
    s3 <- conf.matrix[2,1] + conf.matrix[1,2] + conf.matrix[3,3]
    s4 <- conf.matrix[2,1] + conf.matrix[3,2] + conf.matrix[1,3]
    s5 <- conf.matrix[3,1] + conf.matrix[1,2] + conf.matrix[2,3]
    s6 <- conf.matrix[3,1] + conf.matrix[2,2] + conf.matrix[1,3]
    
    # take the best combination
    acc.vec[i] <- max(c(s1,s2,s3,s4,s5,s6))
  }
  
  result.df <- data.frame(matrix(ncol = 3, nrow = 1))
  result.df[1,1] <- "KMC"
  result.df[1,2] <- datasetsNames(a.first.index, a.second.index)
  result.df[1,3] <- round(mean(acc.vec)/nrow(a.data) * 100, 2)
  result.df[1,4] <- a.formula
  colnames(result.df) <- c("Algorithm",
                           "Dataset", 
                           "Average.acc", 
                           "Formula")
  
  # return mean 
  return(result.df)
}


# call train method with all combinations of features for a data object
# returns data frame
trainAllKMeansFeaturesOnData = function(a.data, a.feature.list, a.num.iter, a.first.index, a.second.index) {
  
  # create result dataframe
  result.df <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(result.df) <- c("Algorithm",
                           "Dataset", 
                           "Average.acc", 
                           "Formula")
  
  # set result.column
  result.column <- a.data$C1Stress
  
  # for each feature combination
  for(i in 1:length(a.feature.list)) {
    
    # create data with custom feature combination
    feature.data <- as.data.frame(a.data[,a.feature.list[[i]]])
    
    # create formula as character
    form <- paste("C1Stress ~", paste(column.names[a.feature.list[[i]]][!column.names[a.feature.list[[i]]] %in% "PSS_Score"], collapse = " + "))
    
    # run training on the new dataset and put it in result dataframe
    result.df <- rbind(result.df, trainKMeansOnData(feature.data, result.column, a.num.iter, a.first.index, a.second.index, form))
  }
  
  
  result.df <- result.df[order(result.df$Average.acc, decreasing = TRUE), ]
  
  return(result.df[1,])
  
}


MLKmeansClustering = function(a.datasets.list, a.feature.list, a.num.iter) {

  # start timing
  start.time <- proc.time()
  
  curr.num.data <- 0
  cat("K Means Clustering: ", round((curr.num.data/num.datasets)*100, 2), "%\n")
  
  # create data frame for kmeans results
  KMeans.Clustering.df <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(KMeans.Clustering.df) <- c("Algorithm",
                                      "Dataset", 
                                      "Average.acc", 
                                      "Formula")
  
  # start training on all datasets
  for(i in 1:length(a.datasets.list)) {
    
    # take list of dataset list
    dataset.list <- a.datasets.list[[i]]
    
    partial.result.df <- data.frame()
    
    # each dataset
    for(j in 1:length(dataset.list)) {
      
      partial.result.df <- data.frame(matrix(ncol = 3, nrow = 0))
      
      # take dataset
      dataset <- dataset.list[[j]]
      
      # take result
      result <- trainAllKMeansFeaturesOnData(dataset, a.feature.list, a.num.iter, i, j)
      
      # print progress
      curr.num.data <- curr.num.data + 1
      cat("K Means Clustering: Dataset", datasetsNames(i, j), "list =", i, "  dataset =", j, " -> ", round((curr.num.data/num.datasets)*100, 2),
          "%  time: ", (proc.time() - start.time)[[3]]%/%60, "(m) ", round((proc.time() - start.time)[[3]]%%60, 3), "(s)\n")
      
      # save partial results
      partial.result.df <- rbind(partial.result.df, result)
    }
    
    partial.result.df <- partial.result.df[order(partial.result.df$Average.acc, decreasing = TRUE), ]
    
    # save results
    KMeans.Clustering.df <- rbind(KMeans.Clustering.df, partial.result.df[1,])
  }
  
  # output a csv file
  write.csv(KMeans.Clustering.df, file = "KMeansClusteringResults.csv")
}

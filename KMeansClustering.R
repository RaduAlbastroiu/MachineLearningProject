# K Means Clustering

# train K Means on a data with specific columns
# returns data frame
trainKMeansOnData = function(data, result.column, num.iter, first.index, second.index, form) {
  
  # store results
  acc.vec <- vector()
  
  # train for num.iter times
  for(i in 1:num.iter) {
    
    # train model
    model <- kmeans(data, 3)
    
    # create confusion matrix
    conf.matrix <- table(model$cluster, result.column)
    
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
  result.df[1,1] <- datasetsNames(first.index, second.index)
  result.df[1,2] <- round((mean(acc.vec)/nrow(data))*100, 2)
  result.df[1,3] <- form
  colnames(result.df) <- c("Dataset", "AverageAccuracy", "Formula")
  
  # return mean 
  return(result.df)
}



# call train method with all combinations of features for a data object
# returns data frame
trainAllKMeansFeaturesOnData = function(data, num.iter, first.index, second.index) {
  
  # create result dataframe
  result.df <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(result.df) <- c("Dataset", "Accuracy", "Formula")
  
  # set result.column
  result.column <- data$C1Stress
  
  # for each feature combination
  for(i in 1:length(feature.selection.list)) {
    
    # create data with custom feature combination
    feature.data <- as.data.frame(data[,feature.selection.list[[i]]])
    
    # create formula as character
    form <- as.character(as.formula(paste("PSS_Score ~", paste(column.names[feature.selection.list[[i]]][!column.names[feature.selection.list[[i]]] %in% "PSS_Score"], collapse = " + "))))
    
    # run training on the new dataset and put it in result dataframe
    result.df <- rbind(result.df, trainKMeansOnData(feature.data, result.column, num.iter, first.index, second.index, form))
  }
  
  
  result.df <- result.df[order(result.df$AverageAccuracy, decreasing = TRUE), ]
  
  return(result.df[1,])
  
}



# num iterations
num.iter <- 100
prog <- 0
cat("K Means Clustering: ", round((prog/num.datasets)*100, 2), "%\n")

# create data frame for kmeans results
KMeans.Clustering.df <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(KMeans.Clustering.df) <- c("Dataset", "AverageAccuracy", "Formula")

# run for simple data
KMeans.Clustering.df <- rbind(KMeans.Clustering.df, trainAllKMeansFeaturesOnData(simple.data, num.iter, 1, 1))

# Progress
prog <- prog + 1
cat("K Means Clustering: Dataset list =", 1, "  dataset =", 1, " -> ", round((prog/num.datasets)*100, 2), "%\n")

# start training on all datasets
for(i in 2:length(datasets.list)) {
  
  # take list of dataset list
  dataset.list <- datasets.list[[i]]
  
  partial.result.df <- data.frame()
  
  # each dataset
  for(j in 1:length(dataset.list)) {
    
    partial.result.df <- data.frame(matrix(ncol = 3, nrow = 0))
    
    # take dataset
    dataset <- dataset.list[[j]]
    
    # take result
    result <- trainAllKMeansFeaturesOnData(dataset, num.iter, i, j)
    
    # print progress
    prog <- prog + 1
    cat("K Means Clustering: Dataset list =", 1, "  dataset =", 1, " -> ", round((prog/num.datasets)*100, 2), "%\n")
    
    # save partial results
    partial.result.df <- rbind(partial.result.df, result)
  }
  
  partial.result.df <- partial.result.df[order(partial.result.df$AverageAccuracy, decreasing = TRUE), ]
  
  # save results
  KMeans.Clustering.df <- rbind(KMeans.Clustering.df, partial.result.df[1,])
}

# output a csv file
write.csv(KMeans.Clustering.df, file = "KMeansClustering.csv")



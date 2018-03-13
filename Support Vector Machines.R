# Support Vector Machines

# tune and run svm
# returns accuracy
svmTuneRun = function(train, trainResult, test, testResult) {
  
  # train model
  best.model <- tune(svm, train.x = train, train.y = trainResult, kernel = 'radial', ranges = list(cost = c(0.01,0.1,1,10,100), gamma = c(0.25, 0.5, 1, 2, 5, 10, 100)))
  best.model <- best.model$best.model
  
  # predict
  pred <- predict(best.model, test)
  
  # compute accuracy
  acc <- mean(pred == testResult)
  
  # return accuracy
  return(acc)
}

# run svm on all features
# returns a dataframe with:
# data name
# acc for simple split
# acc for k fold
svmFeatureRun = function(data, k, first.index, second.index) {
  
  best.simple.split <- -10
  best.kfolds.split <- -10
  best.formula <- 'a'
  
  for(i in 1:length(feature.selection.list)) {
    cat("Linear Regression: Feature combination number =", i, "\n")
    # split data on features
    partial.data <- as.data.frame(data[,feature.selection.list[[i]]])
    partial.data$Result <- data$C1Stress
    
    # Simple Split data
    partial.results <- vector()
    for(j in 1:k) {
      # split data
      split.data <- dataSplit(partial.data, partial.data$Result, 0.7)
      
      train.data <- split.data[[1]][1:length(feature.selection.list[[i]])]
      train.data.result <- split.data[[1]]$Result
      test.data <- split.data[[2]][1:length(feature.selection.list[[i]])]
      test.data.result <- split.data[[2]]$Result
      
      partial.results[j] <- svmTuneRun(train.data, train.data.result, test.data, test.data.result)
    }
    simple.split.result <- mean(partial.results)
    
    # K Folds data split
    partial.results <- vector()
    data.split.list <- kFoldSplit(partial.data, k)
    for(j in 1:k) {
      
      # select k fold
      split.data <- data.split.list[[j]]
      
      train.data <- split.data[[1]][1:length(feature.selection.list[[i]])]
      train.data.result <- split.data[[1]]$Result
      test.data <- split.data[[2]][1:length(feature.selection.list[[i]])]
      test.data.result <- split.data[[2]]$Result
      
      partial.results[j] <- svmTuneRun(train.data, train.data.result, test.data, test.data.result)
    }
    kfold.split.result <- mean(partial.results)
    
    # refresh results
    avg <- 0
    if(is.na(kfold.split.result) == FALSE && is.na(simple.split.result) == FALSE) {
      # compute avg
      avg <- (kfold.split.result + simple.split.result)/2
      old.avg <- (best.kfolds.split + best.simple.split)/2
      
      # compute best avg
      if(avg > old.avg) {
        best.formula <- paste("PSS_Score ~", paste(column.names[feature.selection.list[[i]]][!column.names[feature.selection.list[[i]]] %in% "PSS_Score"], collapse = " + "))
        best.kfolds.split <- kfold.split.result
        best.simple.split <- simple.split.result
      }
    }
  }
  
  result.df <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(result.df) <- c("Dataset", "Avg.pred.data.split", "Avg.pred.kfolds", "Formula")
  result.df[1,1] = datasetsNames(first.index, second.index)
  result.df[1,2] = best.simple.split
  result.df[1,3] = best.kfolds.split
  result.df[1,4] = best.formula

  return(result.df)
}



# set value for kfolds
k <- 5

# create results df
SVM.df.results <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(SVM.df.results) <- c("Dataset", "Avg.pred.data.split", "Avg.pred.kfolds", "Formula")

# run simple.data
cat("Linear Regression Dataset: Simple data\n")
SVM.df.results <- rbind(SVM.df.results, svmFeatureRun(simple.data, k, 1, 1))



# run for oversampled data
partial.results.df <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(partial.results.df) <- c("Dataset", "Avg.pred.data.split", "Avg.pred.kfolds", "Formula")
for(i in 1:length(oversampled.datasets)) {
  cat("Linear Regression Dataset: Oversampled data num.", i,"\n")
  # take dataset
  dataset <- oversampled.datasets[[i]]
  
  # run train
  partial.results.df <- rbind(partial.results.df, svmFeatureRun(dataset, k, 2, 1))
}
partial.results.df[order(partial.results.df$Avg.pred.data.split, decreasing = TRUE), ]
partial.results.df$Avg.pred.data.split <- mean(partial.results.df$Avg.pred.data.split)
partial.results.df$Avg.pred.kfolds <- mean(partial.results.df$Avg.pred.kfolds)

# bind to result
SVM.df.results <- rbind(SVM.df.results, partial.results.df[1,])




# run for undersampled data
partial.results.df <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(partial.results.df) <- c("Dataset", "Avg.pred.data.split", "Avg.pred.kfolds", "Formula")
for(i in 1:length(undersampled.datasets)) {
  cat("Linear Regression Dataset: Undersamples data num.", i,"\n")
  # take dataset
  dataset <- undersampled.datasets[[i]]
  
  # run train
  partial.results.df <- rbind(partial.results.df, svmFeatureRun(dataset, k, 3, 1))
}
partial.results.df[order(partial.results.df$Avg.pred.data.split, decreasing = TRUE), ]
partial.results.df$Avg.pred.data.split <- mean(partial.results.df$Avg.pred.data.split)
partial.results.df$Avg.pred.kfolds <- mean(partial.results.df$Avg.pred.kfolds)

# bind to result
SVM.df.results <- rbind(SVM.df.results, partial.results.df[1,])




# run for hybrid data
partial.results.df <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(partial.results.df) <- c("Dataset", "Avg.pred.data.split", "Avg.pred.kfolds", "Formula")
for(i in 1:length(hybrid.datasets)) {
  cat("Linear Regression Dataset: Hybrid data num.", i,"\n")
  # take dataset
  dataset <- hybrid.datasets[[i]]
  
  # run train
  partial.results.df <- rbind(partial.results.df, svmFeatureRun(dataset, k, 4, 1))
}
partial.results.df[order(partial.results.df$Avg.pred.data.split, decreasing = TRUE), ]
partial.results.df$Avg.pred.data.split <- mean(partial.results.df$Avg.pred.data.split)
partial.results.df$Avg.pred.kfolds <- mean(partial.results.df$Avg.pred.kfolds)

# bind to result
SVM.df.results <- rbind(SVM.df.results, partial.results.df[1,])


# write a csv
write.csv(SVM.df.results, file = "SupportVectorMachinesResult.csv")





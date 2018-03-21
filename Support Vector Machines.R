# Support Vector Machines

# load necessary libs
library(e1071)

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
    if(is.na(kfold.split.result) == FALSE & is.na(simple.split.result) == FALSE) {
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

# run svm for a dataset list and a feature selection list
# write a csv file with results of svm ml
MLSVM = function(datasets.list, feature.selection.list, k) { 

  curr.num.data <- 1
  
  # create results df
  SVM.df.results <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(SVM.df.results) <- c("Dataset", "Avg.pred.data.split", "Avg.pred.kfolds", "Formula")
  
  # run simple.data
  cat("SVM working on Dataset: Simple data ->", round((curr.num.data/num.datasets)*100, 2), "%\n")
  SVM.df.results <- rbind(SVM.df.results, svmFeatureRun(simple.data, k, 1, 1))
  
  
  # run for all datasets
  for(i in 2:length(datasets.list)) {
    
    # current dataset  
    dataset.list <- datasets.list[[i]]
    
    # run for data
    partial.results.df <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(partial.results.df) <- c("Dataset", "Avg.pred.data.split", "Avg.pred.kfolds", "Formula")
    for(j in 1:length(dataset.list)) {
      
      curr.num.data <- curr.num.data + 1
      cat("SVM working on Dataset:", datasetsNames(i,j), " nr of dataset:", j, "->", round((curr.num.data/num.datasets)*100, 2), "%\n")

      # take dataset
      dataset <- dataset.list[[j]]
      
      # run train
      a <-  svmFeatureRun(dataset, k, i, j)
      partial.results.df <- rbind(partial.results.df, a)
    }
    
    partial.results.df[order(partial.results.df$Avg.pred.data.split, decreasing = TRUE), ]
    partial.results.df$Avg.pred.data.split <- mean(partial.results.df$Avg.pred.data.split)
    partial.results.df$Avg.pred.kfolds <- mean(partial.results.df$Avg.pred.kfolds)
    
    # bind to result
    SVM.df.results <- rbind(SVM.df.results, partial.results.df[1,])
  }
  
  
  # write a csv
  write.csv(SVM.df.results, file = "SupportVectorMachinesResult.csv")

}



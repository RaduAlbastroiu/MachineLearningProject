# Support Vector Machines

# load necessary libs
library(e1071)

# tune and run svm
# returns accuracy
svmTuneRun = function(a.train, a.trainResult, a.test, a.testResult) {
  
  # train model
  best.model <- tune(svm, train.x = a.train, train.y = a.trainResult, kernel = 'radial', ranges = list(cost = c(0.01,0.1,1,10,100), gamma = c(0.25, 0.5, 1, 2, 5, 10, 100)))
  best.model <- best.model$best.model
  
  # predict
  pred <- predict(best.model, a.test)
  
  # compute accuracy
  acc <- mean(pred == a.testResult)
  
  # return accuracy
  return(acc)
}

# run svm on all features
# returns a dataframe with:
# data name
# acc for simple split
# acc for k fold
svmFeatureRun = function(a.data, a.feature.list, a.k, a.first.index, a.second.index, a.start.time.SVM) {
  
  best.simple.split <- -10
  best.kfolds.split <- -10
  best.formula <- 'a'
  
  for(i in 1:length(a.feature.list)) {
    
    # split data on features
    partial.data <- as.data.frame(a.data[,a.feature.list[[i]]])
    partial.data$Result <- a.data$C1Stress
    
    # Simple Split data
    partial.results <- vector()
    for(j in 1:a.k) {
      # split data
      split.data <- dataSplit(partial.data, partial.data$Result, 0.7)
      
      train.data <- split.data[[1]][1:length(a.feature.list[[i]])]
      train.data.result <- split.data[[1]]$Result
      test.data <- split.data[[2]][1:length(a.feature.list[[i]])]
      test.data.result <- split.data[[2]]$Result
      
      partial.results[j] <- svmTuneRun(train.data, train.data.result, test.data, test.data.result)
    }
    simple.split.result <- mean(partial.results)
    
    # K Folds data split
    partial.results <- vector()
    data.split.list <- kFoldSplit(partial.data, a.k)
    for(j in 1:a.k) {
      
      # select k fold
      split.data <- data.split.list[[j]]
      
      train.data <- split.data[[1]][1:length(a.feature.list[[i]])]
      train.data.result <- split.data[[1]]$Result
      test.data <- split.data[[2]][1:length(a.feature.list[[i]])]
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
        best.formula <- paste("C1Stress ~", paste(column.names[a.feature.list[[i]]][!column.names[a.feature.list[[i]]] %in% "PSS_Score"], collapse = " + "))
        best.kfolds.split <- kfold.split.result
        best.simple.split <- simple.split.result
      }
    }
    
    
    # progress with time
    cat(" - current feature comb num: ",i, " -> ", round((i/length(a.feature.list))*100, 2),  
        "%  time: ", (proc.time() - a.start.time.SVM)[[3]]%/%60, "(m) ", round((proc.time() - a.start.time.SVM)[[3]]%%60, 3), "(s)\n")
    
  }
  
  result.df <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(result.df) <- c("Algorithm",
                           "Dataset", 
                           "Avg.pred.data.split", 
                           "Avg.pred.kfolds", 
                           "Average.acc",
                           "Formula")
  
  result.df[1,1] <- "Support Vector Machines"
  result.df[1,2] <- datasetsNames(a.first.index, a.second.index)
  result.df[1,3] <- best.simple.split
  result.df[1,4] <- best.kfolds.split
  result.df[1,5] <- (best.simple.split + best.kfolds.split) / 2
  result.df[1,6] <- best.formula

  return(result.df)
}

# run svm for a dataset list and a feature selection list
# write a csv file with results of svm ml
MLSVM = function(a.datasets.list, a.feature.list, a.k) { 
  
  # start timing
  start.time.SVM <- proc.time()

  curr.num.data <- 0
  
  # create results df
  SVM.df.results <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(SVM.df.results) <- c("Algorithm",
                                "Dataset", 
                                "Avg.pred.data.split", 
                                "Avg.pred.kfolds", 
                                "Average.acc",
                                "Formula")
  
  # run for all datasets
  for(i in 1:length(a.datasets.list)) {
    
    # current dataset  
    dataset.list <- a.datasets.list[[i]]
    
    # run for data
    partial.results.df <- data.frame(matrix(ncol = 6, nrow = 0))
    colnames(partial.results.df) <- c("Algorithm",
                                      "Dataset", 
                                      "Avg.pred.data.split", 
                                      "Avg.pred.kfolds", 
                                      "Average.acc",
                                      "Formula")
    for(j in 1:length(dataset.list)) {
      
      curr.num.data <- curr.num.data + 1
      cat("SVM working on Dataset:", datasetsNames(i, j), ", nr of dataset:", curr.num.data, "->", round((curr.num.data/num.datasets)*100, 2),
          "%  time: ", (proc.time() - start.time.SVM)[[3]]%/%60, "(m) ", round((proc.time() - start.time.SVM)[[3]]%%60, 3), "(s)\n")
      
      # take dataset
      dataset <- dataset.list[[j]]
      
      # run train
      a <-  svmFeatureRun(dataset, a.feature.list, a.k, i, j, start.time.SVM)
      partial.results.df <- rbind(partial.results.df, a)
    }
    
    partial.results.df[order(partial.results.df$Avg.pred.data.split, decreasing = TRUE), ]
    partial.results.df$Avg.pred.data.split <- round(mean(partial.results.df$Avg.pred.data.split) * 100, 2)
    partial.results.df$Avg.pred.kfolds <- round(mean(partial.results.df$Avg.pred.kfolds) * 100, 2)
    partial.results.df$Average.acc <- round((partial.results.df$Avg.pred.data.split + partial.results.df$Avg.pred.kfolds)/2, 2)
    
    # bind to result
    SVM.df.results <- rbind(SVM.df.results, partial.results.df[1,])
  }
  
  # write a csv
  write.csv(SVM.df.results, file = "SupportVectorMachinesResults.csv")
  
  # remove unnecesary data
  rm(start.time.SVM)
}



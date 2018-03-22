# Decision Tree 

# load necessary libs
library(e1071)
library(dplyr)


# Trains a model using simple split and returns its accuracy/MCE
# returns a vec where first is accuracy and second is MCE
naiveBayesSimpleSplit = function(a.data, a.formula, a.split.ratio) {
  
  a.data <- a.data[, !colnames(a.data) %in% c("PSS_Score", "C2Stress")]
  
  # split data
  temp.list <- dataSplit(a.data, a.data$C1Stress, a.split.ratio)
  train.data <- temp.list[[1]]
  test.data <- temp.list[[2]]
  
  # build tree
  model <- naiveBayes(a.formula, data = train.data)
  
  # predict
  predictions <- predict(model, test.data, type = "class")

  # compute accuracy
  missclassification.error <- mean(predictions != test.data$C1Stress)
  accuracy = 1 - missclassification.error
  
  # return accuracy and misclassification error
  result <- vector()
  result[1] <- accuracy
  result[2] <- missclassification.error
  
  return(result)
}


# Trains a model using k folds split and returns its accuracy/MCE
# returns a vec where first is accuracy and second is MCE
naiveBayesKFoldsSplit = function(a.data, a.formula, a.k) {
  
  a.data <- a.data[, !colnames(a.data) %in% c("PSS_Score", "C2Stress")]
  
  # split data
  folds <- kFoldSplit(a.data, a.k)
  
  # result vectors
  ACC.vec <- vector()
  MCE.vec <- vector()
  
  for(i in 1:a.k) {
    
    oneFold <- folds[[i]]
    
    train.data <- oneFold[[1]]
    test.data <- oneFold[[2]]
    
    # build tree
    model <- naiveBayes(a.formula, data = train.data)
    
    # predict
    predictions <- predict(model, test.data, type = "class")
    
    # compute accuracy
    missclassification.error <- mean(predictions != test.data$C1Stress)
    accuracy = 1 - missclassification.error
    
    # add in results vectors
    ACC.vec[length(ACC.vec) + 1] <- accuracy
    MCE.vec[length(MCE.vec) + 1] <- missclassification.error
  }
  
  ACC.vec <- ACC.vec[!is.na(ACC.vec)]
  MCE.vec <- MCE.vec[!is.na(MCE.vec)]
  
  # return the average accuracy and misclassification error for all folds
  result <- vector()
  result[1] <- mean(ACC.vec)
  result[2] <- mean(MCE.vec)
  
  return(result)
}


# For every dataset
# returns in a data frame the following items:
# - dataset name
# - avg accuracy on simple data split
# - avg accuracy on k folds
# - avg missclassification error for simple data split
# - avg missclassification error for k folds split
# - best formula as character
naiveBayesClassifier = function(a.data, a.feature.list, a.num.iter, a.k, a.first.index, a.second.index) {
  
  # store all results
  prediction.ACC.simple.split <- vector()
  prediction.MCE.simple.split <- vector()
  prediction.ACC.kfolds.split <- vector()
  prediction.MCE.kfolds.split <- vector()
  
  best.prediction <- -100000
  best.formula <- vector()
  
  for(i in 1:length(a.feature.list)) {
    
    cat("Feature no.", i, "\n")
    # formula for this combination of features
    f <- as.formula(paste("C1Stress ~", paste(column.names[a.feature.list[[i]]][!column.names[a.feature.list[[i]]] %in% "C1Stress"], collapse = " + ")))
    
    # for each run
    for(j in 1:a.num.iter) {
      
      ACC.vec <- vector()
      MCE.vec <- vector()
      Kfolds.split <- vector()
      
      # run simple split as many times as k
      for(z in 1:a.k) {
        res <- naiveBayesSimpleSplit(a.data, f, 0.7)
        
        ACC.vec[length(ACC.vec) + 1] <- res[1]
        MCE.vec[length(MCE.vec) + 1] <- res[2]
      }
      
      ACC.vec <- ACC.vec[!is.na(ACC.vec)]
      MCE.vec <- MCE.vec[!is.na(MCE.vec)]
      
      # simple split results
      prediction.ACC.simple.split[j] <- mean(ACC.vec)
      prediction.MCE.simple.split[j] <- mean(MCE.vec)
      
      # run k folds
      Kfolds.split <- naiveBayesKFoldsSplit(a.data, f, a.k)
      prediction.ACC.kfolds.split[j] <- Kfolds.split[1]
      prediction.MCE.kfolds.split[j] <- Kfolds.split[2]
      
      meanPred <- (prediction.ACC.simple.split[j] + prediction.ACC.kfolds.split[j]) / 2
      if(is.na(meanPred) == FALSE & is.na(best.prediction) == FALSE) {
        if(meanPred > best.prediction) {
          best.prediction <- meanPred
          best.formula <- f
        }
      }
    }
  }
  
  result.df <- data.frame(datasetsNames(a.first.index, a.second.index), mean(prediction.ACC.simple.split), mean(prediction.ACC.kfolds.split), mean(prediction.MCE.simple.split), mean(prediction.MCE.kfolds.split), as.character(Reduce(paste, deparse(best.formula))))
  colnames(result.df) <- c("Dataset", 
                           "Avg.acc.data.split", 
                           "Avg.acc.kfolds", 
                           "Avg.mce.data.split", 
                           "Avg.mce.kfolds", 
                           "Formula")
  result.df$Formula <- as.character(result.df$Formula)
  
  return(result.df)
}


# method which runs the Decision Tree algorithm on all datasets 
MLNaiveBayes = function(a.datasets.list, a.feature.list, a.num.iter, a.k) {
  
  curr.num.data <- 0
  
  # create decision tree data frame
  Naive.Bayes.df <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(Naive.Bayes.df) <- c("Dataset", 
                                  "Avg.acc.data.split", 
                                  "Avg.acc.kfolds", 
                                  "Avg.mce.data.split", 
                                  "Avg.mce.kfolds", 
                                  "Formula")
  
  # for in list of datasets
  for(i in 1:length(a.datasets.list)) {

    if(i == 3)
      next
    
    datasets <- a.datasets.list[[i]]

    # result
    result.df <- data.frame(matrix(ncol = 6, nrow = 0))
    colnames(result.df) <- c("Dataset",
                             "Avg.acc.data.split",
                             "Avg.acc.kfolds",
                             "Avg.mce.data.split",
                             "Avg.mce.kfolds",
                             "Formula")

    # for each dataset train and keep results
    for(j in 1:length(datasets)) {

      # progressometer
      curr.num.data <- curr.num.data + 1
      cat("Naive Bayes: Dataset list =", i, "  dataset =", j, " -> ", round((curr.num.data/num.datasets)*100, 2), "%\n")

      # train on dataset
      dataset <- datasets[[j]]
      result.df <- rbind(result.df, naiveBayesClassifier(dataset, a.feature.list, a.num.iter, a.k, i, j))
    }

    # compute average on column
    result.df$Avg.acc.data.split <- mean(result.df$Avg.acc.data.split)
    result.df$Avg.acc.kfolds <- mean(result.df$Avg.acc.kfolds)
    result.df$Avg.mce.data.split <- mean(result.df$Avg.mce.data.split)
    result.df$Avg.mce.kfolds <- mean(result.df$Avg.mce.kfolds)
    result.df$Formula <- rle(sort(result.df$Formula, decreasing = TRUE))[[2]][[1]]

    # add to final results
    Naive.Bayes.df <- rbind(Naive.Bayes.df, result.df[1,])
  }
  
  # output a csv file
  write.csv(Naive.Bayes.df, file = "NaiveBayesResults.csv")
}

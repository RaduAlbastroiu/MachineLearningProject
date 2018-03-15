# Logistic Regression

# load necessary libs
library(caret)
library(VGAM)
library(dplyr)

# Trains a model using simple data split and returns its accuracy
# returns a vec where first is accuracy and second is MCE
logRegressionSimpleSplit = function(data, form, split.ratio) {
  
  #remove the other class
  data <- select(data, -C2Stress, -PSS_Score)
  
  # split data
  temp.list <- dataSplit(data, data$C1Stress, split.ratio)
  train.data <- temp.list[[1]]
  test.data <- temp.list[[2]]
  
  # train a model
  model <- vglm(form, family = propodds, data = train.data)
  
  # predict 
  predictions <- predict(model, type = 'response', test.data)
 
  if(any(is.na(predictions)) == TRUE)
    print(TRUE)
  
  fitted.result <- vector()
  for(i in 1:nrow(predictions)) {
    if(predictions[i,1] >= predictions[i,2] & predictions[i,1] >= predictions[i,3])
      fitted.result[i] <- colnames(predictions)[1]
    else if(predictions[i,2] >= predictions[i,1] & predictions[i,2] >= predictions[i,3])
      fitted.result[i] <- colnames(predictions)[2]
    else if(predictions[i,3] >= predictions[i,1] & predictions[i,3] >= predictions[i,2])
      fitted.result[i] <- colnames(predictions)[3]
  }
  
  fitted.result <- as.factor(fitted.result)
  levels(fitted.result) <- c('lowStress', 'moderateStress', 'highStress')

  # compute error and accuracy
  missclassification.error <- mean(fitted.result != test.data$C1Stress)
  accuracy = 1 - missclassification.error
  
  # creating the result
  result <- vector()
  result[1] <- accuracy
  result[2] <- missclassification.error
  print(result)
  return(result)
}

# Trains a model using k folds split and returns its accuracy
# returns a vec where first is accuracy and second is MCE
logRegressionKFoldsSplit = function(data, form, k) {
  
  #remove the other class
  data <- select(data, -C2Stress, -PSS_Score)
  
  # split data
  folds <- kFoldSplit(data, k)
  
  # result vectors
  ACC.vec <- vector()
  MCE.vec <- vector()
  
  for(i in 1:k) {
    
    oneFold <- folds[[i]]
    
    train.data <- oneFold[[1]]
    test.data <- oneFold[[2]]
    
    # train a model
    model <- vglm(form, family = propodds, data = train.data)
    
    # predict 
    predictions <- predict(model, test.data,  type = 'response')
    
    if(any(is.na(predictions)) == TRUE)
      print(TRUE)
    
    fitted.result <- vector()
    for(i in 1:nrow(predictions)) {
      if(predictions[i,1] >= predictions[i,2] & predictions[i,1] >= predictions[i,3])
        fitted.result[i] <- colnames(predictions)[1]
      else if(predictions[i,2] >= predictions[i,1] & predictions[i,2] >= predictions[i,3])
        fitted.result[i] <- colnames(predictions)[2]
      else if(predictions[i,3] >= predictions[i,1] & predictions[i,3] >= predictions[i,2])
        fitted.result[i] <- colnames(predictions)[3]
    }
    
    fitted.result <- as.factor(fitted.result)
    levels(fitted.result) <- c('lowStress', 'moderateStress', 'highStress')
    
    # compute error and accuracy
    missclassification.error <- mean(fitted.result != test.data$C1Stress)
    accuracy = 1 - missclassification.error
    
    # add in results vectors
    ACC.vec[length(ACC.vec) + 1] <- accuracy
    MCE.vec[length(MCE.vec) + 1] <- missclassification.error
  }
  
  result <- vector()
  result[1] <- mean(ACC.vec)
  result[2] <- mean(MCE.vec)
  print(result)
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
logRegression = function(data, num.runs, k, first.index, second.index) {
  
  # store all results
  prediction.ACC.simple.split <- vector()
  prediction.MCE.simple.split <- vector()
  prediction.ACC.kfolds.split <- vector()
  prediction.MCE.kfolds.split <- vector()
  
  best.prediction <- -100000
  best.formula <- vector()
  
  for(i in 1:length(feature.selection.list)) {
    
    # formula for this combination of features
    f <- as.formula(paste("C1Stress ~", paste(column.names[feature.selection.list[[i]]][!column.names[feature.selection.list[[i]]] %in% "C1Stress"], collapse = " + ")))
    
    # for each run
    for(j in 1:num.runs) {
      
      ACC.vec <- vector()
      MCE.vec <- vector()
      Kfolds.split <- vector()
      
      # run simple split as many times as k
      for(z in 1:k) {
        res <- logRegressionSimpleSplit(data, f, 0.7)
        
        ACC.vec[length(ACC.vec) + 1] <- res[1]
        MCE.vec[length(MCE.vec) + 1] <- res[2]
      }
      
      ACC.vec <- ACC.vec[!is.na(ACC.vec)]
      MCE.vec <- MCE.vec[!is.na(MCE.vec)]
      
      # simple split results
      prediction.ACC.simple.split[j] <- mean(ACC.vec)
      prediction.MCE.simple.split[j] <- mean(MCE.vec)
      
      # run k folds
      Kfolds.split <- logRegressionKFoldsSplit(data, f, k)
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
  
  result.df <- data.frame(datasetsNames(first.index, second.index), mean(prediction.ACC.simple.split), mean(prediction.ACC.kfolds.split), mean(prediction.MCE.simple.split), mean(prediction.MCE.kfolds.split), as.character(Reduce(paste, deparse(best.formula))))
  colnames(result.df) <- c("Dataset", "Avg.acc.data.split", "Avg.acc.kfolds", "Avg.mce.data.split", "Avg.mce.kfolds", "Formula")
  result.df$Formula <- as.character(result.df$Formula)
  
  return(result.df)
}

#parameters
k <- 5
num.runs <- 3

curr.num.data <- 1

# create logistic regression data frame
Logistic.Regression.df <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(Logistic.Regression.df) <- c("Dataset", "Avg.acc.data.split", "Avg.acc.kfolds", "Avg.mce.data.split", "Avg.mce.kfolds", "Formula")


# simple data
curr.num.data <- 1
cat("Logistic Regression: Dataset list =", 1, "  dataset =", 1, " -> ", round((curr.num.data/num.datasets)*100, 2), "%\n")
prediction.simple.data <- logRegression(simple.data, num.runs, k, 1, 1)
Logistic.Regression.df <- rbind(Logistic.Regression.df, prediction.simple.data)
Logistic.Regression.df$Formula <- as.character(Logistic.Regression.df$Formula)

# for in list of datasets
for(i in 2:length(datasets.list)) {
  
  if(i == 3)
    break
  
  datasets <- datasets.list[[i]]
  
  # result
  result.df <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(result.df) <- c("Dataset", "Avg.acc.data.split", "Avg.acc.kfolds", "Avg.mce.data.split", "Avg.mce.kfolds", "Formula")
  
  # for each dataset train and keep results
  for(j in 1:length(datasets)) {
    
    # progressometer
    curr.num.data <- curr.num.data + 1
    cat("Logistic Regression: Dataset list =", i, "  dataset =", j, " -> ", round((curr.num.data/num.datasets)*100, 2), "%\n")
    
    # train on dataset
    dataset <- datasets[[j]]
    result.df <- rbind(result.df, logRegression(dataset, num.runs, k, i, j))
  }
  
  # compute average on column
  result.df$Avg.acc.data.split <- mean(result.df$Avg.acc.data.split)
  result.df$Avg.acc.kfolds <- mean(result.df$Avg.acc.kfolds)
  result.df$Avg.mce.data.split <- mean(result.df$Avg.mce.data.split)
  result.df$Avg.mce.kfolds <- mean(result.df$Avg.mce.kfolds)
  result.df$Formula <- rle(sort(result.df$Formula, decreasing = TRUE))[[2]][[1]]
  
  # add to final results
  Logistic.Regression.df <- rbind(Logistic.Regression.df, result.df[1,])
}

# output a csv file
write.csv(Logistic.Regression.df, file = "LogisticRegressionResults.csv")
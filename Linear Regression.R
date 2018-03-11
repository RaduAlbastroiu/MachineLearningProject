# Linear Regression

# Create formula for each feature selection
# returns a formula
getFormulaForIthFeatureSelection = function(i) {
  return(as.formula(paste("PSS_Score ~", paste(column.names[feature.selection.list[[i]]][!column.names[feature.selection.list[[i]]] %in% "PSS_Score"], collapse = " + "))))
}

# Trains a model using simple data split and returns it's accuracy
# returns a vec where first is R2 and second is MSE
trainModelAndGetAccSimpleSplit = function(data, form, split.ratio) {
  # split data
  temp.list <- dataSplit(data, data$PSS_Score, split.ratio)
  train.data <- temp.list[[1]]
  test.data <- temp.list[[2]]
  
  # train a model
  model <- lm(form, data = train.data)
  
  # predict 
  predictions <- predict(model, test.data)
  
  # create predicted vs actual dataframe
  results <- cbind(predictions, test.data$PSS_Score)
  colnames(results) <- c("predicted", "actual") 
  results <- as.data.frame(results)
  
  # remove 0's from predictions
  results$predicted <- ifelse(results$predicted < 0, 0, results$predicted)
  
  # mean square error
  mse <- mean((results$actual - results$predicted)^2)
  
  # computing accuracy
  sse <- sum((results$predicted - results$actual)^2)
  sst <- sum((mean(data$PSS_Score) - results$actual)^2)
  R2 <- 1 - sse/sst
  
  # creating the result
  result <- vector()
  result[1] <- R2
  result[2] <- mse
  return(result)
}

# Trains a model using k split and returns it's accuracy
# returns a vec where first is R2 and second is MSE
trainModelAndGetAccKfoldsSplit = function(data, form, k) {
  # split data
  folds <- kFoldSplit(data, k)
  
  # result vectors
  R2.vec <- vector()
  MSE.vec <- vector()
  
  for(i in 1:length(temp.lists)) {
    
    oneFold <- folds[[i]]
    
    train.data <- oneFold[[1]]
    test.data <- oneFold[[2]]
    
    # train a model
    model <- lm(form, data = train.data)
    
    # predict 
    predictions <- predict(model, test.data)
    
    # create predicted vs actual dataframe
    results <- cbind(predictions, test.data$PSS_Score)
    colnames(results) <- c("predicted", "actual") 
    results <- as.data.frame(results)
    
    # remove 0's from predictions
    results$predicted <- ifelse(results$predicted < 0, 0, results$predicted)
    
    # mean square error
    mse <- mean((results$actual - results$predicted)^2)
    
    # computing accuracy
    sse <- sum((results$predicted - results$actual)^2)
    sst <- sum((mean(data$PSS_Score) - results$actual)^2)
    R2 <- 1 - sse/sst
    
    # add in results vectors
    R2.vec[length(R2.vec) + 1] <- R2
    MSE.vec[length(MSE.vec) + 1] <- mse
  }
  
  result <- vector()
  result[1] <- mean(R2.vec)
  result[2] <- mean(MSE.vec)
  return(result)
}

# For every dataset
# returns in a list the following items:
# [[1]] -> avg prediction on simple data split
# [[2]] -> avg prediction on k folds
# [[3]] -> avg mse for simple data split
# [[4]] -> avg mse for k folds split
# [[5]] -> best formula
trainPredictOnFeatures = function(data, num.runs, k) {
  
  # create result list
  result.list <- list()
  
  # store all results
  prediction.R2.simple.split <- vector()
  prediction.MSE.simple.split <- vector()
  prediction.R2.kfolds.split <- vector()
  prediction.MSE.kfolds.split <- vector()
  
  best.prediction <- -100000
  best.formula <- vector()
  
  for(i in 1:length(feature.selection.list)) {
    
    # formula for this combination of features
    f <- getFormulaForIthFeatureSelection(i)
    
    # for each run
    for(j in 1:num.runs) {
      
      R2.vec <- vector()
      MSE.vec <- vector()
      Kfolds.split <- vector()
      
      # run simple split as many times as k
      for(z in 1:k) {
        res <- trainModelAndGetAccSimpleSplit(data, f, 0.7)
        R2.vec[z] <- res[1]
        MSE.vec[z] <- res[2]
      }
      
      # simple split results
      prediction.R2.simple.split[j] <- mean(R2.vec)
      prediction.MSE.simple.split[j] <- mean(MSE.vec)
      
      # run k folds
      Kfolds.split <- trainModelAndGetAccKfoldsSplit(data, f, k)
      prediction.R2.kfolds.split[j] <- Kfolds.split[1]
      prediction.MSE.kfolds.split[j] <- Kfolds.split[2]
      
      if(mean(c(prediction.R2.simple.split[j], prediction.R2.kfolds.split[j])) > best.prediction) {
        best.prediction <- mean(c(prediction.R2.simple.split[j], prediction.R2.kfolds.split[j]))
        best.formula <- f
      }
    }
  }
  
  result.list[[1]] <- mean(prediction.R2.simple.split)
  result.list[[2]] <- mean(prediction.R2.kfolds.split)
  result.list[[3]] <- mean(prediction.MSE.simple.split)
  result.list[[4]] <- mean(prediction.MSE.kfolds.split)
  result.list[[5]] <- best.formula
  
  return(result.list)
}

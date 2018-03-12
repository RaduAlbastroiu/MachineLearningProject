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
  
  for(i in 1:k) {
    
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
# returns in a data frame the following items:
# - dataset name
# - avg prediction on simple data split
# - avg prediction on k folds
# - avg mse for simple data split
# - avg mse for k folds split
# - best formula as character
trainPredictOnFeatures = function(data, num.runs, k, first.index, second.index) {
  
  # store all results
  prediction.R2.simple.split <- vector()
  prediction.MSE.simple.split <- vector()
  prediction.R2.kfolds.split <- vector()
  prediction.MSE.kfolds.split <- vector()
  
  best.prediction <- -100000
  best.formula <- vector()
  
  for(i in 1:length(feature.selection.list)) {
    
    # formula for this combination of features
    f <- as.formula(paste("PSS_Score ~", paste(column.names[feature.selection.list[[i]]][!column.names[feature.selection.list[[i]]] %in% "PSS_Score"], collapse = " + ")))
    
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
      
      meanPred <- (prediction.R2.simple.split[j] + prediction.R2.kfolds.split[j]) / 2
      if(is.na(meanPred) == FALSE & is.na(best.prediction) == FALSE) {
        if(meanPred > best.prediction) {
          best.prediction <- meanPred
          best.formula <- f
        }
      }
    }
  }
  
  result.df <- data.frame(datasetsNames(first.index, second.index), mean(prediction.R2.simple.split), mean(prediction.R2.kfolds.split), mean(prediction.MSE.simple.split), mean(prediction.MSE.kfolds.split), as.character(Reduce(paste, deparse(best.formula))))
  colnames(result.df) <- c("Dataset", "Avg.pred.data.split", "Avg.pred.kfolds", "Avg.mse.data.split", "Avg.mse.kfolds", "Formula")
  result.df$Formula <- as.character(result.df$Formula)
  
  return(result.df)
}

# parameters
k <- 5
num.runs <- 5

curr.num.data <- 1

# create linear regression data frame
Linear.Regression.df <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(Linear.Regression.df) <- c("Dataset", "Avg.pred.data.split", "Avg.pred.kfolds", "Avg.mse.data.split", "Avg.mse.kfolds", "Formula")


# Add first 3 elements by hand, don't ask
# simple data
curr.num.data <- 1
cat("Linear Regression: Dataset list =", 1, "  dataset =", 1, " -> ", round((curr.num.data/num.datasets)*100, 2), "%\n")
prediction.simple.data <- trainPredictOnFeatures(simple.data, num.runs, k, 1, 1)
Linear.Regression.df <- rbind(Linear.Regression.df, prediction.simple.data)
Linear.Regression.df$Formula <- as.character(Linear.Regression.df$Formula)

# normalized simple data
#curr.num.data <- 2
#cat("Linear Regression: Dataset list =", 1, "  dataset =", 2, " -> ", round((curr.num.data/num.datasets)*100, 2), "%\n")
#prediction.normalized.simple.data <- trainPredictOnFeatures(normalized.simple.data, num.runs, k, 1, 2)
#Linear.Regression.df <- rbind(Linear.Regression.df, prediction.normalized.simple.data)

# scaled simple data
#curr.num.data <- 3
#cat("Linear Regression: Dataset list =", 1, "  dataset =", 3, " -> ", round((curr.num.data/num.datasets)*100, 2), "%\n")
#prediction.scaled.simple.data <- trainPredictOnFeatures(scaled.simple.data, num.runs, k, 1, 3)
#Linear.Regression.df <- rbind(Linear.Regression.df, prediction.scaled.simple.data)


# for in list of datasets
for(i in 2:length(datasets.list)) {
  
  datasets <- datasets.list[[i]]
  
  # result 
  result.df <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(result.df) <- c("Dataset", "Avg.pred.data.split", "Avg.pred.kfolds", "Avg.mse.data.split", "Avg.mse.kfolds", "Formula")
  
  # for each dataset train and keep results
  for(j in 1:length(datasets)) {
    
    # progressometer
    curr.num.data <- curr.num.data + 1
    cat("Linear Regression: Dataset list =", i, "  dataset =", j, " -> ", round((curr.num.data/num.datasets)*100, 2), "%\n")
    
    # train on dataset
    dataset <- datasets[[j]]
    result.df <- rbind(result.df, trainPredictOnFeatures(dataset, num.runs, k, i, j))
  }
  
  # compute average on column
  result.df$Avg.pred.data.split <- mean(result.df$Avg.pred.data.split)
  result.df$Avg.pred.kfolds <- mean(result.df$Avg.pred.kfolds)
  result.df$Avg.mse.data.split <- mean(result.df$Avg.mse.data.split)
  result.df$Avg.mse.kfolds <- mean(result.df$Avg.mse.kfolds)
  result.df$Formula <- rle(sort(result.df$Formula, decreasing = TRUE))[[2]][[1]]
  
  # add to final results
  Linear.Regression.df <- rbind(Linear.Regression.df, result.df[1,])
}

# output a csv file
write.csv(Linear.Regression.df, file = "LinearRegressionResults.csv")





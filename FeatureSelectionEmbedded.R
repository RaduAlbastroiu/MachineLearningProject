# Embedded feature selection

# install.packages("randomForest")
# install.packages("caret")
library(randomForest)
library(caret)


# Feature Selection using Random Forest

# train random forest model for features
featureSelRandomForest = function(data, num.features) { 
  
  # build random forest model
  rand.forest.data <- randomForest(PSS_Score ~ ., data[,1:30])
  
  # plot variable importance
  # print(varImpPlot(rand.forest.data))
  
  # get the feature importance 
  feature.importance <- importance(rand.forest.data)
  
  # sort by importance
  sorted.feature.importance <- feature.importance[order(feature.importance[,1], decreasing = T),]
  
  # get indexes of features
  randForestColNum <- numeric()
  j <- 0
  for(colName in names(sorted.feature.importance)) {
    
    for(i in 3:30) {
      if(colName == column.names[i] & j > 0) {
        randForestColNum[j] <- i
        break()
      }
    }
    j <- j + 1
    if(j > num.features)
      break
  }
  
  randForestColNum <- subset(randForestColNum, is.na(randForestColNum) == FALSE)
  return(randForestColNum) 
}




# Feature Selection using Logistic Regression

# train random forest model for features
featureSelLogRegression = function(data, num.features) { 
  
  # Fit a logistic regression model
  logModel = glm(C1Stress~., data[3:31],family = "binomial")
  
  # get features sorted by logistic regression
  log.regression.features <- varImp(logModel)
  log.regression.features <- log.regression.features[order(log.regression.features[,1], decreasing = T), ,drop = FALSE]
  
  # add feature inndexes in a list
  logRegressionColNum <- numeric()
  j <- 0
  for(name in rownames(log.regression.features)) {
    
    for(i in 3:30) {
      if(name == column.names[i] & j > 0) {
        logRegressionColNum[j] <- i
        break()
      }
    }
    j <- j + 1
    if(j > num.features)
      break
  }
  
  logRegressionColNum <- subset(logRegressionColNum, is.na(logRegressionColNum) == FALSE)
  return(logRegressionColNum)
}




# run both methods of feature selection on all datasets
num.iterations <- 15
for(i in 1:length(datasets.list)) {
  
  # progress
  cat("Feature Selection Embedded run number", i, " -> ", round((i/num.datasets)*100, 2), "%\n")
  
  datasets <- datasets.list[[i]]
  
  # for each dataset 
  for(j in 1:length(datasets)) {

    dataset <- datasets[[j]]

    for (iter in 3:num.iterations) {
      
      # retrieve features sorted by random forest
      rand.forest.features <- featureSelRandomForest(dataset, iter)
      
      # retrieve features sorted by logistic regression
      log.regression.features <- featureSelLogRegression(dataset, iter)
      
      feature.selection.list[[length(feature.selection.list) + 1]] <- rand.forest.features
      feature.selection.list[[length(feature.selection.list) + 1]] <- log.regression.features
    }
  }
}



rm(i, j, num.iterations, iter)
rm(rand.forest.features, log.regression.features)
rm(featureSelRandomForest)
rm(featureSelLogRegression)

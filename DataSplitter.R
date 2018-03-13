# Data Splitter

# methods
# - Data Split + used
# - Bootstrap
# - k-fold Cross Validation + used
# - Repeated k-fold Cross Validation 
# - Leave One Out Cross Validation

library(caret)
library(klaR)

# Data split method
# returns a list with 2 elements
# first is train second is test
dataSplit = function(data, predicted.column, split.ratio) {
  
  # split data
  trainIndex <- createDataPartition(predicted.column, p = split.ratio, list = FALSE)
  data.train <- data[ trainIndex,]
  data.test <- data[-trainIndex,]
  
  # put split data in list
  # 1 for train
  # 2 for test
  data.split <- list()
  data.split[[1]] <- data.train
  data.split[[2]] <- data.test
  
  return(data.split)
}


# k-fold cross validation
# returns a list with k folds
# each fold is a list with 2 elements
# first is train and second is test
kFoldSplit = function(data, k) {
  vec <- vector()
  kfold.list <- list()
  
  for(i in 1:nrow(data)) {
    vec[i] <- sample(1:k, 1)
  }
  
  for(i in 1:k) {
    data.split <- list()
    
    data.train <- subset(data, vec != i)
    data.test <- subset(data, vec == i)
    
    data.split[[1]] <- data.train
    data.split[[2]] <- data.test
    
    kfold.list[[i]] <- data.split
  }
  
  return(kfold.list)
}






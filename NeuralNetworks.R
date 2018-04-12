# Neural Networks

library(neuralnet)
library(nnet)
library(ggplot2)

# comment line 65 and 66
# fixInNamespace("calculate.neuralnet", "neuralnet")

neuralNetworkSimpleSplit = function(a.data, a.formula, a.feature.combination, a.split.ratio) {

  # split data
  temp.list <- dataSplit(a.data, a.data$C1Stress, a.split.ratio)
  train.data <- temp.list[[1]]
  test.data <- temp.list[[2]]
  
  # split result labels into separate columns
  train.data <- cbind(train.data, class.ind(train.data$C1Stress))
  
  # train the neural network
  nn <- neuralnet(a.formula,
                  data = train.data,
                  hidden = 10,
                  threshold = 0.01,
                  stepmax = 100000,
                  startweights = NULL,
                  act.fct = 'logistic',
                  algorithm = 'rprop+',
                  linear.output = FALSE)
  
  # predict
  row.predictions <- compute(nn, test.data[,a.feature.combination])$net.result
  
  # merge row predictions into one
  predictions <- vector()
  for(i in 1:nrow(test.data)) {
    
    if(row.predictions[i,1] > row.predictions[i,2] && row.predictions[i,1] > row.predictions[i,3]) {
      predictions[i] <- 'lowStress'
    }
    
    if(row.predictions[i,2] > row.predictions[i,1] && row.predictions[i,2] > row.predictions[i,3]) {
      predictions[i] <- 'moderateStress'
    }
    
    if(row.predictions[i,3] > row.predictions[i,2] && row.predictions[i,3] > row.predictions[i,2]) {
      predictions[i] <- 'highStress'
    }
  }
  
  accuracy <- mean(test.data$C1Stress == predictions)
  
  return(accuracy)
}

neuralNetworkKFoldsSplit = function(a.data, a.formula, a.feature.combination, a.k) {
  
  # split data
  folds <- kFoldSplit(a.data, a.k)
  
  accuracy.vec <- vector()
  
  for (i in 1:a.k) {
    oneFold <- folds[[i]]
    
    train.data <- oneFold[[1]]
    test.data <- oneFold[[2]]
    
    # split result labels into separate columns
    train.data <- cbind(train.data, class.ind(train.data$C1Stress))
    
    # train the neural network
    nn <- neuralnet(a.formula,
                    data = train.data,
                    hidden = 10,
                    threshold = 0.01,
                    stepmax = 100000,
                    startweights = NULL,
                    act.fct = 'logistic',
                    algorithm = 'rprop+',
                    linear.output = FALSE)
    
    # predict
    row.predictions <- compute(nn, test.data[,a.feature.combination])$net.result
    
    # merge row predictions into one
    predictions <- vector()
    for(i in 1:nrow(test.data)) {
      
      if(row.predictions[i,1] > row.predictions[i,2] && row.predictions[i,1] > row.predictions[i,3]) {
        predictions[i] <- 'lowStress'
      }
      
      if(row.predictions[i,2] > row.predictions[i,1] && row.predictions[i,2] > row.predictions[i,3]) {
        predictions[i] <- 'moderateStress'
      }
      
      if(row.predictions[i,3] > row.predictions[i,2] && row.predictions[i,3] > row.predictions[i,2]) {
        predictions[i] <- 'highStress'
      }
    }
    
    # compute accuracy 
    accuracy <- mean(test.data$C1Stress == predictions)
    
    # add accuracy to vector
    accuracy.vec[length(accuracy.vec) + 1] <- accuracy
  }
  
  return(mean(accuracy.vec))
}


neuralNetwork = function(a.data, a.feature.list, a.num.iter, a.k, a.first.index, a.second.index) {
  
  # store all results
  prediction.acc.simple.split <- vector()
  prediction.acc.kfolds.split <- vector()
  
  best.prediction <- -100000
  best.formula <- vector()
  
  for(i in 1:length(a.feature.list)) {
    
    # formula for this combination of features
    f <- as.formula(paste("lowStress + moderateStress + highStress ~", paste(column.names[a.feature.list[[i]]][!column.names[a.feature.list[[i]]] %in% "C1Stress"], collapse = " + ")))
    
    # for each run
    for(j in 1:a.num.iter) {
      
      acc.vec <- vector()
      
      # run simple split k times
      for(z in 1:a.k) {
        acc <- neuralNetworkSimpleSplit(a.data, f, a.feature.list[[i]], 0.7)
        
        acc.vec[length(acc.vec) + 1] <- acc
      }
      # store prediction simple split
      prediction.acc.simple.split <- mean(acc.vec)
      
      # run kfolds 
      acc.kfolds <- neuralNetworkKFoldsSplit(a.data, f, a.feature.list[[i]], a.k)
      # stor prediction kfolds
      prediction.acc.kfolds.split <- acc.kfolds
      
      meanPred <- (prediction.acc.simple.split[j] + prediction.acc.kfolds.split[j]) / 2
      if(is.na(meanPred) == FALSE & is.na(best.prediction) == FALSE) {
        if(meanPred > best.prediction) {
          best.prediction <- meanPred
          best.formula <- f
        }
      }
    }
  }
  
  result.df <- data.frame("NN",
                          datasetsNames(a.first.index, a.second.index), 
                          mean(prediction.acc.simple.split),
                          mean(prediction.acc.kfolds.split),
                          (mean(prediction.acc.simple.split) + mean(prediction.acc.kfolds.split))/2,
                          as.character(Reduce(paste, deparse(best.formula))))
  
  colnames(result.df) <- c("Algorithm",
                           "Dataset", 
                           "Avg.acc.data.split", 
                           "Avg.acc.kfolds", 
                           "Average.acc",
                           "Formula")
  result.df$Formula <- as.character(result.df$Formula)
  
  return(result.df)
}

MLNeuralNetwork = function(a.datasets.list, a.feature.list, a.num.iter, a.k) {
  # start timing
  start.time <- proc.time()
  
  curr.num.data <- 0
  
  # create decision tree data frame
  Neural.Network.df <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(Neural.Network.df) <- c("Algorithm",
                                   "Dataset", 
                                   "Avg.acc.data.split", 
                                   "Avg.acc.kfolds", 
                                   "Average.acc",
                                   "Formula")
  
  # for in list of datasets
  for(i in 20:length(a.datasets.list)) {
    
    datasets <- a.datasets.list[[i]]
    
    # result
    result.df <- data.frame(matrix(ncol = 6, nrow = 0))
    colnames(result.df) <- c("Algorithm",
                             "Dataset", 
                             "Avg.acc.data.split", 
                             "Avg.acc.kfolds", 
                             "Average.acc",
                             "Formula")
    
    # for each dataset train and keep results
    for(j in 1:length(datasets)) {
      
      # progressometer
      curr.num.data <- curr.num.data + 1
      cat("Neural Network: Dataset", datasetsNames(i, j), "list =", i, "  dataset =", j, " -> ", round((curr.num.data/num.datasets)*100, 2), 
          "%  time: ", (proc.time() - start.time)[[3]]%/%60, "(m) ", round((proc.time() - start.time)[[3]]%%60, 3), "(s)\n")
      
      # train on dataset
      dataset <- datasets[[j]]
      result.df <- rbind(result.df, neuralNetwork(dataset, a.feature.list, a.num.iter, a.k, i, j))
    }
    # compute average on column
    result.df$Avg.acc.data.split <- round(mean(result.df$Avg.acc.data.split) * 100, 2)
    result.df$Avg.acc.kfolds <- round(mean(result.df$Avg.acc.kfolds) * 100, 2)
    result.df$Average.acc <- round((result.df$Avg.acc.data.split + result.df$Avg.acc.kfolds) / 2, 2)
    result.df$Formula <- rle(sort(result.df$Formula, decreasing = TRUE))[[2]][[1]]
    
    # add to final results
    Neural.Network.df <- rbind(Neural.Network.df, result.df[1,])
  }
  
  # output a csv file
  write.csv(Neural.Network.df, file = "NeuralNetworkResults.csv")
  
}




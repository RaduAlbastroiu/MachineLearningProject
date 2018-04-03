library(caret)
library(caretEnsemble)



randForestSvmSimpleSplit = function(a.data, a.formula, a.split.ratio) {
  
  a.data <- a.data[, !colnames(a.data) %in% c("PSS_Score", "C2Stress")]
  
  # split data
  temp.list <- dataSplit(a.data, a.data$C1Stress, a.split.ratio)
  train.data <- temp.list[[1]]
  test.data <- temp.list[[2]]
  
  # Random Forests model
  mod.rf <- train(f, data = train.data, metric = "Accuracy",  method = "rf", verbose=FALSE)
  pred.rf <- predict(mod.rf, test.data)
  
  
  # SVM model
  mod.svm <- train(f, data = train.data,  method = "svmRadial", metric = "Accuracy", verbose=FALSE)
  pred.svm <- predict(mod.svm, test.data)
  
  results <- resamples(list(rf = mod.rf, svm = mod.svm)) 
  
  # correlation between results
  # modelCor(results) 
  # summary(results)
  # dotplot(results)
  # splom(results)
  
  # predictions data frame
  predDF <- data.frame(pred.rf, pred.svm, C1Stress = test.data$C1Stress)
  
  combModFit.gbm <- train(C1Stress~., 
                          method = "gbm",
                          metric = "Accuracy",  
                          tuneGrid=expand.grid(
                            .n.trees=20,
                            .interaction.depth=2,
                            .shrinkage=.1,
                            .n.minobsinnode = 2
                          ),
                          data = predDF, 
                          distribution = "multinomial", verbose=FALSE)
  combPred.gbm <- predict(combModFit.gbm, predDF)
  
  #confusionMatrix(combPred.gbm, test.data$C1Stress)$overall[1]
  
  pred1V <- predict(mod.rf, test.data)
  pred2V <- predict(mod.svm, test.data)
  predVDF <- data.frame(pred.rf = pred1V, pred.svm = pred2V)
  combPredV <- predict(combModFit.gbm, predVDF)
  
  
  accuracy <- cbind(confusionMatrix(pred1V, test.data$C1Stress)$overall[1], 
                    confusionMatrix(pred2V, test.data$C1Stress)$overall[1], 
                    confusionMatrix(combPredV, test.data$C1Stress)$overall[1])
  
  colnames(accuracy) <- c("RF", "SVM", "Stacking")
  
  return(accuracy)
}




randForestSvmKFoldsSplit = function(a.data, a.formula, a.k) {
  a.data <- a.data[, !colnames(a.data) %in% c("PSS_Score", "C2Stress")]
  
  # split data
  folds <- kFoldSplit(a.data, a.k)
  
  # result vectors
  rf.vec <- vector()
  svm.vec <- vector()
  stack.vec <- vector()
  
  for(i in 1:a.k) {
    oneFold <- folds[[i]]
    
    train.data <- oneFold[[1]]
    test.data <- oneFold[[2]]
    
    # Random Forests model
    mod.rf <- train(f, data = train.data, metric = "Accuracy", method = "rf", verbose=FALSE)
    pred.rf <- predict(mod.rf, test.data)
    
    # SVM model
    mod.svm <- train(f, data = train.data, metric = "Accuracy", method="svmRadial", verbose=FALSE)
    pred.svm <- predict(mod.svm, test.data)
    
    # RF model accuracy
    #confusionMatrix(pred.rf, test.data$C1Stress)$overall[1]
    
    # SVM model accuracy
    #confusionMatrix(pred.svm, test.data$C1Stress)$overall[1]
    
    results <- resamples(list(rf = mod.rf, svm = mod.svm)) 
    
    # correlation between results
    # modelCor(results) 
    # summary(results)
    # dotplot(results)
    # splom(results)
    
    print(i)
    print(dim(predDF))
    
    # predictions data frame
    predDF <- data.frame(pred.rf, pred.svm, C1Stress = test.data$C1Stress)

    combModFit.gbm <- train(C1Stress~., 
                            method = "gbm",
                            metric = "Accuracy",
                            tuneGrid=expand.grid(
                              .n.trees=20,
                              .interaction.depth=2,
                              .shrinkage=.1,
                              .n.minobsinnode = 2
                            ),
                            data = predDF, 
                            distribution = "multinomial", verbose=FALSE)
    
    combPred.gbm <- predict(combModFit.gbm, predDF)
    
    #confusionMatrix(combPred.gbm, test.data$C1Stress)$overall[1]
    
    pred1V <- predict(mod.rf, test.data)
    pred2V <- predict(mod.svm, test.data)
    predVDF <- data.frame(pred.rf = pred1V, pred.svm = pred2V)
    combPredV <- predict(combModFit.gbm, predVDF)
    
    
    rf.vec[length(rf.vec) + 1] <- confusionMatrix(pred1V, test.data$C1Stress)$overall[1]
    svm.vec[length(svm.vec) + 1] <- confusionMatrix(pred2V, test.data$C1Stress)$overall[1]
    stack.vec[length(stack.vec) + 1] <- confusionMatrix(combPredV, test.data$C1Stress)$overall[1]
    
  }
  
  rf.vec <- rf.vec[!is.na(rf.vec)]
  svm.vec <- svm.vec[!is.na(svm.vec)]
  stack.vec <- stack.vec[!is.na(stack.vec)]
  
  accuracy <- cbind(mean(rf.vec), 
                    mean(svm.vec), 
                    mean(stack.vec))
  
  colnames(accuracy) <- c("RF", "SVM", "Stacking")
  
  return(accuracy)
}





randForestSVMStacking = function(a.data, a.feature.list, a.num.iter, a.k, a.first.index, a.second.index) {
  
  prediction.RF.simple.split <- vector()
  prediction.SVM.simple.split <- vector()
  prediction.STACK.simple.split <- vector()
  prediction.RF.kfolds.split <- vector()
  prediction.SVM.kfolds.split <- vector()
  prediction.STACK.kfolds.split <- vector()
  
  best.prediction <- -100000
  best.formula <- vector()
  
  for(i in 1:length(a.feature.list)) {
    
    # formula for this combination of features
    f <- as.formula(paste("C1Stress ~", paste(column.names[a.feature.list[[i]]][!column.names[a.feature.list[[i]]] %in% "C1Stress"], collapse = " + ")))
    
    # for each run
    for(j in 1:a.num.iter) {
      
      RF.vec <- vector()
      SVM.vec <- vector()
      STACK.vec <- vector()
      Kfolds.split <- vector()
      
      # run simple split as many times as k
      for(z in 1:a.k) {
        res <- randForestSvmSimpleSplit(a.data, f, 0.7)
        
        RF.vec[length(RF.vec) + 1] <- res[1]
        SVM.vec[length(SVM.vec) + 1] <- res[2]
        STACK.vec[length(STACK.vec) + 1] <- res[3]
      }
      
      RF.vec <- RF.vec[!is.na(RF.vec)]
      SVM.vec <- SVM.vec[!is.na(SVM.vec)]
      STACK.vec <- STACK.vec[!is.na(STACK.vec)]
      
      
      # simple split results
      prediction.RF.simple.split[j] <- mean(RF.vec)
      prediction.SVM.simple.split[j] <- mean(SVM.vec)
      prediction.STACK.simple.split[j] <- mean(STACK.vec)
      
      # run k folds
      Kfolds.split <- randForestSvmKFoldsSplit(a.data, f, a.k)
      
      prediction.RF.kfolds.split <- Kfolds.split[1]
      prediction.SVM.kfolds.split <- Kfolds.split[2]
      prediction.STACK.kfolds.split <- Kfolds.split[3]
      
      meanPred <- (prediction.STACK.simple.split[j] + prediction.STACK.kfolds.split[j]) / 2
      if(is.na(meanPred) == FALSE & is.na(best.prediction) == FALSE) {
        if(meanPred > best.prediction) {
          best.prediction <- meanPred
          best.formula <- f
        }
      }
    }
  }
  
  print(a.first.index)
  print(a.second.index)
  result.df <- data.frame("Stacking Random Forest and SVM", 
                          datasetsNames(a.first.index, a.second.index), 
                          mean(prediction.RF.simple.split),
                          mean(prediction.SVM.simple.split),
                          mean(prediction.STACK.simple.split),
                          mean(prediction.RF.kfolds.split),
                          mean(prediction.SVM.kfolds.split),
                          mean(prediction.STACK.kfolds.split),
                          (mean(prediction.STACK.simple.split) + mean(prediction.STACK.kfolds.split))/2,
                          as.character(Reduce(paste, deparse(best.formula))))
  
  colnames(result.df) <- c("Algorithm",
                           "Dataset", 
                           "Avg.rf.data.split", 
                           "Avg.svm.data.split",
                           "Avg.stack.data.split",
                           "Avg.rf.kfolds.split", 
                           "Avg.svm.kfolds.split",
                           "Avg.stack.kfolds.split",
                           "Average.acc.stack",
                           "Formula")
  result.df$Formula <- as.character(result.df$Formula)
  
  return(result.df)
}


MLRandForestSVMStacking = function(a.datasets.list, a.feature.list, a.num.iter, a.k) {
  # start timing
  start.time <- proc.time()
  
  curr.num.data <- 0
  
  # create decision tree data frame
  RF.SVM.df <- data.frame(matrix(ncol = 10, nrow = 0))
  colnames(RF.SVM.df) <- c("Algorithm",
                           "Dataset",
                           "Avg.rf.data.split", 
                           "Avg.svm.data.split",
                           "Avg.stack.data.split",
                           "Avg.rf.kfolds.split", 
                           "Avg.svm.kfolds.split",
                           "Avg.stack.kfolds.split",
                           "Average.acc.stack",
                           "Formula")
  
  # for in list of datasets
  for(i in 1:length(a.datasets.list)) {
    
    datasets <- a.datasets.list[[i]]
    
    # result
    result.df <- data.frame(matrix(ncol = 10, nrow = 0))
    colnames(result.df) <- c("Algorithm",
                             "Dataset", 
                             "Avg.rf.data.split", 
                             "Avg.svm.data.split",
                             "Avg.stack.data.split",
                             "Avg.rf.kfolds.split", 
                             "Avg.svm.kfolds.split",
                             "Avg.stack.kfolds.split",
                             "Average.acc.stack",
                             "Formula")
    
    # for each dataset train and keep results
    for(j in 1:length(datasets)) {
      
      # progressometer
      curr.num.data <- curr.num.data + 1
      cat("Random Forest and SVM stacking: Dataset", datasetsNames(i, j), "list =", i, "  dataset =", j, " -> ", round((curr.num.data/num.datasets)*100, 2),
          "%  time: ", (proc.time() - start.time)[[3]]%/%60, "(m) ", round((proc.time() - start.time)[[3]]%%60, 3), "(s)\n")
      
      # train on dataset
      dataset <- datasets[[j]]
      result.df <- rbind(result.df, randForestSVMStacking(dataset, a.feature.list, a.num.iter, a.k, i, j))
    }
    
    # compute average on column
    result.df$Avg.rf.data.split <- round(mean(result.df$Avg.rf.data.split) * 100, 2)
    result.df$Avg.svm.data.split <- round(mean(result.df$Avg.svm.data.split) * 100, 2)
    result.df$Avg.stack.data.split <- round(mean(result.df$Avg.stack.data.split) * 100, 2)
    result.df$Avg.rf.kfolds.split <- round(mean(result.df$Avg.rf.kfolds.split) * 100, 2)
    result.df$Avg.svm.kfolds.split <- round(mean(result.df$Avg.svm.kfolds.split) * 100, 2)
    result.df$Avg.stack.kfolds.split <- round(mean(result.df$Avg.stack.kfolds.split) * 100, 2)
    result.df$Average.acc.stack <- round((result.df$Avg.stack.data.split + result.df$Avg.stack.kfolds.split) / 2, 2)
    result.df$Formula <- rle(sort(result.df$Formula, decreasing = TRUE))[[2]][[1]]
    
    # add to final results
    RF.SVM.df <- rbind(RF.SVM.df, result.df[1,])
  }
  
  # output a csv file
  write.csv(RF.SVM.df, file = "RFSVMResults.csv")
}













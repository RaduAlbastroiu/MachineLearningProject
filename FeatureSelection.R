# prepare data

FitAll = lm(PSS_Score ~ ., data = selData)
FitStart = lm(PSS_Score ~ 1, data = selData)


# forward selection
forward <- step(FitStart, direction = "forward", scope = formula(FitAll))

# put forward results column numbers in a list
forwardResult <- colnames(forward$model)
forwardColNum <- list()
j <- 0
for(colName in forwardResult) {
  for(i in 3:30) {
    if(colName == columnNames[i] && j > 0) {
        forwardColNum[[j]] <- i
        break()
    }
  }
  j <- j + 1
}
rm(forward)
rm(forwardResult)


# backward elimination
backward <- step(FitAll, direction = "backward")
backwardResult <- backward$model[0,]

# put backward results column numbers in a list
backwardResult <- colnames(backward$model)
backwardColNum <- list()
j <- 0
for(colName in backwardResult) {
  for(i in 3:30) {
    if(colName == columnNames[i] && j > 0) {
      backwardColNum[[j]] <- i
      break()
    }
  }
  j <- j + 1
}
rm(backward)
rm(backwardResult)

# stepwise regression
stepwise <- step(FitStart, direction = "both", scope = formula(FitAll))
stepwiseResult <- stepwise$model[0,]

# put stepwise results column numbers in a list
stepwiseResult <- colnames(stepwise$model)
stepwiseColNum <- list()
j <- 0
for(colName in stepwiseResult) {
  for(i in 3:30) {
    if(colName == columnNames[i] && j > 0) {
      stepwiseColNum[[j]] <- i
      break()
    }
  }
  j <- j + 1
}
rm(stepwise)
rm(stepwiseResult)

rm(FitAll)
rm(FitStart)

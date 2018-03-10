# Wrapper Feature Selection

# method
extractFeatures = function(stepwise) {
  result <- colnames(stepwise$model)
  num.col <- vector()
  j <- 1
  for(name.col in result) {
    for(i in 3:30) {
      if(name.col == column.names[i]) {
        num.col[j] <- i
        break()
      }
    }
    j <- j + 1
  }
  num.col <- num.col[2:length(num.col)]
  num.col <- subset(num.col, is.na(num.col) == FALSE)
  return(num.col)
}



# run forward backword and stepwise for each dataset
for(i in 1:length(datasets.list)) {
  
  datasets <- datasets.list[[i]]
  
  # for each dataset 
  for(j in 1:length(datasets)) {
 
    dataset <- datasets[[j]]
    # prepare model
    FitAll = lm(PSS_Score ~ ., data = dataset[,1:30])
    FitStart = lm(PSS_Score ~ 1, data = dataset[,1:30])

    # forward selection
    forward <- step(FitStart, direction = "forward", scope = formula(FitAll), trace = 0)

    # backward elimination
    backward <- step(FitAll, direction = "backward", trace = 0)

    # stepwise regression
    stepwise <- step(FitStart, direction = "both", scope = formula(FitAll), trace = 0)

    # extract features from model
    feature.selection.list[[length(feature.selection.list) + 1]] <- extractFeatures(forward)
    feature.selection.list[[length(feature.selection.list) + 1]] <- extractFeatures(backward)
    feature.selection.list[[length(feature.selection.list) + 1]] <- extractFeatures(stepwise)
  }
}


# remove residual variables
rm(i,j)
rm(forward)
rm(backward)
rm(stepwise)
rm(FitAll, FitStart)
rm(dataset, datasets)
rm(simple.list)
rm(extractFeatures)
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


# put all data in one list
datasets.list <- list()
simple.list <- list()
simple.list[[1]] <- simple.data
simple.list[[2]] <- normalized.simple.data
simple.list[[3]] <- scaled.simple.data

# add simple data
datasets.list[[1]] <- simple.list

# add oversampled data
datasets.list[[2]] <- oversampled.datasets
datasets.list[[3]] <- normalized.oversampled.datasets
datasets.list[[4]] <- scaled.oversampled.datasets

# add undersampled data
datasets.list[[5]] <- undersampled.datasets
datasets.list[[6]] <- normalized.undersampled.datasets
datasets.list[[7]] <- scaled.undersampled.datasets

# add hybrid data
datasets.list[[8]] <- hybrid.datasets
datasets.list[[9]] <- normalized.hybrid.datasets
datasets.list[[10]] <- scaled.hybrid.datasets


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
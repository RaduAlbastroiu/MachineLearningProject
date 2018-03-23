# Data normalization


# normalize data
normalizeData = function(a.data) {
  
  # separate data
  numeric.data <- a.data[,1:30]
  non.numeric.data <- a.data[,31:32]
  
  # get min and max of each feature
  maxs <- apply(numeric.data, 2, max)
  mins <- apply(numeric.data, 2, min)

  # normalized data
  normalized.data <- scale(numeric.data, center = mins, scale = maxs - mins)
  normalized.data <- as.data.frame(normalized.data)
  
  normalized.data <- cbind(normalized.data, non.numeric.data)
  return(normalized.data) 
}

# scale data
scaleData = function(a.data) {
  
  # separate data
  numeric.data <- a.data[,1:30]
  non.numeric.data <- a.data[,31:32]
  
  # normalized data
  scaled.data <- scale(numeric.data)
  
  scaled.data <- cbind(scaled.data, non.numeric.data)
  
  return(scaled.data) 
}

# normalize a list of datasets
normalizeDatasets = function(a.datasets) {
  
  # create normalized list of datasets
  normalized.datasets <- list()
  
  for(i in 1:length(a.datasets)) {
    
    data <- a.datasets[[i]]
      
    # normalize
    normalized.data <- normalizeData(data)
      
    # add to list
    normalized.datasets[[i]] <- normalized.data
  }
  
  return(normalized.datasets)
}

# scale a list of datasets
scaleDatasets = function(a.datasets) {
  
  # create scaled list of datasets
  scaled.datasets <- list()
  
  for(i in 1:length(a.datasets)) {
    
    data <- a.datasets[[i]]
    
    # scale
    scaled.data <- scaleData(data)
    
    # add to list
    scaled.datasets[[i]] <- scaled.data
  }
  
  return(scaled.datasets)
}

# data normalization

# simple data normalized and scaled
normalized.simple.data <- normalizeData(simple.data)
scaled.simple.data <- scaleData(simple.data)


# C1Stress modified data normalized and scaled
normalized.C1Stress.dataset <- normalizeData(C1Stress.modified.dataset)
scaled.C1Stress.dataset <- scaleData(C1Stress.modified.dataset)


# oversampled data normalized and scaled
normalized.oversampled.datasets <- normalizeDatasets(oversampled.datasets)
scaled.oversampled.datasets <- scaleDatasets(oversampled.datasets)


# undersampled data normalized and scaled
normalized.undersampled.datasets <- normalizeDatasets(undersampled.datasets)
scaled.undersampled.datasets <- scaleDatasets(undersampled.datasets)


# hybrid data normalized and scaled
normalized.hybrid.datasets <- normalizeDatasets(hybrid.datasets)
scaled.hybrid.datasets <- scaleDatasets(hybrid.datasets)


# Rose data normalized and scaled
normalized.rose.datasets <- normalizeDatasets(rose.datasets)
scaled.rose.datasets <- scaleDatasets(rose.datasets)


# Smote data normalized and scaled
normalized.smote.datasets <- normalizeDatasets(smote.datasets)
scaled.smote.datasets <- scaleDatasets(smote.datasets)


# remove residual variables
rm(normalizeData, scaleData, normalizeDatasets, scaleDatasets)


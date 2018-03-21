# Filter data

library(caTools)
library(ROSE)
library(DMwR)

# Filter average excess of distance 
data <- subset(data, averageExcessOfDistanceBetweenClicks < 25 & decision_time_efficiency > 10000)

# list of datasets
undersampled.datasets <- list()
oversampled.datasets <- list()
hybrid.datasets <- list()
rose.datasets <- list()
smote.datasets <- list()

# simple data
simple.data <- data

# number of datasets of each type
num.datasets <- 3

# number of elements from each category
num.low.stress <- nrow(data[data$C1Stress == 'lowStress',])
num.moderate.stress <- nrow(data[data$C1Stress == 'moderateStress',])
num.high.stress <- nrow(data[data$C1Stress == 'highStress',])

df.low.stress <- subset(data, C1Stress == 'lowStress')
df.moderate.stress <- subset(data, C1Stress == 'moderateStress')
df.high.stress <- subset(data, C1Stress == 'highStress')


# oversampling
for(i in 1:num.datasets) {
  
  # randomize row selections
  sample.low <- sample(1:nrow(df.low.stress), num.moderate.stress - num.low.stress, replace = TRUE)
  sample.high <- sample(1:nrow(df.high.stress), num.moderate.stress - num.high.stress, replace = TRUE)
  
  # create copy from row selections for low values
  df.low.copy <- data.frame()
  for(index in sample.low) {
    row <- df.low.stress[index,]
    df.low.copy <- rbind(df.low.copy, row)
  }
  colnames(df.low.copy) <- colnames(df.low.stress)
  
  # create copy from row selections for high values
  df.high.copy <- data.frame()
  for(index in sample.high) {
    row <- df.high.stress[index,]
    df.high.copy <- rbind(df.high.copy, row)
  }
  colnames(df.high.copy) <- colnames(df.high.stress)
  
  # create new oversampled dataset
  df.oversampled <- rbind(df.low.copy, df.low.stress, df.moderate.stress, df.high.copy, df.high.stress)
  
  # add new created df to datasets list
  oversampled.datasets[[i]] <- df.oversampled
}


# undersampling data
for(i in 1:num.datasets) {
  
  # randomize row selections
  sample.low <- sample(1:nrow(df.low.stress), num.high.stress, replace = TRUE)
  sample.moderate <- sample(1:nrow(df.moderate.stress), num.high.stress, replace = TRUE)
  
  # create copy from row selections for low values
  df.low.copy <- data.frame()
  for(index in sample.low) {
    row <- df.low.stress[index,]
    df.low.copy <- rbind(df.low.copy, row)
  }
  colnames(df.low.copy) <- colnames(df.low.stress)
  
  # create copy from row selections for moderate values
  df.moderate.copy <- data.frame()
  for(index in sample.moderate) {
    row <- df.moderate.stress[index,]
    df.moderate.copy <- rbind(df.moderate.copy, row)
  }
  colnames(df.moderate.copy) <- colnames(df.moderate.stress)
  
  # create new undersampled dataset
  df.undersampled <- rbind(df.low.copy, df.moderate.copy, df.high.stress)
  
  # add new created df to datasets list
  undersampled.datasets[[i]] <- df.undersampled
}


# hybrid data
for(i in 1:num.datasets) {
  
  # number of selections
  selection.num <- 50
  
  # randomize row selections
  sample.low <- sample(1:nrow(df.low.stress), selection.num, replace = TRUE)
  sample.moderate <- sample(1:nrow(df.moderate.stress), selection.num, replace = TRUE)
  sample.high <- sample(1:nrow(df.high.stress), selection.num, replace = TRUE)
  
  # create copy from row selections for low values
  df.low.copy <- data.frame()
  for(index in sample.low) {
    row <- df.low.stress[index,]
    df.low.copy <- rbind(df.low.copy, row)
  }
  colnames(df.low.copy) <- colnames(df.low.stress)
  
  # create copy from row selections for moderate values
  df.moderate.copy <- data.frame()
  for(index in sample.moderate) {
    row <- df.moderate.stress[index,]
    df.moderate.copy <- rbind(df.moderate.copy, row)
  }
  colnames(df.moderate.copy) <- colnames(df.moderate.stress)
  
  # create copy from row selections for high values
  df.high.copy <- data.frame()
  for(index in sample.high) {
    row <- df.high.stress[index,]
    df.high.copy <- rbind(df.high.copy, row)
  }
  colnames(df.high.copy) <- colnames(df.high.stress)
  
  # create new undersampled dataset
  df.hybrid <- rbind(df.low.copy, df.moderate.copy, df.high.copy)
  
  # add new created df to datasets list
  hybrid.datasets[[i]] <- df.hybrid
}


# rose method
for(i in 1:(2*num.datasets)) {
  
  # take only 2 v
  lowModDf <- subset(simple.data, C1Stress != 'highStress')
  highModDf <- subset(simple.data, C1Stress != 'lowStress')

  # apply rose method
  lowModDf <- ROSE(C1Stress~., data = lowModDf, N = 100)$data
  highModDf <- ROSE(C1Stress~., data = highModDf, N = 100)$data
  
  # combine the results
  resultDf <- lowModDf
  resultDf <- rbind(resultDf, subset(highModDf, C1Stress == 'highStress'))
  
  # put results in list
  rose.datasets[[i]] <- resultDf
}

# smote method
for(i in 1:(2*num.datasets)) {
  
  # take only 2 v
  lowModDf <- subset(simple.data, C1Stress != 'highStress')
  highModDf <- subset(simple.data, C1Stress != 'lowStress')
  
  # reduce factors to only 2
  lowModDf$C1Stress <- factor(lowModDf$C1Stress)
  highModDf$C1Stress <- factor(highModDf$C1Stress)
  
  # apply smote method
  lowModDf <- SMOTE(C1Stress ~ ., data = lowModDf, perc.over = 350, perc.under=150)
  highModDf <- SMOTE(C1Stress ~ ., data = highModDf, perc.over = 600, perc.under=150)
  
  # combine the results
  resultDf <- lowModDf
  resultDf <- rbind(resultDf, subset(highModDf, C1Stress == 'highStress'))
  
  # put results in list
  smote.datasets[[i]] <- resultDf
}

# C1Stress modified data

# set dataset
C1Stress.modified.dataset <- data
  
C1Stress.modified.dataset$C1Stress <- ifelse(C1Stress.modified.dataset$PSS_Score <= 22, 'lowStress', C1Stress.modified.dataset$C1Stress)
C1Stress.modified.dataset$C1Stress <- ifelse(C1Stress.modified.dataset$PSS_Score > 28, 'highStress', C1Stress.modified.dataset$C1Stress)
C1Stress.modified.dataset$C1Stress <- ifelse(C1Stress.modified.dataset$C1Stress != 'lowStress' & C1Stress.modified.dataset$C1Stress != 'highStress', 'moderateStress', C1Stress.modified.dataset$C1Stress)
C1Stress.modified.dataset$C1Stress <- as.factor(C1Stress.modified.dataset$C1Stress)

# remove residual variables
rm(num.datasets, num.low.stress, num.moderate.stress, num.high.stress)
rm(df.low.stress, df.moderate.stress, df.high.stress)
rm(df.low.copy, df.moderate.copy, df.high.copy)
rm(sample.low, sample.moderate, sample.high)
rm(i, index, row, selection.num)
rm(df.undersampled, df.oversampled, df.hybrid)

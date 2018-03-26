
# read and store data
source('DataReader.R')

# process data
source('DataProcessing.R')

# normalize and scale data
source('DataNormalization.R')

# data spliter
source('DataSplitter.R')


# returns the name of the dataset from datasets.list
datasetsNames = function(first.index, second.index) {
  if(first.index > 2)
  {
    if(first.index == 3)
      return('Oversampled data')
    if(first.index == 4)
      return('Undersampled data')
    if(first.index == 5)
      return('Hybrid data')
    if(first.index == 6)
      return('Rose data')
    if(first.index == 7)
      return('Smote data')
    if(first.index == 8)
      return('Normalized Oversampled data')
    if(first.index == 9)
      return('Normalized Undersampled data')
    if(first.index == 10)
      return('Normalized Hybrid data')
    if(first.index == 11)
      return('Normalized Rose data')
    if(first.index == 12)
      return('Normalized Smote data')
    if(first.index == 13)
      return('Scaled Oversampled data')
    if(first.index == 14)
      return('Scaled Undersampled data')
    if(first.index == 15)
      return('Scaled Hybrid data')
    if(first.index == 16)
      return('Scaled Rose data')
    if(first.index == 17)
      return('Scaled Smote data')
   
  }
  else
  {
    if(first.index == 1) {
      if(second.index == 1)
        return('Simple data')
      if(second.index == 2)
        return('Normalized simple data')
      if(second.index == 3)
        return('Scaled simple data')
    }
    if(first.index == 2) {
      if(second.index == 1)
        return('C1Stress modified')
      if(second.index == 2)
        return('Normalized C1Stress modified')
      if(second.index == 3)
        return('Scaled C1Stress modified')
    }
  }
}


# put all data in one list
datasets.list <- list()
simple.list <- list()

simple.list[[1]] <- simple.data
simple.list[[2]] <- normalized.simple.data
simple.list[[3]] <- scaled.simple.data

# add simple data
datasets.list[[1]] <- simple.list


# add C1Stress modified data
c1stress.modified.list <- list()
c1stress.modified.list[[1]] <- C1Stress.modified.dataset
c1stress.modified.list[[2]] <- normalized.C1Stress.dataset
c1stress.modified.list[[3]] <- scaled.C1Stress.dataset
datasets.list[[2]] <- c1stress.modified.list


# add oversampled data
datasets.list[[3]] <- oversampled.datasets

# add undersampled data
datasets.list[[4]] <- undersampled.datasets

# add hybrid data
datasets.list[[5]] <- hybrid.datasets

# add Rose data
datasets.list[[6]] <- rose.datasets

# add Smote data
datasets.list[[7]] <- smote.datasets


# add normalized data

# add normalized oversampled
datasets.list[[8]] <- normalized.oversampled.datasets

# add normalized undersampled
datasets.list[[9]] <- normalized.undersampled.datasets

# add normalized hybrid
datasets.list[[10]] <- normalized.hybrid.datasets

# add normalized rose 
datasets.list[[11]] <- normalized.rose.datasets

# add normalized smote
datasets.list[[12]] <- normalized.smote.datasets


# add scaled data

# add scaled oversampled
datasets.list[[13]] <- scaled.oversampled.datasets

# add scaled undersampled
datasets.list[[14]] <- scaled.undersampled.datasets

# add scaled hybrid
datasets.list[[15]] <- scaled.hybrid.datasets

# add scaled rose
datasets.list[[16]] <- scaled.rose.datasets

# add scaled smote
datasets.list[[17]] <- scaled.smote.datasets



# compute number datasets
num.datasets <- 0
for(i in 1:length(datasets.list)) {
  num.datasets <- num.datasets + length(datasets.list[[i]])
}

rm(i)
rm(simple.list, c1stress.modified.list)
#rm(normalized.simple.data, scaled.simple.data)
#rm(normalized.oversampled.datasets, scaled.oversampled.datasets)
#rm(normalized.undersampled.datasets, scaled.undersampled.datasets)
#rm(normalized.hybrid.datasets, scaled.hybrid.datasets)


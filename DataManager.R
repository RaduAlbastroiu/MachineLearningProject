
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
  if(first.index > 1)
  {
    if(first.index == 2)
      return('Oversampled data')
    if(first.index == 3)
      return('Undersampled data')
    if(first.index == 4)
      return('Hybrid data')
    if(first.index == 5)
      return('C1stress modif data')
    if(first.index == 6)
      return('Rose method dataset')
    if(first.index == 7)
      return('Smote method dataset')
    
    #if(first.index == 6)
    #  return('normalized undersampled data')
    #if(first.index == 7)
    #  return('scaled undersampled data')
    
    #if(first.index == 8)
    #  return('hybrid data')
    #if(first.index == 9)
    #  return('normalized hybrid data')
    #if(first.index == 10)
    #  return('scaled hybrid data')
  }
  else
  {
    if(second.index == 1)
      return('simple data')
    #if(second.index == 2)
    #  return('normalized simple data')
    #if(second.index == 3)
    #  return('scaled simple data')
  }
}


# put all data in one list
datasets.list <- list()
simple.list <- list()

simple.list[[1]] <- simple.data
#simple.list[[2]] <- normalized.simple.data
#simple.list[[3]] <- scaled.simple.data

# add simple data
datasets.list[[1]] <- simple.list

# add oversampled data
datasets.list[[2]] <- oversampled.datasets
#datasets.list[[3]] <- normalized.oversampled.datasets
#datasets.list[[4]] <- scaled.oversampled.datasets

# add undersampled data
datasets.list[[3]] <- undersampled.datasets
#datasets.list[[6]] <- normalized.undersampled.datasets
#datasets.list[[7]] <- scaled.undersampled.datasets

# add hybrid data
datasets.list[[4]] <- hybrid.datasets
#datasets.list[[9]] <- normalized.hybrid.datasets
#datasets.list[[10]] <- scaled.hybrid.datasets

# add C1Stress modified data
c1stress.modified.list <- list()
c1stress.modified.list[[1]] <- C1Stress.modified.dataset
datasets.list[[5]] <- c1stress.modified.list

# add Rose data
datasets.list[[6]] <- rose.datasets

# add Smote data
datasets.list[[7]] <- smote.datasets



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


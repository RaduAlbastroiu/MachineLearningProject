
# read and store data
source('DataReader.R')

# process data
source('DataProcessing.R')

# normalize and scale data
source('DataNormalization.R')

# data spliter
source('DataSplitter.R')

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


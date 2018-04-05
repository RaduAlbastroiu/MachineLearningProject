
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

    if(first.index == 1)
      return('Simple data')
    if(first.index == 2)
      return('C1Stress data')
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
      return('Normalized Simple data')
    if(first.index == 9)
      return('Normalized C1Stress data')
    if(first.index == 10)
      return('Normalized Oversampled data')
    if(first.index == 11)
      return('Normalized Undersampled data')
    if(first.index == 12)
      return('Normalized Hybrid data')
    if(first.index == 13)
      return('Normalized Rose data')
    if(first.index == 14)
      return('Normalized Smote data')
    if(first.index == 15)
      return("Scaled Simple data")
    if(first.index == 16)
      return("Scaled C1Stress data")
    if(first.index == 17)
      return('Scaled Oversampled data')
    if(first.index == 18)
      return('Scaled Undersampled data')
    if(first.index == 19)
      return('Scaled Hybrid data')
    if(first.index == 20)
      return('Scaled Rose data')
    if(first.index == 21)
      return('Scaled Smote data')
   
}


# put all data in one list
datasets.list <- list()
simple.list <- list()

# # Using only Smote and Rose datasets
# datasets.list[[1]] <- scaled.rose.datasets
# datasets.list[[2]] <- scaled.smote.datasets

simple.list[[1]] <- simple.data

# add simple data
datasets.list[[1]] <- simple.list


# add C1Stress modified data
c1stress.modified.list <- list()
c1stress.modified.list[[1]] <- C1Stress.modified.dataset
datasets.list[[2]] <- c1stress.modified.list


#add oversampled data
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

# add normalized simple data
simple.list[[1]] <- normalized.simple.data
datasets.list[[8]] <- simple.list

# add normalized C1Stress data
simple.list[[1]] <- normalized.C1Stress.dataset
datasets.list[[9]] <- simple.list

# add normalized oversampled
datasets.list[[10]] <- normalized.oversampled.datasets

# add normalized undersampled
datasets.list[[11]] <- normalized.undersampled.datasets

# add normalized hybrid
datasets.list[[12]] <- normalized.hybrid.datasets

# add normalized rose 
datasets.list[[13]] <- normalized.rose.datasets

# add normalized smote
datasets.list[[14]] <- normalized.smote.datasets


# add scaled data

# add scaled simple data
simple.list[[1]] <- scaled.simple.data
datasets.list[[15]] <- simple.list

# add scaled C1Stress data
simple.list[[1]] <- scaled.C1Stress.dataset
datasets.list[[16]] <- simple.list

# add scaled oversampled
datasets.list[[17]] <- scaled.oversampled.datasets

# add scaled undersampled
datasets.list[[18]] <- scaled.undersampled.datasets

# add scaled hybrid
datasets.list[[19]] <- scaled.hybrid.datasets

# add scaled rose
datasets.list[[20]] <- scaled.rose.datasets

# add scaled smote
datasets.list[[21]] <- scaled.smote.datasets



# compute number datasets
num.datasets <- 0
for(i in 1:length(datasets.list)) {
  num.datasets <- num.datasets + length(datasets.list[[i]])
}

rm(i)
rm(simple.list)
rm(c1stress.modified.list)
rm(normalized.simple.data, scaled.simple.data)
rm(normalized.oversampled.datasets, scaled.oversampled.datasets)
rm(normalized.undersampled.datasets, scaled.undersampled.datasets)
rm(normalized.hybrid.datasets, scaled.hybrid.datasets)


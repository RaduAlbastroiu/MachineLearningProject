# matrix with the corresponding Pearson Correlation values
correlationMatrix <- data.frame(ColumnNr = 3:30, Value = cor(data[,3:30], data[,2]))

# sort correlation matrix
correlationMatrix <- correlationMatrix[order(abs(correlationMatrix$Value), decreasing = TRUE),]

# create a list with the chosen columns by the correlation
feature.list.pearson <- list() 

# nr of features
num.features <- 30

# choose features in order from most important to least important
for (i in 1:num.features) {
  vec <- numeric(num.features) 
  
  for (j in num.features:i) {
      vec[j] <- 0
  }
  
  for (j in 1:i) {
    vec[j] <- correlationMatrix[j,1] 
  }
  
  # add vector to list
  feature.list.pearson[[i]] <- vec
}

# remove residual variables
rm(num.features, i, j)


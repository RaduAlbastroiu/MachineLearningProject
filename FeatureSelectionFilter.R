# Feature Selection Filter

# matrix with the corresponding Pearson Correlation values
correlationMatrix <- data.frame(ColumnNr = 3:30, Value = cor(data[,3:30], data[,2]))

# sort correlation matrix
correlationMatrix <- correlationMatrix[order(abs(correlationMatrix$Value), decreasing = TRUE),]

# nr of features
num.features <- 30

# choose features in order from most important to least important
for (i in 1:num.features) {
  vec <- numeric() 
  
  for (j in 1:i) {
    vec[j] <- correlationMatrix[j,1] 
  }
  
  vec <- subset(vec, is.na(vec) == FALSE)
  # add vector to list
  feature.selection.list[[length(feature.selection.list) + 1]] <- vec
}

# remove residual variables
rm(vec)
rm(correlationMatrix)
rm(num.features, i, j)


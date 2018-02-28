# matrix with the corresponding Pearson Correlation values
correlationMatrix <- data.frame(ColumnNr = 3:30, Value = cor(data[,3:30], data[,2]))

# sort correlation matrix
correlationMatrix <- correlationMatrix[order(abs(correlationMatrix$Value), decreasing = TRUE),]

# create a matrix with the chosen columns by the correlation
mylist <- list() 

for (i in 1:20) {
  vec <- numeric(20) 
  
  for (j in 20:i) {
      vec[j] <- 0
  }
  
  for (j in 1:i) {
    vec[j] <- correlationMatrix[j,1] 
  }
  
  # add vector to matrix
  mylist[[i]] <- vec
}

# make dataframe from matrix
df <- do.call("rbind",mylist)

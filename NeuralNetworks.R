# Neural Networks

library(neuralnet)
library(nnet)
library(ggplot2)

best.acc <- 0
best.iter <- 0
all.acc <- vector()

iter <- 27
for(j in 1:25) {
  # formula
  f <- as.formula(paste("lowStress + moderateStress + highStress ~", paste(column.names[a.feature.list[[iter]]][!column.names[a.feature.list[[iter]]] %in% "C1Stress"], collapse = " + ")))
  
  # data
  d <- datasets.list[[1]]
  d <- d[[1]]
  d <- dataSplit(d, d$C1Stress, 0.7)
  
  train <- d[[1]]
  train <- cbind(train[, 3:30], class.ind(train$C1Stress))
  test <- d[[2]]
  
  
  # fit the model
  nn <- neuralnet(f,
                  data = train,
                  hidden = 10,
                  linear.output = F)
  
  a <- compute(nn, test[,a.feature.list[[iter]]])$net.result
  
  b <- vector()
  for(i in 1:nrow(test)) {
    
    if(a[i,1] > a[i,2] && a[i,1] > a[i,3]) {
      b[i] <- 'lowStress'
    }
    
    if(a[i,2] > a[i,1] && a[i,2] > a[i,3]) {
      b[i] <- 'moderateStress'
    }
    
    if(a[i,3] > a[i,2] && a[i,3] > a[i,2]) {
      b[i] <- 'highStress'
    }
  }
  
  acc <- mean(test$C1Stress == b) 
  all.acc[j] <- acc
}

print(mean(all.acc))
plot(nn)

# Plotting and interpretting results

# run results binder
#source('ResultsBinder.R')


# libraries
library(ggplot2)


results.dataset <- all.results
# levels(results.dataset$Algorithm) = c('SVM', 'KMC', 'DT', 'RF', 'NB')

avgSVM <- mean(results.dataset$Average.acc[results.dataset$Algorithm == 'SVM'])
avgRF <- mean(results.dataset$Average.acc[results.dataset$Algorithm == 'RF'])
avgDT <- mean(results.dataset$Average.acc[results.dataset$Algorithm == 'DT'])
avgNB <- mean(results.dataset$Average.acc[results.dataset$Algorithm == 'NB'])
avgNN <- mean(results.dataset$Average.acc[results.dataset$Algorithm == 'NN'])

# plot algorithms on accuracy
pl.alg.acc.boxplot <- ggplot(data = results.dataset, aes(x = reorder(Algorithm, Average.acc, FUN = mean), y = Average.acc)) +
  geom_boxplot(aes(fill = Algorithm), alpha = 0.5)

# plot dataset against algorithm with respect to accuracy
pl.dataset.alg.points <- ggplot(data = results.dataset, aes(x = Algorithm, y = Dataset)) + 
  geom_point(size = 4, aes(color = Average.acc)) +
  scale_color_continuous(low = 'red', high = 'green')

# plot acc on algorithms from low to high
pl.acc.alg.hist <- ggplot(data = results.dataset, aes(x = Average.acc)) + 
  geom_histogram(aes(fill = Algorithm), color = 'black')

# plot acc on datasets from low to high, a bit confusing
pl.acc.dataset.hist <- ggplot(data = results.dataset, aes(x = Average.acc)) + 
  geom_histogram(aes(fill = Dataset), color = 'black')



# this dataset has only 3 labels for datasets: normalized, scaled, simple
results.dataset.normalized <- results.dataset
{
  results.dataset.normalized$Dataset <- as.character(results.dataset.normalized$Dataset)
  results.dataset.normalized$Dataset[regexpr('Normalized', results.dataset$Dataset) > 0] <- 'Normalized'
  results.dataset.normalized$Dataset[regexpr('Scaled', results.dataset$Dataset) > 0] <- 'Scaled'
  results.dataset.normalized$Dataset[regexpr('Normalized', results.dataset$Dataset) == -1 &
                                       regexpr('Scaled', results.dataset$Dataset) == -1] <- 'Simple'
  results.dataset.normalized$Dataset <- as.factor(results.dataset.normalized$Dataset)
  
  # average acc for each type of data
  # average for each normalized algorithm
  avg.acc.normalized.SVM <- mean(results.dataset.normalized$Average.acc[results.dataset.normalized$Dataset == 'Normalized' & 
                                                                      results.dataset.normalized$Algorithm == 'SVM'])
  avg.acc.normalized.NB <- mean(results.dataset.normalized$Average.acc[results.dataset.normalized$Dataset == 'Normalized' & 
                                                                      results.dataset.normalized$Algorithm == 'NB'])
  avg.acc.normalized.NN <- mean(results.dataset.normalized$Average.acc[results.dataset.normalized$Dataset == 'Normalized' & 
                                                                      results.dataset.normalized$Algorithm == 'NN'])
  avg.acc.normalized.DT <- mean(results.dataset.normalized$Average.acc[results.dataset.normalized$Dataset == 'Normalized' & 
                                                                      results.dataset.normalized$Algorithm == 'DT'])
  avg.acc.normalized.RF <- mean(results.dataset.normalized$Average.acc[results.dataset.normalized$Dataset == 'Normalized' & 
                                                                      results.dataset.normalized$Algorithm == 'RF'])
  
  # average for each scaled algorithm
  avg.acc.scaled.SVM <- mean(results.dataset.normalized$Average.acc[results.dataset.normalized$Dataset == 'Scaled' & 
                                                                         results.dataset.normalized$Algorithm == 'SVM'])
  avg.acc.scaled.NB <- mean(results.dataset.normalized$Average.acc[results.dataset.normalized$Dataset == 'Scaled' & 
                                                                         results.dataset.normalized$Algorithm == 'NB'])
  avg.acc.scaled.NN <- mean(results.dataset.normalized$Average.acc[results.dataset.normalized$Dataset == 'Scaled' & 
                                                                         results.dataset.normalized$Algorithm == 'NN'])
  avg.acc.scaled.DT <- mean(results.dataset.normalized$Average.acc[results.dataset.normalized$Dataset == 'Scaled' & 
                                                                         results.dataset.normalized$Algorithm == 'DT'])
  avg.acc.scaled.RF <- mean(results.dataset.normalized$Average.acc[results.dataset.normalized$Dataset == 'Scaled' & 
                                                                         results.dataset.normalized$Algorithm == 'RF'])
  
  # average for each scaled algorithm
  avg.acc.simple.SVM <- mean(results.dataset.normalized$Average.acc[results.dataset.normalized$Dataset == 'Simple' & 
                                                                      results.dataset.normalized$Algorithm == 'SVM'])
  avg.acc.simple.NB <- mean(results.dataset.normalized$Average.acc[results.dataset.normalized$Dataset == 'Simple' & 
                                                                     results.dataset.normalized$Algorithm == 'NB'])
  avg.acc.simple.NN <- mean(results.dataset.normalized$Average.acc[results.dataset.normalized$Dataset == 'Simple' & 
                                                                     results.dataset.normalized$Algorithm == 'NN'])
  avg.acc.simple.DT <- mean(results.dataset.normalized$Average.acc[results.dataset.normalized$Dataset == 'Simple' & 
                                                                     results.dataset.normalized$Algorithm == 'DT'])
  avg.acc.simple.RF <- mean(results.dataset.normalized$Average.acc[results.dataset.normalized$Dataset == 'Simple' & 
                                                                     results.dataset.normalized$Algorithm == 'RF'])
  
}

# plot all datasets colored by simple vs scaled vs normalized
pl.acc.dataset.norm.hist <- ggplot(data = results.dataset.normalized, aes(x = Average.acc)) + 
  geom_histogram(aes(fill = Dataset), color = 'black')

# plot simple vs scaled vs normalized data
pl.norm.boxplot <- ggplot(data = results.dataset.normalized, aes(x = reorder(Dataset, Average.acc, FUN = mean), y = Average.acc, fill = Dataset)) + 
  geom_boxplot(alpha = 0.5)


# plot for each algorithm simple vs norm vs scaled
data <- results.dataset.normalized[results.dataset.normalized$Algorithm == 'SVM',]
pl.SVM.type.barplot <- ggplot(data = data, aes(x = reorder(Dataset, Average.acc, FUN = median), y = Average.acc)) + 
  geom_boxplot(color = "black", aes(fill = Dataset), alpha = 0.3)

data <- results.dataset.normalized[results.dataset.normalized$Algorithm == 'DT',]
pl.DT.type.barplot <- ggplot(data = data, aes(x = reorder(Dataset, Average.acc, FUN = median), y = Average.acc)) + 
  geom_boxplot(color = "black", aes(fill = Dataset), alpha = 0.3)

data <- results.dataset.normalized[results.dataset.normalized$Algorithm == 'RF',]
pl.RF.type.barplot <- ggplot(data = data, aes(x = reorder(Dataset, Average.acc, FUN = median), y = Average.acc)) + 
  geom_boxplot(color = "black", aes(fill = Dataset), alpha = 0.3)

data <- results.dataset.normalized[results.dataset.normalized$Algorithm == 'NB',]
pl.NB.type.barplot <- ggplot(data = data, aes(x = reorder(Dataset, Average.acc, FUN = median), y = Average.acc)) + 
  geom_boxplot(color = "black", aes(fill = Dataset), alpha = 0.3)


# this dataset ignores the simple/norm/scaled labels
results.dataset.method <- results.dataset
{
results.dataset.method$Dataset <- as.character(results.dataset.method$Dataset)

results.dataset.method$Dataset[regexpr('Simple', results.dataset.method$Dataset) > 0] <- 'Simple'
results.dataset.method$Dataset[regexpr('Oversampled', results.dataset.method$Dataset) > 0] <- 'Oversampled'
results.dataset.method$Dataset[regexpr('Undersampled', results.dataset.method$Dataset) > 0] <- 'Undersampled'
results.dataset.method$Dataset[regexpr('Hybrid', results.dataset.method$Dataset) > 0] <- 'Hybrid'
results.dataset.method$Dataset[regexpr('C1Stress', results.dataset.method$Dataset) > 0] <- 'C1Stress'
results.dataset.method$Dataset[regexpr('Rose', results.dataset.method$Dataset) > 0] <- 'Rose'
results.dataset.method$Dataset[regexpr('Smote', results.dataset.method$Dataset) > 0] <- 'Smote'

results.dataset.method$Dataset <- as.factor(results.dataset.method$Dataset)

# average acc for each type of data
avg.acc.simple <- mean(results.dataset.method$Average.acc[results.dataset.method$Dataset == 'Simple'])
avg.acc.oversampled <- mean(results.dataset.method$Average.acc[results.dataset.method$Dataset == 'Oversampled'])
avg.acc.undersampled <- mean(results.dataset.method$Average.acc[results.dataset.method$Dataset == 'Undersampled'])
avg.acc.hybrid <- mean(results.dataset.method$Average.acc[results.dataset.method$Dataset == 'Hybrid'])
avg.acc.c1Stress <- mean(results.dataset.method$Average.acc[results.dataset.method$Dataset == 'C1Stress'])
avg.acc.rose <- mean(results.dataset.method$Average.acc[results.dataset.method$Dataset == 'Rose'])
avg.acc.smote <- mean(results.dataset.method$Average.acc[results.dataset.method$Dataset == 'Smote'])
}

# plot datasets ignoring simple/norm/scaled labels
pl.method.boxplot <- ggplot(data = results.dataset.method, aes(x = reorder(Dataset, Average.acc, FUN = median), y = Average.acc, fill = Dataset)) + 
  geom_boxplot(alpha = 0.5)

# plot dataset types
pl.alg.method.barplot <- ggplot(results.dataset.method, aes(Algorithm, Average.acc)) + 
  geom_bar(color = "black", stat="identity", aes(fill = Dataset), position = "dodge")

# plot for each algorithm simple vs norm vs scaled
data <- results.dataset.method[results.dataset.method$Algorithm == 'SVM',]
pl.SVM.method.barplot <- ggplot(data = data, aes(x = reorder(Dataset, Average.acc, FUN = median), y = Average.acc)) + 
  geom_boxplot(color = "black", aes(fill = Dataset), alpha = 0.3)

data <- results.dataset.method[results.dataset.method$Algorithm == 'DT',]
pl.DT.method.barplot <- ggplot(data = data, aes(x = reorder(Dataset, Average.acc, FUN = median), y = Average.acc)) + 
  geom_boxplot(color = "black", aes(fill = Dataset), alpha = 0.3)

data <- results.dataset.method[results.dataset.method$Algorithm == 'RF',]
pl.RF.method.barplot <- ggplot(data = data, aes(x = reorder(Dataset, Average.acc, FUN = median), y = Average.acc)) + 
  geom_boxplot(color = "black", aes(fill = Dataset), alpha = 0.3)

data <- results.dataset.method[results.dataset.method$Algorithm == 'NB',]
pl.NB.method.barplot <- ggplot(data = data, aes(x = reorder(Dataset, Average.acc, FUN = median), y = Average.acc)) + 
  geom_boxplot(color = "black", aes(fill = Dataset), alpha = 0.3)




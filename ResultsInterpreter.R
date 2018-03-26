# Plotting and interpretting results

# run results binder
source('ResultsBinder.R')


# libraries
library(ggplot2)


results.dataset <- all.results
levels(results.dataset$Algorithm) = c('SVM', 'KMC', 'DT', 'RF', 'NB')


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
  avg.acc.normalized <- mean(results.dataset.normalized$Average.acc[results.dataset.normalized$Dataset == 'Normalized'])
  avg.acc.scaled <- mean(results.dataset.normalized$Average.acc[results.dataset.normalized$Dataset == 'Scaled'])
  avg.acc.simple <- mean(results.dataset.normalized$Average.acc[results.dataset.normalized$Dataset == 'Simple'])
}

# plot all datasets colored by simple vs scaled vs normalized
pl.acc.dataset.norm.hist <- ggplot(data = results.dataset.normalized, aes(x = Average.acc)) + 
  geom_histogram(aes(fill = Dataset), color = 'black')

# plot simple vs scaled vs normalized data
pl.norm.boxplot <- ggplot(data = results.dataset.normalized, aes(x = reorder(Dataset, Average.acc, FUN = median), y = Average.acc, fill = Dataset)) + 
  geom_boxplot(alpha = 0.5)


# this dataset ignores the simple/norm/scaled labels
results.dataset.method <- results.dataset
{
results.dataset.method$Dataset <- as.character(results.dataset.method$Dataset)

results.dataset.method$Dataset[regexpr('simple', results.dataset.method$Dataset) > 0] <- 'Simple'
results.dataset.method$Dataset[regexpr('Oversampled', results.dataset.method$Dataset) > 0] <- 'Oversampled'
results.dataset.method$Dataset[regexpr('Undersampled', results.dataset.method$Dataset) > 0] <- 'Undersampled'
results.dataset.method$Dataset[regexpr('Hybrid', results.dataset.method$Dataset) > 0] <- 'Hybrid'
results.dataset.method$Dataset[regexpr('C1Stress', results.dataset.method$Dataset) > 0] <- 'C1Stress'
results.dataset.method$Dataset[regexpr('Rose', results.dataset.method$Dataset) > 0] <- 'Rose'
results.dataset.method$Dataset[regexpr('Smote', results.dataset.method$Dataset) > 0] <- 'Smote'

results.dataset.method$Dataset <- as.factor(results.dataset.method$Dataset)
}

# plot datasets ignoring simple/norm/scaled labels
pl.method.boxplot <- ggplot(data = results.dataset.method, aes(x = reorder(Dataset, Average.acc, FUN = median), y = Average.acc, fill = Dataset)) + 
  geom_boxplot(alpha = 0.5)







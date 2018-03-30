
# data frame containing the results
all.results <- data.frame((matrix(ncol = 4, nrow = 0)))
colnames(all.results) <- c("Algorithm",
                           "Dataset",
                           "Average.acc",
                           "Formula")
# add SVM to results
SVM.df <- read.csv(file = "SupportVectorMachinesResults.csv", header = T)
all.results <- rbind(all.results, SVM.df[c(2, 3, 6, 7)])

#add k means to results
#KMeansClustering.df <- read.csv(file = "KMeansClusteringResults.csv", header = T)
#all.results <- rbind(all.results, KMeansClustering.df[2:5])

# add linear regression to results
#LinearRegression.df <- read.csv(file = "LinearRegressionResults.csv", header = T)
#all.results <- rbind(all.results, LinearRegression.df[c(2, 3, 8, 9)])

# add logistic regression to results
#LogisticRegression.df <- read.csv(file = "LogisticRegressionResults.csv", header = T)
#all.results <- rbind(all.results, LogisticRegression.df[c(2, 3, 8, 9)])

# add decision tree to results
DecisionTree.df <- read.csv(file = "DecisionTreeResults.csv", header = T)
all.results <- rbind(all.results, DecisionTree.df[c(2, 3, 8, 9)])

# add random forest to results
RandomForest.df <- read.csv(file = "RandomForestResults.csv", header = T)
all.results <- rbind(all.results, RandomForest.df[c(2, 3, 8, 9)])

# add naive bayes to results
NaiveBayes.df <- read.csv(file = "NaiveBayesResults.csv", header = T)
all.results <- rbind(all.results, NaiveBayes.df[c(2, 3, 8, 9)])

# output a csv file
write.csv(all.results, file = "AllResults.csv")

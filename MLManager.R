# ML Manager

# get datasets
source('DataManager.R')

# get feature selection
source('FeatureSelection.R')

# list of ML algorithms
all.results <- data.frame((matrix(ncol = 4, nrow = 0)))
colnames(all.results) <- c("Algorithm",
                           "Dataset",
                           "Average.acc",
                           "Formula")
# SVM
source("Support Vector Machines.R")
MLSVM(a.datasets.list = datasets.list, 
      a.feature.list = feature.selection.list, 
      a.k = 5)

# KMeans
source("KMeansClustering.R")
MLKmeansClustering(a.datasets.list = datasets.list, 
                   a.feature.list = feature.selection.list, 
                   a.num.iter = 10)

# Linear Regression
source("Linear Regression.R")
MLLinearRegression(a.datasets.list = datasets.list, 
                   a.feature.list = feature.selection.list, 
                   a.num.iter = 5, 
                   a.k = 5)

# Logistic Regression
source("Logistic Regression.R")
MLLogisticRegression(a.datasets.list = datasets.list, 
                     a.feature.list = feature.selection.list, 
                     a.num.iter = 5, 
                     a.k = 5)

# Decision Tree
source("Decision Tree.R")
MLDecisionTree(a.datasets.list = datasets.list, 
               a.feature.list = feature.selection.list, 
               a.num.iter = 5, 
               a.k = 5)

# Random Forest
source("Random Forest.R")
MLRandomForest(a.datasets.list = datasets.list, 
               a.feature.list = feature.selection.list, 
               a.num.iter = 5, 
               a.k = 5)

# Naive Bayes
source("Naive Bayes.R")
MLNaiveBayes(a.datasets.list = datasets.list, 
             a.feature.list = feature.selection.list, 
             a.num.iter = 5, 
             a.k = 5)

# output a csv file
write.csv(all.results, file = "AllResults.csv")

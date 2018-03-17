# ML Manager

# get datasets
source('DataManager.R')

# get feature selection
source('FeatureSelection.R')

# list of ML algorithms

# SVM
source("Support Vector Machines.R")
MLSVM(datasets.list = datasets.list, feature.selection.list = feature.selection.list[1:10], k = 5)

# KMeans
source("KMeansClustering.R")
MLKmeansClustering(dataset.lists = datasets.list, feature.selection.list = feature.selection.list, num.iter = 500)

# Linear Regression
source("Linear Regression.R")
MLLinearRegression(datasets.list = datasets.list, feature.selection.list = feature.selection.list, num.runs = 5, k = 5)

# Logistic Regression
source("Logistic Regression.R")
MLLogisticRegression(datasets.list = datasets.list, feature.selection.list = feature.selection.list, num.runs = 5, k = 5)

# Decision Tree
source("Decision Tree.R")
MLDecisionTree(datasets.list = datasets.list, feature.selection.list = feature.selection.list, num.runs = 5, k = 5)

# Random Forest
source("Random Forest.R")
MLRandomForest(datasets.list = datasets.list, feature.selection.list = feature.selection.list, num.runs = 5, k = 5)

# Random Forest
source("Naive Bayes.R")
MLNaiveBayes(datasets.list = datasets.list, feature.selection.list = feature.selection.list, num.runs = 5, k = 5)





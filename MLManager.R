# ML Manager

# get datasets
source('DataManager.R')

# get feature selection
source('FeatureSelection.R')

# list of ML algorithms

# SVM
MLSVM(datasets.list = datasets.list, feature.selection.list = feature.selection.list[1:1], k = 5)

# KMeans
MLKmeansClustering(dataset.lists = datasets.list, feature.selection.list = feature.selection.list, num.iter = 500)

# Linear Regression
MLLinearRegression(datasets.list = datasets.list, feature.selection.list = feature.selection.list, num.runs = 5, k = 5)

# Logistic Regression
MLLogisticRegression(datasets.list = datasets.list, feature.selection.list = feature.selection.list, num.runs = 5, k = 5)

# Decision Tree
MLDecisionTree(datasets.list = datasets.list, feature.selection.list = feature.selection.list, num.runs = 5, k = 5)

# Random Forest
MLRandomForest(datasets.list = datasets.list, feature.selection.list = feature.selection.list, num.runs = 5, k = 5)

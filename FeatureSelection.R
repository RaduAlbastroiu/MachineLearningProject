# Main Feature Selection Script

# create list of features
feature.selection.list = list()

# filter methods
source('FeatureSelectionFilter.R')

# wrapper methods
source('FeatureSelectionWrapper.R')

# keep unique elements only
feature.selection.list <- unique(feature.selection.list)

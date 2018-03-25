# Main Feature Selection Script

# create list of features
feature.selection.list = list()

# filter methods
source('FeatureSelectionFilter.R')

# wrapper methods
source('FeatureSelectionWrapper.R')

# embedded methods
source('FeatureSelectionEmbedded.R')

# sort values to eliminate duplicates
for(i in 1:length(feature.selection.list)) {
  feature.selection.list[[i]] <- sort(feature.selection.list[[i]])
}

# keep unique elements only
feature.selection.list <- unique(feature.selection.list)

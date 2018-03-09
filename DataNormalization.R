# Data normalization

# Normalize data
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)

# normalized data
scaled.data <- scale(data, center = mins, scale = maxs - mins)
scaled <- as.data.frame(scaled.data)

nums <- sapply(data, is.numeric)
data[,nums == TRUE]
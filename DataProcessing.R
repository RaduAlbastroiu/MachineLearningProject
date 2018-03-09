# Filter data

# Filter average excess of distance 
data <- subset(data, averageExcessOfDistanceBetweenClicks < 25 & decision_time_efficiency > 10000)

# simple data
simple.data <- data

# oversampling

# undersampling data
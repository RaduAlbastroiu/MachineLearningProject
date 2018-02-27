# Filter data

detach(data)

# Filter average excess of distance 
data <- subset(data, averageExcessOfDistanceBetweenClicks < 25 & decision_time_efficiency > 10000)

# Attach data to memory
attach(data)

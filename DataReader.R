# read data
data <- read.table(file.choose(), sep = ",", header = TRUE)

columnNames <- data[0,]

selData <- data[,1:30]

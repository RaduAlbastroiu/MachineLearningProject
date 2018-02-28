# read data
data <- read.table(file.choose(), sep = ",", header = TRUE)

columnNames <- colnames(data)

selData <- data[,1:30]

# Data reader

# read data
primary.data <- read.table(file.choose(), sep = ",", header = TRUE)

# don't touch resources
data <- primary.data
column.names <- colnames(primary.data)


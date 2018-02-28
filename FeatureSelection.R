#prepare data
detach(data)
selData <- data[,1:30]
attach(selData)

FitAll = lm(PSS_Score ~ ., data = selData)
FitStart = lm(PSS_Score ~ 1, data = selData)

#forward selection
step(FitStart, direction = "forward", scope = formula(FitAll))

#backward elimination
step(FitAll, direction = "backward")

#stepwise regression
step(FitStart, direction = "both", scope = formula(FitAll))
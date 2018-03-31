# Ensemble Machine Learning
# Stacking

#install.packages("nnls")
#install.packages("quadprog")
#install.packages("SuperLearner")

library("SuperLearner")

d <- datasets.list[[1]]
d <- d[[1]]

levels(d$C1Stress) <- c(1,2,3)
d$C1Stress <- as.numeric(d$C1Stress)

d <- dataSplit(d, d$C1Stress, 0.7)
train <- d[[1]]
test <- d[[2]]

x.train <- train[3:30]
y.train <- train$C1Stress
x.test <- train[3:30]
y.test <- test$C1Stress

#fitSL <- SuperLearner(Y = train$C1Stress, X = train,
#                      SL.library = c('SL.glm'), family = gaussian(),
#                      method = 'method.NNLS', verbose = TRUE ,
#                      cvControl = list(V = 10))

sl = SuperLearner(Y = y.train, X = x.train, family = binomial(),
                  SL.library = c("SL.mean", "SL.glmnet", "SL.randomForest"))

res <- predict(sl, x.test, onlySL = T)

acc <- mean(res == y.test)



##########################
# credits to:
# https://cran.r-project.org/web/packages/SuperLearner/vignettes/Guide-to-SuperLearner.html

# Extract our outcome variable from the dataframe.
outcome = Boston$medv

# Create a dataframe to contain our explanatory variables.
data = subset(Boston, select = -medv)

# Check structure of our dataframe.
str(data)

# Set a seed for reproducibility in this random sampling.
set.seed(1)

# Reduce to a dataset of 150 observations to speed up model fitting.
train_obs = sample(nrow(data), 150)

# X is our training sample.
X_train = data[train_obs, ]

# Create a holdout set for evaluating model performance.
# Note: cross-validation is even better than a single holdout sample.
X_holdout = data[-train_obs, ]

# Create a binary outcome variable: towns in which median home value is > 22,000.
outcome_bin = as.numeric(outcome > 22)

Y_train = outcome_bin[train_obs]
Y_holdout = outcome_bin[-train_obs]

# Review the outcome variable distribution.
table(Y_train, useNA = "ifany")



# Fit one model
# Set the seed for reproducibility.
set.seed(1)

# Fit lasso model.
sl_lasso = SuperLearner(Y = Y_train, X = X_train, family = binomial(),
                        SL.library = "SL.glmnet")

# Review the elements in the SuperLearner object.
names(sl_lasso)



# Fit Multiple Models
set.seed(1)
sl = SuperLearner(Y = Y_train, X = X_train, family = binomial(),
                  SL.library = c("SL.mean", "SL.glmnet", "SL.randomForest"))

# Predict
# Predict back on the holdout dataset.
# onlySL is set to TRUE so we don't fit algorithms that had weight = 0, saving computation.
pred = predict(sl, X_holdout, onlySL = T)

# Review AUC - Area Under Curve
pred_rocr = ROCR::prediction(pred$pred, Y_holdout)
auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
auc





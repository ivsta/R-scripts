#-------------------------------------------------
# http://dmlc.ml/rstats/2016/03/10/xgboost.html
#-------------------------------------------------

library(xgboost)

#----------------
# Beginner's guide
#----------------

# Load data
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test

# Train model
model <- xgboost(data = train$data, label = train$label, nrounds = 2, objective = "binary:logistic")

# Predict test set
preds <- predict(model, test$data)

# Cross-validation
cv.res <- xgb.cv(data = train$data, label = train$label, nfold = 5, nrounds = 2, objective = "binary:logistic")


#----------------
# Convenient interface
#----------------

# Customised objective
loglossobj <- function(preds, dtrain) {
  # dtrain is the internal format of the training data
  # We extract the labels from the training data
  labels <- getinfo(dtrain, "label")
  # We compute the 1st and 2nd gradient, as grad and hess
  preds <- 1/(1 + exp(-preds))
  grad <- preds - labels
  hess <- preds * (1 - preds)
  # Return the result as a list
  return(list(grad = grad, hess = hess))
}

model <- xgboost(data = train$data, label = train$label, nrounds = 2, objective = loglossobj, eval_metric = "error")


# Early stopping
bst <- xgb.cv(data = train$data, label = train$label, nfold = 5,
              nrounds = 20, objective = "binary:logistic",
              early.stop.round = 3, maximize = FALSE)


# Continue training
dtrain <- xgb.DMatrix(train$data, label = train$label)
model <- xgboost(data = dtrain, nrounds = 2, objective = "binary:logistic")
pred_train <- predict(model, dtrain, outputmargin=TRUE)

setinfo(dtrain, "base_margin", pred_train)
model <- xgboost(data = dtrain, nrounds = 2, objective = "binary:logistic")


# Handle missing values
dat <- matrix(rnorm(128), 64, 2)
label <- sample(0:1, nrow(dat), replace = TRUE)
for (i in 1:nrow(dat)) {
  ind <- sample(2, 1)
  dat[i, ind] <- NA
}

model <- xgboost(data = dat, label = label, missing = NA, nrounds = 2, objective = "binary:logistic")

#----------------------
# Model inspection
#----------------------
bst <- xgboost(data = train$data, label = train$label, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic")
xgb.plot.tree(feature_names = agaricus.train$data@Dimnames[[2]], model = bst)

# Multiple-in-one plot
bst <- xgboost(data = train$data, label = train$label, max.depth = 15,
               eta = 1, nthread = 2, nround = 30, objective = "binary:logistic",
               min_child_weight = 50)
xgb.plot.multi.trees(model = bst, feature_names = agaricus.train$data@Dimnames[[2]], features.keep = 3)

# Feature importance
bst <- xgboost(data = train$data, label = train$label, max.depth = 2,
               eta = 1, nthread = 2, nround = 2,objective = "binary:logistic")
importance_matrix <- xgb.importance(agaricus.train$data@Dimnames[[2]], model = bst)
xgb.plot.importance(importance_matrix)

# Deepness
bst <- xgboost(data = train$data, label = train$label, max.depth = 15,
               eta = 1, nthread = 2, nround = 30, objective = "binary:logistic",
               min_child_weight = 50)
xgb.plot.deepness(model = bst)
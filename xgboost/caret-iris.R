# https://github.com/dmlc/xgboost/issues/1607

library(dplyr)
library(caret)

# Data Prep
iris2 <- iris %>%
  filter(Species %in% c('setosa' , 'virginica'))
iris2$Species <- as.factor(as.character(iris2$Species))
iris2$Petal.Width <- ifelse(iris2$Petal.Width == 0.2, NA, iris2$Petal.Width)

set.seed(12)
trainIndex <- createDataPartition(iris2$Species, p = .8, list = FALSE, times = 1)
irisTrain <- iris2[ trainIndex,]
irisTest  <- iris2[-trainIndex,]

# Training
library(xgboost)
fitControl <- trainControl(
  method = "cv",
  number = 10,
  repeats = 1,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  allowParallel = TRUE)

xgbGrid <-  expand.grid(
  nrounds = c(2),
  max_depth = c(3),
  eta = c(0.1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1)

xgbModel <- train(
  Species ~ .,
  data = irisTrain,
  preProc = c("center", "scale"),
  method = "xgbTree",
  metric = "ROC",
  trControl = fitControl,
  tuneGrid = xgbGrid,
  verbose = 2,
  na.action = na.pass)

# Prediction
pred <- predict(xgbModel, irisTest, na.action = na.pass)
confusionMatrix(pred, irisTest$Species)
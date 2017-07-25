library(dplyr)
library(caret)
library(doParallel)		# parallel processing
library(pROC)
library(classifierplots)
library(Ckmeans.1d.dp)
library(tidyverse)
library(tabplot)
library(ggthemes)


# Set up to do parallel processing   
registerDoParallel(detectCores())		# Registrer a parallel backend for train
getDoParWorkers()

#-----------------------
# Load data
#-----------------------

data(segmentationData)
segmentationData <- segmentationData %>% as_data_frame()

segmentationData %>% dim()
# [1] 2019   61

segmentationData %>% glimpse()

segmentationData %>% str()


#-----------------------
# Split training / test
#-----------------------

trainIndex <- segmentationData$Case %>%
  createDataPartition(p = 0.5, list = FALSE)

trainData <- segmentationData %>% slice(trainIndex) %>% select(-Cell, -Case)
testData <- segmentationData %>% slice(-trainIndex) %>% select(-Cell, -Case)

trainX <- trainData %>% select(-Class)        # Pull out the dependent variable
testX <- testData %>% select(-Class)

trainX %>% glimpse()
trainX %>% summary()
trainX %>% tableplot()


#-------------------------
# Set up training control
#-------------------------

ctrl <- trainControl(method = "repeatedcv"    # 10fold cross validation
                     , number = 5							# do 5 repititions of cv
                     , summaryFunction = twoClassSummary # Use AUC to pick the best model
                     , classProbs = TRUE
                     , allowParallel = TRUE)


#-------------------------
# Set up hyperparameter grid
#-------------------------

xgb.grid <- expand.grid(nrounds = c(400, 500, 600) # the maximum number of iterations
                        , eta = c(0.01, 0.1) # shrinkage
                        , max_depth = c(2, 5, 15)
                        , colsample_bytree = 0.8
                        , min_child_weight = 10
                        , gamma = 1
                        , subsample = 1)


#-------------------------
# Train xgboost
#-------------------------

set.seed(666)
xgb.tune <-train(x = trainX
                 , y = trainData$Class
                 , method = "xgbTree"
                 , metric = "ROC"
                 , trControl = ctrl
                 , tuneGrid = xgb.grid)


xgb.tune$bestTune
plot(xgb.tune)  		# Plot the performance of the training models
(res <- xgb.tune$results)


#-----------------------------------------
# Make predictions using the test data set
#-----------------------------------------

xgb.pred <- xgb.tune %>% predict(testX)


#-----------------------------------------
# Assess test results
#-----------------------------------------

# Look at the confusion matrix  
xgb.pred %>% confusionMatrix(testData$Class)   

#Draw the ROC curve 
xgb.probs <- xgb.tune %>% predict(testX, type = "prob")
head(xgb.probs)

xgb.ROC <- roc(predictor = xgb.probs$PS
               , response = testData$Class
               , levels = rev(levels(testData$Class)))
xgb.ROC$auc
# Area under the curve: 0.8888

plot(xgb.ROC, main = "xgboost ROC")

# Plot the propability of poor segmentation
histogram(~ xgb.probs$PS|testData$Class
          , xlab = "Probability of Poor Segmentation")


#-----------------------------------------
# classifierplots
#-----------------------------------------

testData <- testData %>%
  mutate(label = case_when(Class == 'PS' ~ 1, Class == 'WS' ~ 0))

classifierplots(test.y = testData$label, pred.prob = xgb.probs$PS)


#-----------------------------------------
# Plot variable importance
#-----------------------------------------

var_imp <- xgb.importance(feature_names = names(testX)
                          , model = xgb.tune$finalModel)

xgb.ggplot.importance(importance_matrix = var_imp, rel_to_first = TRUE)

xgb.plot.importance(importance_matrix = var_imp
                    , top_n = 20, rel_to_first = TRUE)


#-----------------------------------------
# Plot tree
#-----------------------------------------

xgb_tree <- xgb.model.dt.tree(feature_names = names(testX)
                              , model = xgb.tune$finalModel
                              , text = NULL, n_first_tree = 1)

xgb.plot.tree(feature_names = names(testX)
              , model = xgb.tune$finalModel, n_first_tree = 0)



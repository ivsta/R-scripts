library(caret)
library(doParallel)		# parallel processing
library(pROC)
library(classifierplots)


### Get the Data
# Load the data and construct indices to divied it into training and test data sets.
data(segmentationData)  	# Load the segmentation data set
dim(segmentationData)
head(segmentationData, 2)

trainIndex <- createDataPartition(segmentationData$Case, p = 0.5, list = FALSE)
trainData <- segmentationData[trainIndex,-c(1,2)]
testData  <- segmentationData[-trainIndex,-c(1,2)]

trainX <-trainData[,-1]        # Pull out the dependent variable
testX <- testData[,-1]
sapply(trainX,summary) # Look at a summary of the training data


# Set up to do parallel processing   
registerDoParallel(detectCores())		# Registrer a parallel backend for train
getDoParWorkers()

set.seed(666)



# Set up training control
ctrl <- trainControl(method = "repeatedcv"    # 10fold cross validation
                     , number = 5							# do 5 repititions of cv
                     , summaryFunction = twoClassSummary # Use AUC to pick the best model
                     , classProbs = TRUE
                     , allowParallel = TRUE)


# Train xgboost
xgb.grid <- expand.grid(nrounds = 500 # the maximum number of iterations
                        , eta = c(0.01, 0.1) # shrinkage
                        , max_depth = c(2, 6, 10)
                        , colsample_bytree = 0.8
                        , min_child_weight = 10
                        , gamma = 1
                        , subsample = 1
)

xgb.tune <-train(x=trainX,y=trainData$Class
                 , method = "xgbTree"
                 , metric = "ROC"
                 , trControl = ctrl
                 , tuneGrid = xgb.grid)


xgb.tune$bestTune
plot(xgb.tune)  		# Plot the performance of the training models
res <- xgb.tune$results
res

### xgboostModel Predictions and Performance
# Make predictions using the test data set
xgb.pred <- predict(xgb.tune,testX)

#Look at the confusion matrix  
confusionMatrix(xgb.pred,testData$Class)   

#Draw the ROC curve 
xgb.probs <- predict(xgb.tune,testX,type="prob")
#head(xgb.probs)

xgb.ROC <- roc(predictor = xgb.probs$PS
               , response = testData$Class
               , levels = rev(levels(testData$Class)))
xgb.ROC$auc
# Area under the curve: 0.8888

plot(xgb.ROC,main="xgboost ROC")
# Plot the propability of poor segmentation
histogram(~xgb.probs$PS|testData$Class,xlab="Probability of Poor Segmentation")

setDT(testData)
testData[Class == 'PS', label := 1]
testData[Class == 'WS', label := 0]

classifierplots(test.y = testData$label, pred.prob = xgb.probs$PS)

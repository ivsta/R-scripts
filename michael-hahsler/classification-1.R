#' ---
#' title: "R Code for Chapter 4 of Introduction to Data Mining: Classification"
#' author: "Michael Hahsler"
#' output:
#'  html_document:
#'    toc: true
#' ---

#' This code covers chapter 4 of _"Introduction to Data Mining"_
#' by Pang-Ning Tan, Michael Steinbach and Vipin Kumar.
#'
#' ![CC](https://i.creativecommons.org/l/by/4.0/88x31.png)
#' This work is licensed under the
#' [Creative Commons Attribution 4.0 International License](http://creativecommons.org/licenses/by/4.0/). For questions please contact
#' [Michael Hahsler](http://michael.hahsler.net).
#'


#' Load Zoo data set
data(Zoo, package="mlbench")
head(Zoo)

#' make all the TRUE/FALSE values into factors (nominal)
for(i in c(1:12, 14:16)) Zoo[[i]] <- as.factor(Zoo[[i]])

#' Get summary statistics
summary(Zoo)



#' # Recursive partitioning (similar to CART)

library(rpart)
tree1 <- rpart(type ~ ., data=Zoo)
tree1

#' Plotting
library(rpart.plot)
rpart.plot(tree1)

#' Create a full tree (see: ?rpart.control)
tree2 <- rpart(type ~., data=Zoo, control=rpart.control(minsplit=2, cp=0))
rpart.plot(tree2)
tree2

# training error
head(predict(tree1, Zoo))
pred <- predict(tree1, Zoo, type="class")
head(pred)

confusion_table <- table(Zoo$type, pred)
confusion_table

correct <- sum(diag(confusion_table))
correct
error <- sum(confusion_table)-correct
error

accuracy <- correct / (correct+error)
accuracy

#' Use a function for accuracy
accuracy <- function(truth, prediction) {
  tbl <- table(truth, prediction)
  sum(diag(tbl))/sum(tbl)
}

accuracy(Zoo$type, pred)

#' Training error of the full tree
accuracy(Zoo$type, predict(tree2, Zoo, type="class"))

#' Get a confusion table with more statistics (using caret)
library(caret)
confusionMatrix(data = pred, reference = Zoo$type)

#' # Generalization Error
#' ## Use training and test set

n_train <- as.integer(nrow(Zoo)*.66)
train_id <- sample(1:nrow(Zoo), n_train)

train <- Zoo[train_id,]
test <- Zoo[-train_id, -17]
test_type <- Zoo[-train_id, 17]

tree1 <- rpart(type ~., data=train,control=rpart.control(minsplit=2))

#' Training error
accuracy(train$type, predict(tree1, train, type="class"))

#' Generalization error
accuracy(test_type, predict(tree1, test, type="class"))

#' ## 10-fold cross-validation

index <- 1:nrow(Zoo)
index <- sample(index) ### shuffle index
fold <- rep(1:10, each=nrow(Zoo)/10)[1:nrow(Zoo)]

folds <- split(index, fold) ### create list with indices for each fold

#' Do each fold
accs <- vector(mode="numeric")
for(i in 1:length(folds)) {
  tree <- rpart(type ~., data=Zoo[-folds[[i]],], control=rpart.control(minsplit=2, cp=0.01))
  accs[i] <- accuracy(Zoo[folds[[i]],]$type, predict(tree, Zoo[folds[[i]],], type="class"))
}
accs

#' Report the average
mean(accs)

#' # Use caret for easier model building and evaluation
#' see http://cran.r-project.org/web/packages/caret/vignettes/caret.pdf
#'
#' Use multi-core
library(doParallel)
registerDoParallel()

#' ## Cross-Validation
#' Evaluation with caret. Train tries to tune cp for rpart using accuracy to
#' chose the best model. Minsplit is set to 2 since we have not much data.
library(caret)
fit <- train(type ~ ., data = Zoo , method = "rpart",
             control=rpart.control(minsplit=2),
             trControl = trainControl(method = "cv", number = 10),
             tuneLength=5)
fit
#' Note that train has built 10 trees. A model using the best tuning parameters
#' and using all the data is available as `fit$finalModel`.

rpart.plot(fit$finalModel)

#' caret also computes variable importance. By default it uses competing splits
#' for rpart models
varImp(fit)
varImp(fit, compete = FALSE)
dotPlot(varImp(fit, compete=FALSE))
#' For Recursive Partitioning:  reduction in the loss function (e.g. mean squared error) attributed to each variable at each split is tabulated and the sum is returned.


#' An alternative to CV is repeated bootstrap sampling
fit <- train(type ~ ., data = Zoo, method = "rpart",
             control=rpart.control(minsplit=2),
             trControl = trainControl(method = "boot", number = 10),
             tuneLength=5)
fit

#' ## Do train/test sample
inTrain <- createDataPartition(y=Zoo$type, p = .75, list=FALSE)
training <- Zoo[ inTrain,]
testing <- Zoo[-inTrain,]

#' Find best model (trying more values for tuning)
fit <- train(type ~ ., data = training, method = "rpart",
             control=rpart.control(minsplit=2),
             trControl = trainControl(method = "cv", number = 10),
             tuneLength=20)
fit

plot(fit)

#' Use the best model on the test data
fit$finalModel
pred <- predict(fit, newdata = testing)
head(pred)

#' Confusion matrix (incl. confidence interval) on test data
confusionMatrix(data = pred, testing$type)

#' # Feature selection

#' Decision trees implicitly select features for splitting, but we can also
#' select features manually. This is usually done by looking at how related
#' each feature is to the class variable (e.g. using the chi-square statistic)

library(FSelector)
#' see: http://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Dimensionality_Reduction/Feature_Selection#The_Feature_Ranking_Approach

weights <- chi.squared(type ~ ., data=Zoo)
weights

#' Get the 5 best features
subset <- cutoff.k(weights, 5)
subset

#' Use only the best 5 features to build a model
f <- as.simple.formula(subset, "type")
f

m <- rpart(f, data=Zoo)
rpart.plot(m)


#' # Compare two models
library(caret)

#' Create fixed sampling scheme (10-folds)
train <- createFolds(Zoo$type,k=10)


#' Build models
rpartFit <- train(type ~ .,  data = Zoo, method = "rpart",
                  tuneLength = 10,
                  trControl = trainControl(
                    method = "cv", indexOut = train))

knnFit <- train(type ~ .,  data = Zoo, method = "knn",
                tuneLength = 10,
                trControl = trainControl(
                  method = "cv", indexOut = train))

#' Compare accuracy
resamps <- resamples(list(
  CART = rpartFit,
  kNearestNeighbors = knnFit
))
summary(resamps)

#' Plot the accuracy of the two models models for each resampling. If the
#' models are the same then all points will fall on the diagonal.
xyplot(resamps)
#'
#' Find out if one models is statistically better than the other (is
#' the difference in accuracy is not zero).
difs <- diff(resamps)
difs
summary(difs)
#' p-values tells you the probability of seeing an even more extreme value (difference between accuracy) given that the null hypothesis (difference = 0) is true. For a better classifier p-value should be less than .05 or 0.01. `diff` automatically applies Bonferoni correction for multiple testing. In this case, the classifiers do not perform statistically differently.
#'
#' # Dealing With the Class Imbalance Problem
#'
#' Decide if an animal is an reptile. First we change the class variable.
#' __Note:__ We use here the training data for testing. You should use a
#' separate testing data set!
Zoo_reptile <- Zoo
Zoo_reptile$type <- factor(Zoo$type == "reptile",
                           levels = c(FALSE, TRUE), labels =c("nonreptile", "reptile"))
#' Do not forget to make the class variable a factor (a nominal variable)
#' or you will get a regression tree instead of a classification tree.

summary(Zoo_reptile)

#' See if we have a class imbalance problem.
barplot(table(Zoo_reptile$type), xlab = "Reptile", ylab="Count")
#' the new class variable is clearly not balanced. This is a problem
#' for building a tree!

fit <- train(type ~ ., data=Zoo_reptile, method = "rpart",
             trControl = trainControl(method = "cv"))
fit
rpart.plot(fit$finalModel)
#' the tree predicts everything as non-reptile. Have a look at the error on
#' the training set.

confusionMatrix(data = predict(fit, Zoo_reptile),
                ref = Zoo_reptile$type, positive = "reptile")
#' Note that the accuracy is exactly the same as the no-information rate
#' and kappa is zero. Also, I set the positive class
#' or some measures will be incorrect
#'
#' ## Option 1: Balance Data With Resampling
#'
#' We use stradified sampling with replacement (to oversample the minority class).
#' You could also use SMOTE or other sampling strategies.
library(sampling)
id <- strata(Zoo_reptile, stratanames="type", size=c(50,50), method="srswr")
Zoo_reptile_balanced <- Zoo_reptile[id$ID_unit, ]
table(Zoo_reptile_balanced$type)

fit <- train(type ~ ., data = Zoo_reptile_balanced, method = "rpart",
             trControl = trainControl(method = "cv"),
             control = rpart.control(minsplit = 5))
fit
rpart.plot(fit$finalModel)

#' check on balanced training data
confusionMatrix(data = predict(fit, Zoo_reptile_balanced),
                ref = Zoo_reptile_balanced$type, positive = "reptile")

#' However, real data that we will make predictions for will not be balanced.
#' Check on original data with original class distribution.

confusionMatrix(data = predict(fit, Zoo_reptile),
                ref = Zoo_reptile$type, positive = "reptile")

#' __Note__ that the accuracy is below the no information rate! However,
#' you see that this model is able to find all reptiles, but also misclassifies
#' many non-reptiles as reptiles. The tradeoff can be controlled using the sample
#' proportions.
#'

#' ## Option 2: Build A Larger Tree and use Predicted Probabilities
#'
#' Increase complexity and require less data for splitting a node.
#' Here I also use AUC (ROC) as the tuning metric. You need to specify the two class
#' summary function. Note that the tree still trying to improve accuracy on the
#' data and not AUC! I also enable class probabilities since I want to predict
#' probabilities later.

fit <- train(type ~ ., data=Zoo_reptile, method = "rpart",
             tuneLength=20,
             trControl = trainControl(method = "cv",
                                      classProbs = TRUE,                 ## necessary for predict with type="prob"
                                      summaryFunction=twoClassSummary),  ## necessary for ROC
             metric = "ROC",
             control = rpart.control(minsplit = 5))
fit
rpart.plot(fit$finalModel)

#' predict with `type = "prob"` returns probabilities for each
#' class. These probabilities
#' are the result of the distribution of observations from the training
#' data in the leaf notes.
head(predict(fit, Zoo_reptile, type = "prob"))

confusionMatrix(data = predict(fit, Zoo_reptile),
                ref = Zoo_reptile$type, positive = "reptile")
#' __Note:__ the accuracy is high, but it is close to the no-information rate!
#'
#' ## Create A Biased Classifier
#' We can create a classifier which will detect more reptiles
#' at the expense of misclassifying non-reptiles. This is equivalent
#' to increasing the cost of misclassifying a reptile as a non-reptile.
#' The usual rule is to predict in each node
#' the majority class from the test data in the node.
#' For a binary classification problem that means a probability of > 50%.
#' In the following we reduce this threshold to 25% or more.
#' This means that if the new observation ends up in a leaf node with 25% or
#'  more reptiles from training in it then the observation
#'  will be classified as a reptile. __Note__ that you should use an unseen test set
#'  for predict here! I did not do that since the data set is too small!
prob <- predict(fit, Zoo_reptile, type = "prob")
pred <- as.factor(ifelse(prob[,"reptile"]>=.25, "reptile", "nonreptile"))

confusionMatrix(data = pred,
                ref = Zoo_reptile$type, positive = "reptile")
#' Note that accuracy goes down and is below the no information rate.
#' However, both measures are based on the idea that all errors have the same
#' cost. What is important is that we are now able to find all almost all
#' reptiles (sensitivity is .8) while before we only found 2 out of 5
#' (sensitivity of .4)
#'
#' ## Plot the ROC Curve
#' since we have a binary classification problem, we can also use ROC.
#' For the ROC curve all different cutoff thresholds for the probability
#' are used and then connected with a line.
library("pROC")
r <- roc(Zoo_reptile$type == "reptile", prob[,"reptile"])
r
plot(r)
#' This also reports the area under the curve.

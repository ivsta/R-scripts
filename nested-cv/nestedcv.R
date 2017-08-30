library(doMC)
library(caret)
library(minicaret)
library(mlbench)

registerDoMC(cores = detectCores())

#---------------------------------------------------------------
# https://topepo.github.io/caret/model-training-and-tuning.html
#---------------------------------------------------------------


data(Sonar)
str(Sonar[, 1:10])



set.seed(998)
inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTraining,]
testing  <- Sonar[-inTraining,]

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)



set.seed(825)

training <- training %>% as_data_frame()
training_features <- training %>% select(-Class) %>% data.frame()


gbmFit1 <- train(x = training_features,
                 y = training$Class,
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1



ncv <- nestedcv(x = training_features,
                y = training$Class,
                method = "gbm", 
                trControl = fitControl,
                ## This last option is actually one
                ## for gbm() that passes through
                verbose = FALSE)
ncv

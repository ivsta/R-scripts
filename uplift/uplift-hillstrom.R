library(caret)
library(pROC)
library(modelr)
library(uplift)
library(causalTree)
library(gbm)
library(doMC)
library(tidyverse)
registerDoMC(cores = 8)

# Inputs
input_dir <- "~/R-scripts/uplift/"
input_file <- input_dir %>% str_c("Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")

# http://blog.minethatdata.com/2008/03/minethatdata-e-mail-analytics-and-data.html

#---------------
# Load data
#---------------

dt <- input_file %>%
  read_delim(delim = ",") %>%
  mutate(history_segment = history_segment %>% as.factor()
         , mens = mens %>% as.factor()
         , womens = womens %>% as.factor()
         , zip_code = zip_code %>% as.factor()
         , newbie = newbie %>% as.factor()
         , channel = channel %>% as.factor()
         , segment = segment %>% as.factor()
         , visit = visit %>% make.names() %>% as.factor()
         , conversion = conversion %>% make.names() %>% as.factor())

dt %>% dim()
# [1] 64000    12

dt <- dt %>% mutate(history_segment = recode(history_segment
                                       , `1) $0 - $100` = "S1"
                                       , `2) $100 - $200` = "S2"
                                       , `3) $200 - $350` = "S3"
                                       , `4) $350 - $500` = "S4"
                                       , `5) $500 - $750` = "S5"
                                       , `6) $750 - $1,000` = "S6"
                                       , `7) $1,000 +` = "S7"))

dt %>% summary()


#--------------------
# Two-model approach
#--------------------

# caret
gbmGrid <- expand.grid(n.trees = (1:10) * 25
                       , interaction.depth = (1:5) * 2
                       , shrinkage = 0.1
                       , n.minobsinnode = 10)

trncntrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3
                         , summaryFunction = twoClassSummary, classProbs = TRUE)


dt_features <- dt %>% dplyr::select(-visit, -conversion, -spend)

segment_list <- c("Mens E-Mail", "Womens E-Mail", "No E-Mail")

dt_features_list <- segment_list %>%
  map(~ dt_features %>% filter(segment == .x))

dt_visit_list <- segment_list %>%
  map(~ dt %>% filter(segment == .x) %>% dplyr::select(visit))

dmy <- dummyVars(" ~ history_segment + mens + womens +
                 zip_code + newbie + channel", data = email_mens_features)

dt_dmy_list <- dt_features_list %>%
  map(~ predict(dmy, newdata = .x) %>% as_data_frame())

dt_features_dmy_list <- dt_features_list %>%
  map2(dt_dmy_list, bind_dmy_features)



gbm_visit <- dt_features_dmy_list %>% map2(dt_visit_list, gbm_train_visit)

gbm_visit[[1]] %>% summary()
gbm_visit[[2]] %>% summary()
gbm_visit[[3]] %>% summary()

dt_features_dmy <- dt_features_dmy_list %>% bind_rows()

pred_list <- gbm_visit %>% map2(dt_features_dmy_list, get_preds)

plot_roc(resp = dt_visit_list[[1]]$visit, pred = pred_list[[1]]$X1)
plot_roc(resp = dt_visit_list[[2]]$visit, pred = pred_list[[2]]$X1)
plot_roc(resp = dt_visit_list[[3]]$visit, pred = pred_list[[3]]$X1)


# uplift






#------------------------
# Functions
#------------------------

bind_dmy_features <- function(features, dmy){
  features %>%
  dplyr::select(recency, history) %>%
  bind_cols(dmy)
}

gbm_train_visit <- function(features, labels){
  train(x = features
        , y = labels$visit
        , method = "gbm"
        , tuneGrid = gbmGrid
        , trControl = trncntrl
        , metric = "ROC")
}

get_preds <- function(model, newdata){
  model %>%
    predict(newdata, type = "prob") %>% as_data_frame() %>%
    dplyr::select(X1)
}

plot_roc <-function(resp, pred){
  roc(response = resp, predictor = pred, ci = TRUE) %>%
    plot(print.auc = TRUE)
}

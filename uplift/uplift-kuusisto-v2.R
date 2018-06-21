library(caret)
library(pROC)
library(tidyverse)
library(modelr)
library(uplift)
library(causalTree)
library(gbm)
library(doMC)
library(xgboost)
registerDoMC(cores = 8)

# Inputs
input_dir <- "~/R-scripts/uplift/"
input_file <- input_dir %>% str_c("stereotypical_customer_simulation.csv")


#---------------
# Load data
#---------------

dt <- input_file %>%
  read_delim(delim = ",", col_types = cols(customer_id = col_character()))

node_cols <- names(dt)[str_detect(names(dt), "Node")]


dt %<>%
  mutate(y = ifelse(outcome == "positive", 1, 0)) %>%
  mutate(ct = ifelse(target_control == "target", 1, 0)) %>%
  mutate_at(node_cols, funs(factor(.))) %>%
  mutate(outcome = outcome %>% as.factor()
         , customer_type = customer_type %>% as.factor()
         , target_control = target_control %>% as.factor())

dt %>% dim()
# [1] 10000    25

dt %>% summary()


#--------------------
# Overall uplift
#--------------------

(prop_pos_target <- dt %>%
   filter(target_control == "target" & y == 1) %>% nrow() /
   dt %>% filter(target_control == "target") %>% nrow())
# [1] 0.4942273

(prop_pos_control <- dt %>%
    filter(target_control == "control" & y == 1) %>% nrow() /
    dt %>% filter(target_control == "control") %>% nrow())
# [1] 0.496346

(overall_uplift <- prop_pos_target - prop_pos_control)
# [1] -0.002118776


#------------------------------
# Two-model approach - xgboost
#------------------------------

# split treatment/control
trtm <- dt %>% filter(target_control == "target")
ctrl <- dt %>% filter(target_control == "control")

# split train/test
trtm_split <- resample_partition(trtm, c(test = 0.3, train = 0.7))
ctrl_split <- resample_partition(ctrl, c(test = 0.3, train = 0.7))

map(trtm_split, dim)
map(ctrl_split, dim)

trtm_features <- map(trtm_split, ~ .x %>%
                       as_data_frame() %>%
                       dplyr::select(starts_with("Node")))

ctrl_features <- map(ctrl_split, ~ .x %>%
                       as_data_frame() %>%
                       dplyr::select(starts_with("Node")))

dt_features <- dt %>% dplyr::select(starts_with("Node"))

trtm_y <- map(trtm_split, ~ .x %>%
                       as_data_frame() %>%
                       dplyr::select(y))

ctrl_y <- map(ctrl_split, ~ .x %>%
                as_data_frame() %>%
                dplyr::select(y))

trtm_outcome <- map(trtm_split, ~ .x %>%
                as_data_frame() %>%
                dplyr::select(outcome))


# binarise
dmy <- dummyVars(" ~ .", data = dt_features)

trtm_dmy <- map(trtm_split, ~ predict(dmy, newdata = .x) %>% as_data_frame())
ctrl_dmy <- map(ctrl_split, ~ predict(dmy, newdata = .x) %>% as_data_frame())
dt_dmy <- predict(dmy, newdata = dt) %>% as_data_frame()







# prepare matrices
d_trtm_trn <- xgb.DMatrix(data = trtm_dmy$train %>% as.matrix()
                          , label = trtm_y$train$y)
d_trtm_tst <- xgb.DMatrix(data = trtm_dmy$test %>% as.matrix()
                          , label = trtm_y$test$y)
d_ctrl_trn <- xgb.DMatrix(data = ctrl_dmy$train %>% as.matrix()
                          , label = ctrl_y$train$y)
d_ctrl_tst <- xgb.DMatrix(data = ctrl_dmy$test %>% as.matrix()
                          , label = ctrl_y$test$y)
d_all <- xgb.DMatrix(data = dt_dmy %>% as.matrix()
                          , label = dt$y)


watchlist_trtm <- list(train = d_trtm_trn, test = d_trtm_tst)
watchlist_ctrl <- list(train = d_ctrl_trn, test = d_ctrl_tst)


# train models
params <- list(max.depth = 2
              , eta = 1
              , nthread = 8
              , objective = "binary:logistic"
              , eval_metric = "auc")
num_folds <- 5
esr <- 20
num_rounds <- 200

set.seed(666)
xgb_cv_trtm <- xgb.cv(data = d_trtm_trn
                      , watchlist = watchlist_trtm
                      , param = params
                      , nfold = num_folds
                      , early_stopping_rounds = esr
                      , nrounds = num_rounds)

set.seed(666)
xgb_cv_ctrl <- xgb.cv(data = d_ctrl_trn
                      , watchlist = watchlist_ctrl
                      , param = params
                      , nfold = num_folds
                      , early_stopping_rounds = esr
                      , nrounds = num_rounds)

set.seed(666)
xgb_trtm <- xgb.train(data = d_trtm_trn
                      , watchlist = watchlist_trtm
                      , param = params
                      , nrounds = xgb_cv_trtm$best_iteration)

set.seed(666)
xgb_ctrl <- xgb.train(data = d_ctrl_trn
                      , watchlist = watchlist_ctrl
                      , param = params
                      , nrounds = xgb_cv_ctrl$best_iteration)


test_pred_trtm <- tibble(pred_prob = predict(xgb_trtm, d_trtm_tst)
                         , y = trtm_y$test$y)
test_pred_ctrl <- tibble(pred_prob = predict(xgb_ctrl, d_ctrl_tst)
                         , y = ctrl_y$test$y)

roc(response = test_pred_trtm$y
    , predictor = test_pred_trtm$pred_prob, ci = TRUE) %>%
  plot(print.auc = TRUE)

roc(response = test_pred_ctrl$y
    , predictor = test_pred_ctrl$pred_prob, ci = TRUE) %>%
  plot(print.auc = TRUE)







# score populations
two_model_pred_trtm <- tibble(pred_prob = predict(xgb_trtm, d_all
                                                  , type = "prob"), y = dt$y)
two_model_pred_ctrl <- tibble(pred_prob = predict(xgb_ctrl, d_all
                                                  , type = "prob"), y = dt$y)

roc(response = dt$outcome, predictor = two_model_pred_trtm$pred_prob, ci = TRUE) %>%
  plot(print.auc = TRUE)
roc(response = dt$outcome, predictor = two_model_pred_ctrl$pred_prob, ci = TRUE) %>%
  plot(print.auc = TRUE)


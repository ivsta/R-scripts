library(caret)
library(pROC)
library(tidyverse)
library(modelr)
library(uplift)
library(causalTree)
library(gbm)
library(doMC)
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


#----------------------
# caret hyperparameters
#----------------------

gbmGrid <- expand.grid(n.trees = (1:10) * 25
                       , interaction.depth = (1:5) * 2
                       , shrinkage = 0.1
                       , n.minobsinnode = 10)

trncntrl <- trainControl(method = "repeatedcv"
                         , number = 5
                         , repeats = 3
                         , summaryFunction = twoClassSummary
                         , classProbs = TRUE)


#--------------------
# Two-model approach
#--------------------

# split treatment/control
trtm <- dt %>% filter(target_control == "target")
ctrl <- dt %>% filter(target_control == "control")

trtm_features <- trtm %>% dplyr::select(starts_with("Node"))
ctrl_features <- ctrl %>% dplyr::select(starts_with("Node"))


# binarise
dmy <- dummyVars(" ~ .", data = trtm_features)

trtm_dmy <- predict(dmy, newdata = trtm_features) %>% as_data_frame()
ctrl_dmy <- predict(dmy, newdata = ctrl_features) %>% as_data_frame()
dt_dmy <- predict(dmy, newdata = dt) %>% as_data_frame()


# train models
gbm_trtm <- train(trtm_dmy, trtm$outcome, method = "gbm", tuneGrid = gbmGrid
                  , trControl = trncntrl, metric = "ROC")
gbm_ctrl <- train(ctrl_dmy, ctrl$outcome, method = "gbm", tuneGrid = gbmGrid
                  , trControl = trncntrl, metric = "ROC")

gbm_trtm %>% summary()
gbm_ctrl %>% summary()


# score populations
pred_trtm <- predict(gbm_trtm, dt_dmy, type = "prob") %>%
  as_data_frame() %>% dplyr::select(positive)
pred_ctrl <- predict(gbm_ctrl, dt_dmy, type = "prob") %>%
  as_data_frame() %>% dplyr::select(positive)

roc(response = dt$outcome, predictor = pred_trtm$positive, ci = TRUE) %>%
  plot(print.auc = TRUE)
roc(response = dt$outcome, predictor = pred_ctrl$positive, ci = TRUE) %>%
  plot(print.auc = TRUE)


# uplift
(dt_uplift <- dt %>%
  dplyr::select(-starts_with("Node"), -customer_id, -customer_type
                , -outcome, -ct) %>%
  mutate(pred_trtm = pred_trtm$positive, pred_ctrl = pred_ctrl$positive) %>%
  mutate(pred_uplift = pred_trtm - pred_ctrl) %>%
  # arrange(pred_uplift %>% desc()) %>%
  arrange(pred_uplift) %>%
  mutate(cum_pop = 1:n()
         , population_prop = cum_pop / n()
         , trt_pop = ifelse(target_control == "target", 1, 0)
         , ctrl_pop = ifelse(target_control == "control", 1, 0)
         , trt_positive = ifelse(target_control == "target" & y == 1, 1, 0)
         , ctrl_positive = ifelse(target_control == "control" & y == 1, 1, 0)
         , cum_trt_pop = cumsum(trt_pop)
         , cum_ctrl_pop = cumsum(ctrl_pop)
         , cum_trt_positive = cumsum(trt_positive)
         , cum_ctrl_positive = cumsum(ctrl_positive)
         , cum_positive_prop_trt = cum_trt_positive / cum_trt_pop
         , cum_positive_prop_ctrl = cum_ctrl_positive / cum_ctrl_pop
         , cum_emp_uplift = cum_positive_prop_trt - cum_positive_prop_ctrl
         , cum_emp_uplift_err = prop_diff_err(p1 = cum_positive_prop_trt
                                              , p2 = cum_positive_prop_ctrl
                                              , n1 = cum_trt_pop
                                              , n2 = cum_ctrl_pop)))

dt_uplift %>% glimpse()


# empirical uplift curve
dt_uplift %>%
  filter(!is.na(cum_emp_uplift)) %>%
  ggplot(aes(x = population_prop
             , y = cum_emp_uplift
             , ymin = cum_emp_uplift - cum_emp_uplift_err
             , ymax = cum_emp_uplift + cum_emp_uplift_err)) +
  geom_ref_line(h = overall_uplift) +
  geom_ribbon(alpha = 0.3, fill = "blue") +
  geom_line(colour = "blue") +
  coord_cartesian(ylim = c(-1, 0))


# uplift by decile
dt_uplift_trtm <- dt_uplift %>%
  filter(target_control == "target") %>%
  mutate(decile = ntile(pred_uplift, 10) %>% as.factor())

dt_uplift_ctrl <- dt_uplift %>%
  filter(target_control == "control") %>%
  mutate(decile = ntile(pred_uplift, 10) %>% as.factor())

(decile_trtm <- dt_uplift_trtm %>%
  group_by(decile) %>%
  summarise(trtm_num_cust = n()
            , trtm_y = sum(y)
            , trtm_prob = trtm_y / trtm_num_cust))

(decile_ctrl <- dt_uplift_ctrl %>%
    group_by(decile) %>%
    summarise(ctrl_num_cust = n()
              , ctrl_y = sum(y)
              , ctrl_prob = ctrl_y / ctrl_num_cust))

(decile_uplift <- decile_trtm %>%
    inner_join(decile_ctrl, by = "decile") %>%
    mutate(emp_uplift = trtm_prob - ctrl_prob
           , emp_uplift_err = prop_diff_err(p1 = trtm_prob
                                            , p2 = ctrl_prob
                                            , n1 = trtm_num_cust
                                            , n2 = ctrl_num_cust)))

decile_uplift %>%
  ggplot(aes(x = decile, y = emp_uplift
             , ymax = emp_uplift + emp_uplift_err
             , ymin = emp_uplift - emp_uplift_err)) +
  geom_ref_line(h = overall_uplift) +
  geom_point() +
  geom_errorbar(width = 0.5)





#------------------------------
# Class transformation approach
#------------------------------

tunegrid <- expand.grid(mtry = c(5:10))

class_trans <- dt %>%
  mutate(z = ifelse((outcome == "positive" & target_control == "target") |
                      (outcome == "negative" & target_control == "control")
                    , "Z1", "Z0") %>% as.factor()) %>%
  dplyr::select(-ct, -customer_id, -customer_type)

(class_trans %>%
  group_by(outcome, target_control, z) %>%
  summarise(num_cust = n()))

ex <- resample_partition(class_trans, c(test = 0.3, train = 0.7))
map(ex, dim)

train_features <- ex$train %>% as_data_frame() %>% dplyr::select(starts_with("Node"))
test_features <- ex$test %>% as_data_frame() %>% dplyr::select(starts_with("Node"))

train_labels <- ex$train %>% as_data_frame() %>% dplyr::select(z) %>% c()
test_labels <- ex$test %>% as_data_frame() %>% dplyr::select(z) %>% c()
test_target_control <- ex$test %>% as_data_frame() %>% dplyr::select(target_control) %>% c()
test_y <- ex$test %>% as_data_frame() %>% dplyr::select(y) %>% c()

(class_trans_rf <- train(train_features
                        , train_labels$z
                        , method = "rf"
                        , trControl = trncntrl
                        , metric = "ROC", tuneGrid = tunegrid))

class_trans_rf %>% summary()

pred_test <- predict(class_trans_rf, test_features, type = "prob") %>%
  as_data_frame() %>%
  dplyr::select(Z1) %>%
  mutate(decile = ntile(Z1, 10) %>% as.factor()) %>%
  mutate(actual_label = test_labels$z
         , target_control = test_target_control$target_control
         , y = test_y$y)

roc(response = pred_test$actual_label, predictor = pred_test$Z1, ci = TRUE) %>%
  plot(print.auc = TRUE)



# uplift
(pred_test_uplift <- pred_test %>%
  rename(pred_uplift = Z1) %>%
  # arrange(pred_uplift %>% desc()) %>%
  arrange(pred_uplift) %>%
  mutate(cum_pop = 1:n()
         , population_prop = cum_pop / n()
         , trt_pop = ifelse(target_control == "target", 1, 0)
         , ctrl_pop = ifelse(target_control == "control", 1, 0)
         , trt_positive = ifelse(target_control == "target" & y == 1, 1, 0)
         , ctrl_positive = ifelse(target_control == "control" & y == 1, 1, 0)
         , cum_trt_pop = cumsum(trt_pop)
         , cum_ctrl_pop = cumsum(ctrl_pop)
         , cum_trt_positive = cumsum(trt_positive)
         , cum_ctrl_positive = cumsum(ctrl_positive)
         , cum_positive_prop_trt = cum_trt_positive / cum_trt_pop
         , cum_positive_prop_ctrl = cum_ctrl_positive / cum_ctrl_pop
         , cum_emp_uplift = cum_positive_prop_trt - cum_positive_prop_ctrl
         , cum_emp_uplift_err = prop_diff_err(p1 = cum_positive_prop_trt
                                              , p2 = cum_positive_prop_ctrl
                                              , n1 = cum_trt_pop
                                              , n2 = cum_ctrl_pop)))

pred_test_uplift %>% glimpse()


# empirical uplift curve
pred_test_uplift %>%
  filter(!is.na(cum_emp_uplift)) %>%
  ggplot(aes(x = population_prop
             , y = cum_emp_uplift
             , ymin = cum_emp_uplift - cum_emp_uplift_err
             , ymax = cum_emp_uplift + cum_emp_uplift_err)) +
  geom_ref_line(h = overall_uplift) +
  geom_ribbon(alpha = 0.3, fill = "blue") +
  geom_line(colour = "blue") +
  coord_cartesian(ylim = c(-1, 0))


# uplift by decile
pred_test_uplift_trtm <- pred_test_uplift %>%
  filter(target_control == "target") %>%
  mutate(decile = ntile(pred_uplift, 10) %>% as.factor())

pred_test_uplift_ctrl <- pred_test_uplift %>%
  filter(target_control == "control") %>%
  mutate(decile = ntile(pred_uplift, 10) %>% as.factor())

(test_decile_trtm <- pred_test_uplift_trtm %>%
  group_by(decile) %>%
  summarise(trtm_num_cust = n()
            , trtm_y = sum(y)
            , trtm_prob = trtm_y / trtm_num_cust))

(test_decile_ctrl <- pred_test_uplift_ctrl %>%
    group_by(decile) %>%
    summarise(ctrl_num_cust = n()
              , ctrl_y = sum(y)
              , ctrl_prob = ctrl_y / ctrl_num_cust))

(test_decile_uplift <- test_decile_trtm %>%
    inner_join(test_decile_ctrl, by = "decile") %>%
    mutate(emp_uplift = trtm_prob - ctrl_prob
           , emp_uplift_err = prop_diff_err(p1 = trtm_prob
                                            , p2 = ctrl_prob
                                            , n1 = trtm_num_cust
                                            , n2 = ctrl_num_cust)))

test_decile_uplift %>%
  ggplot(aes(x = decile, y = emp_uplift
             , ymax = emp_uplift + emp_uplift_err
             , ymin = emp_uplift - emp_uplift_err)) +
  geom_ref_line(h = overall_uplift) +
  geom_point() +
  geom_errorbar(width = 0.5)



#-----------------
# ccif
#-----------------

form <- as.formula(paste('y ~'
                         , 'trt(ct) +'
                         , paste('Node', 1:15, sep = '', collapse = "+")
                         , '+'
                         , paste('Node', 17:20, sep = '', collapse = "+")))

fit1 <- ccif(formula = form
             , data = dt
             , ntree = 50
             , split_method = "Int"
             , distribution = approximate (B=999)
             , pvalue = 0.05
             , verbose = TRUE)
print(fit1)
summary(fit1)




#----------------------
# Functions
#----------------------

beta_err <- function(conversion_rate, sample_size, prob){
  qbeta(p = prob
        , shape1 = conversion_rate * sample_size
        , shape2 = sample_size - conversion_rate * sample_size)
}

prop_diff_err <- function(p1, p2, n1, n2){
  # z = 1.645 # 90%
  z = 1.96 # 95%
  # z = 2.576 # 99%
  z * sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
}



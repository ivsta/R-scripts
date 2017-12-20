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
# Two-model approach
#--------------------

# caret
gbmGrid <- expand.grid(n.trees = (1:10) * 25
                       , interaction.depth = (1:5) * 2
                       , shrinkage = 0.1
                       , n.minobsinnode = 10)

trtm <- dt %>% filter(target_control == "target")
ctrl <- dt %>% filter(target_control == "control")

trtm_features <- trtm %>% dplyr::select(starts_with("Node"))
ctrl_features <- ctrl %>% dplyr::select(starts_with("Node"))

dmy <- dummyVars(" ~ .", data = trtm_features)

trtm_dmy <- predict(dmy, newdata = trtm_features) %>% as_data_frame()
ctrl_dmy <- predict(dmy, newdata = ctrl_features) %>% as_data_frame()
dt_dmy <- predict(dmy, newdata = dt) %>% as_data_frame()


trncntrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3, summaryFunction = twoClassSummary, classProbs = TRUE)

gbm_trtm <- train(trtm_dmy, trtm$outcome, method = "gbm", tuneGrid = gbmGrid, trControl = trncntrl, metric = "ROC")
gbm_ctrl <- train(ctrl_dmy, ctrl$outcome, method = "gbm", tuneGrid = gbmGrid, trControl = trncntrl, metric = "ROC")

gbm_trtm %>% summary()
gbm_ctrl %>% summary()

pred_trtm <- predict(gbm_trtm, dt_dmy, type = "prob") %>% as_data_frame() %>% dplyr::select(positive)
pred_ctrl <- predict(gbm_ctrl, dt_dmy, type = "prob") %>% as_data_frame() %>% dplyr::select(positive)

roc(response = dt$outcome, predictor = pred_trtm$positive, ci = TRUE) %>% plot(print.auc = TRUE)
roc(response = dt$outcome, predictor = pred_ctrl$positive, ci = TRUE) %>% plot(print.auc = TRUE)


# uplift
dt_uplift <- dt %>%
    dplyr::select(-starts_with("Node")) %>%
    mutate(pred_trtm = pred_trtm$positive, pred_ctrl = pred_ctrl$positive) %>%
    mutate(pred_uplift = pred_trtm - pred_ctrl)

dt_uplift_trtm <- dt_uplift %>% filter(target_control == "target") %>% mutate(decile = ntile(pred_uplift, 10))
dt_uplift_ctrl <- dt_uplift %>% filter(target_control == "control") %>% mutate(decile = ntile(pred_uplift, 10))

decile_trtm <- dt_uplift_trtm %>%
  group_by(decile) %>%
  summarise(trtm_num_cust = n(), trtm_uplift = mean(pred_uplift), trtm_y = sum(y))

decile_ctrl <- dt_uplift_ctrl %>%
  group_by(decile) %>%
  summarise(ctrl_num_cust = n(), ctrl_uplift = mean(pred_uplift), ctrl_y = sum(y))

decile_uplift <- decile_trtm %>%
  inner_join(decile_ctrl, by = "decile") %>%
  mutate(diff_uplift = trtm_uplift - ctrl_uplift)

decile_uplift %>%
  ggplot(aes(x = decile, y = diff_uplift)) +
  geom_bar(stat = "identity") +
  geom_ref_line(h = 0)

decile_uplift %>%
  mutate(cum_uplift = cumsum(diff_uplift) / decile) %>%
  ggplot(aes(x = decile, y = cum_uplift)) +
  geom_bar(stat = "identity")

decile_uplift %>%
  mutate(gain = ((trtm_y / trtm_num_cust) - (ctrl_y / ctrl_num_cust)) * (trtm_num_cust + ctrl_num_cust)) %>%
  mutate(cum_gain = cumsum(gain)) %>%
  ggplot(aes(x = decile, y = cum_gain)) +
  geom_bar(stat = "identity")


#------------------------------
# Class transformation approach
#------------------------------

tunegrid <- expand.grid(mtry = c(1:5))

class_trans <- dt %>%
  mutate(z = ifelse((outcome == "positive" & target_control == "target") | (outcome == "negative" & target_control == "control"), "Z1", "Z0")) %>%
  mutate(z = z %>% as.factor()) %>%
  dplyr::select(-y, -ct, -customer_id, -customer_type)

class_trans %>%
  group_by(outcome, target_control, z) %>%
  summarise(num_cust = n())

numrows <- 2e3

class_trans_rf <- train(dt_dmy %>% head(numrows), class_trans$z[1:numrows], method = "rf"
                        , trControl = trncntrl
                        , metric = "ROC", tuneGrid = tunegrid)

class_trans_rf %>% summary()

pred_class_trans <- predict(class_trans_rf, dt_dmy, type = "prob") %>% as_data_frame() %>% dplyr::select(Z1)

roc(response = class_trans$z, predictor = pred_class_trans$Z1, ci = TRUE) %>% plot(print.auc = TRUE)

pred_class_trans <- pred_class_trans %>% mutate(decile = ntile(Z1, 10))

(class_trans_decile <- pred_class_trans %>%
  group_by(decile) %>%
  summarise(num_cust = n(), uplift = mean(Z1)))

class_trans_decile %>%
  ggplot(aes(x = decile, y = uplift)) +
  geom_bar(stat = "identity") +
  geom_ref_line(h = 0)



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

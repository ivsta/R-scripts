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
# ccif
#------------------------------

dt %>% glimpse()


# Train/test
trn_tst <- dt %>% resample_partition(c(train = 0.8, test = 0.2))

train <- trn_tst$train %>% as_tibble()
test <- trn_tst$test %>% as_tibble()

model_ccif <- ccif(x = train %>% dplyr::select(starts_with("Node"))
                   , y = train$y, ct = train$ct, split_method = "KL"
                   , ntree = 10, verbose = TRUE)

model_ccif %>% summary()

pred <- predict(model_ccif, test)

perf <- performance(pred[, 1], pred[, 2], test$y, test$ct, direction = 1)

q <- qini(perf, plotit = TRUE)
q$Qini



# Cross-validation
folds <- dt %>% crossv_kfold(5)
as_tibble(folds$train[[1]])$y %>% summary
folds$test[[1]]


folds <- folds %>% mutate(model = model_ccif <- map(folds$train
  , ~ccif(x = as_tibble(.) %>% dplyr::select(starts_with("Node"))
  , y = as_tibble(.)$y, ct = as_tibble(.)$ct
  , split_method = "KL", ntree = 10)))

folds$model[[1]] %>% summary()

summary(folds$model[[1]])$importance

folds$test[[1]] %>% class

preds_tbl <- map2(folds$model, folds$test %>% map(as_data_frame)
              , ~predict(.x, newdata = .y)) %>%
  map(as_data_frame)

preds_tbl <- map(preds_tbl, ~mutate(., uplift = pr.y1_ct1 - pr.y1_ct0))



preds <- map2(folds$model, folds$test %>% map(as_data_frame)
                  , ~predict(.x, newdata = .y))

perfs <- map2(preds
             , folds$test %>% map(as_data_frame)
             , ~performance(.x[, 1], .x[, 2], .y$y, .y$ct, direction = 1))


q_list <- perfs %>% map(qini)

qini_vec <- q_list %>% map_dbl(~.$Qini)
mean(qini_vec)
sd(qini_vec)



# Final model
final_model_ccif <- ccif(x = dt %>% dplyr::select(starts_with("Node"))
                         , y = dt$y, ct = dt$ct, split_method = "KL"
                         , ntree = 10, verbose = TRUE)

final_model_ccif %>% summary()

library(caret)
library(pROC)
library(tidyverse)
library(modelr)
library(uplift)
library(causalTree)
library(gbm)
library(doMC)
library(xgboost)
library(ggplot2)
library(ggthemes)
library(forcats)
registerDoMC(cores = 8)

# Inputs
input_dir <- "~/R-scripts/uplift/"
input_file <- input_dir %>% str_c("stereotypical_customer_simulation.csv")


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

dt %>% glimpse()


#-------------------
# Train/test
#-------------------
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


#-------------------
# Cross-validation
#-------------------
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





preds <- map2(folds$model, folds$test %>% map(as_data_frame)
                  , ~predict(.x, newdata = .y))

perfs <- map2(preds
             , folds$test %>% map(as_data_frame)
             , ~performance(.x[, 1], .x[, 2], .y$y, .y$ct, direction = 1))


q_list <- perfs %>% map(qini)

qini_vec <- q_list %>% map_dbl(~.$Qini)
mean(qini_vec)
# [1] 0.0401919

sd(qini_vec)
# [1] 0.01432016




preds_tbl <- map2(folds$model, folds$test %>% map(as_data_frame)
                  , ~predict(.x, newdata = .y)) %>%
  map(as_data_frame)

preds_tbl <- map(preds_tbl, ~mutate(., uplift = pr.y1_ct1 - pr.y1_ct0))


#-------------------
# Final model
#-------------------
final_model_ccif <- ccif(x = dt %>% dplyr::select(starts_with("Node"))
                         , y = dt$y, ct = dt$ct, split_method = "KL"
                         , ntree = 10, verbose = TRUE)

final_model_ccif %>% summary()

scores <- predict(final_model_ccif, dt) %>% as_data_frame()

results <- dt %>%
  dplyr::select(customer_id, customer_type, ct, y) %>%
  mutate(pr.y1_ct1 = scores$pr.y1_ct1, pr.y1_ct0 = scores$pr.y1_ct0) %>%
  mutate(predicted_uplift = pr.y1_ct1 - pr.y1_ct0) %>%
  arrange(desc(predicted_uplift)) %>%
  mutate(decile = 11 - ntile(predicted_uplift, 10))


results %>%
  group_by(customer_type) %>%
  summarise(mean_predicted_uplift = mean(predicted_uplift)) %>%
  arrange(desc(mean_predicted_uplift))
# customer_type mean_predicted_uplift
# <fct>                         <dbl>
# 1 persuadable                 0.0476
# 2 sure_thing                  0.00292
# 3 lost_cause                 -0.00655
# 4 sleeping_dog               -0.0495

final_perf <- performance(results$pr.y1_ct1, results$pr.y1_ct0
                          , dt$y, dt$ct, direction = 1, groups = 20)

final_q <- qini(final_perf, plotit = TRUE)
final_q$Qini



#-------------------
# uplift by decile
#-------------------

(deciles <- results %>%
  group_by(decile, ct) %>%
  summarise(num = n(), num_y = sum(y), prop = num_y / num
            , mean_predicted_uplift = mean(predicted_uplift)))




uplift_deciles <- deciles %>% filter(ct == 1) %>%
  inner_join(deciles %>% filter(ct == 0), by = "decile") %>%
  mutate(emp_uplift = prop.x - prop.y
         , emp_uplift_err = prop_diff_err(p1 = prop.x
                                          , p2 = prop.y
                                          , n1 = num.x
                                          , n2 = num.y))


uplift_deciles %>%
  ggplot(aes(x = decile %>% as.factor(), y = emp_uplift
             , ymax = emp_uplift + emp_uplift_err
             , ymin = emp_uplift - emp_uplift_err)) +
  geom_ref_line(h = overall_uplift) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  scale_x_discrete(name = "decile")


results %>%
  mutate(customer_type = fct_relevel(customer_type
                                     , c("persuadable", "sure_thing"
                                         , "lost_cause", "sleeping_dog"))) %>%
  ggplot(aes(x = decile %>% as.factor(), fill = customer_type)) +
  geom_bar() +
  scale_fill_ptol() +
  scale_x_discrete(name = "decile")


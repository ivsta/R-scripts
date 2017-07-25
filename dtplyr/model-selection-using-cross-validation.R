#-----------------------------------------------------
# https://ijlyttle.github.io/model_cv_selection.html#
#-----------------------------------------------------

library("devtools")
library("tibble")
library("ggplot2")
library("modelr")
library("dplyr")
library("purrr")
library("tidyr")
library("pryr")


#--------------------
# Linear regression
#--------------------

truth <- function(x){
  1 + 2*x - x^2
}

noise <- function(x){
  rnorm(length(x), sd = 0.1)
}

df <- 
  data_frame(
    x = runif(n = 100, min = 0, max = 1),
    y = truth(x) + noise(x)
  ) %>%
  print()

ggplot(df, aes(x = x, y = y)) +
  stat_function(fun = truth, color = "black", alpha = 0.7, linetype = "dashed") +
  geom_point(alpha = 0.6) +
  theme_minimal()

df_split <-
  df %>%
  crossv_mc(n = 50) %>%
  print()

str(df_split[1,])

# given a dataframe, return a model
fn_model <- function(df){
  lm(y ~ x, data = df)
}

df_split_model <-
  df_split %>%
  mutate(model = map(train, fn_model)) %>%
  print()

object_size(df_split_model)

sample_prediction <- function(model, sample){
  df <- as.data.frame(sample)
  pred <- stats::predict(model, df)
  pred
}

# this will work for lm, biglm - will have to check for others
sample_response <- function(model, sample){
  df <- as.data.frame(sample)
  var_response <- all.vars(formula(model))[[1]]
  df[[var_response]]
}

# the reason I am not using add_prediction here is that I want to jettison the 
# data more-easily and focus on the residuals
df_split_resid <- 
  df_split_model %>%
  gather(key = split, value = data, train, test) %>%
  mutate(
    pred = map2(model, data, sample_prediction),
    resp = map2(model, data, sample_response),
    resid = map2(pred, resp, `-`)
  ) %>%
  select(.id, split, pred, resp, resid) %>%
  unnest() %>%
  print()

ggplot(df_split_resid, aes(x = resid, color = split, group = paste(split, .id))) +
  stat_density(
    aes(y = ..density..), 
    geom = "line", 
    position = "identity",
    alpha = 0.3
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))

df_split_rmse <- 
  df_split_resid %>%
  group_by(.id, split) %>%
  summarize(
    rmse = sqrt(sum(resid*resid)/length(resid))
  ) %>%
  ungroup() %>%
  print()

ggplot(df_split_rmse, aes(x = "linear", y = rmse)) +
  geom_point(
    aes(color = split), 
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5),
    alpha = 0.75
  ) +
  ylim(0, NA) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))

make_model <- function(order){
  
  # superstition (maybe) to evaluate the expression, preserving it
  # in this environment for the function
  order
  
  function(df){
    
    df <- as.data.frame(df)
    
    lm(y ~ poly(x, order), data = df)
  }
}

df_model <-
  frame_data(
    ~model_name, ~model_creator,
    #    "zero", make_model(0),
    "one", make_model(1),
    "two", make_model(2),
    "three", make_model(3),
    "four", make_model(4),
    "five",  make_model(5)
  ) %>% 
  print()

df_sample_model <- 
  expand.grid(.id = df_split$.id, model_name = df_model$model_name, stringsAsFactors = FALSE) %>%
  as_data_frame() %>%
  left_join(df_split, by = ".id") %>%
  left_join(df_model, by = "model_name") %>%
  print()

make_model <- function(data, model_creator){
  model_creator(data)
}

df_sample_model_created <-
  df_sample_model %>%
  mutate(
    model = map2(train, model_creator, make_model)
  ) %>%
  print()

df_sample_resid <-
  df_sample_model_created %>%
  gather(key = split, value = data, train, test) %>%
  mutate(
    pred = map2(model, data, sample_prediction),
    resp = map2(model, data, sample_response)
  ) %>%
  select(.id, model_name, split, pred, resp) %>%
  unnest() %>%
  mutate(resid = pred - resp) %>%
  print()

df_sample_resid %>%
  mutate(model_name = factor(model_name, levels = c("one", "two", "three", "four", "five"))) %>%
  ggplot(aes(x = resid, color = split, group = paste(split, .id))) +
  stat_density(
    aes(y = ..density..), 
    geom = "line", 
    position = "identity",
    alpha = 0.3
  ) +
  facet_wrap(~ model_name) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))

df_sample_rmse <- 
  df_sample_resid %>%
  group_by(.id, model_name, split) %>%
  summarize(
    rmse = sqrt(sum(resid*resid)/length(resid))
  ) %>%
  ungroup() %>%
  print()

df_sample_rmse %>%
  mutate(model_name = factor(model_name, levels = c("one", "two", "three", "four", "five"))) %>%
  ggplot(aes(x = model_name, y = rmse, color = split, group = split)) +
  stat_summary(geom = "line", fun.y = "mean") +
  geom_point(
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5),
    alpha = 0.5
  ) +
  ylim(0, NA) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))

# clean this up
fn_x <- function(x){
  -(x^2 - 2*x - 1)
}

resid_truth <- function(x, y){
  
  (y - fn_x(x))/0.1
  
}

nobs <- 1000

df_logistic <- 
  data_frame(
    x = runif(nobs, 0, 1),
    y = runif(nobs, 1, 2),
    state = runif(nobs, 0, 1) < pnorm(resid_truth(x, y))
  )

df_logistic %>%
  ggplot(aes(x = x, y = y, color = state, shape = state)) +
  stat_function(fun = fn_x, color = "black", alpha = 0.7, linetype = "dashed") +
  geom_point() 

df_logistic_cv <- 
  df_logistic %>%
  crossv_mc(n = 50) %>%
  print()

make_model_logistic <- function(order_x, order_y = 1){
  
  # superstition (maybe) to evaluate the expression, preserving it
  # in this environment for the function
  order
  
  function(df){
    
    df <- as.data.frame(df)
    
    glm(
      state ~ poly(y, order_y) + poly(x, order_x), 
      data = df,
      family = "binomial"
    )
  }
}

model_one <- make_model_logistic(order_x = 1)

df_logistic_model_one <-
  df_logistic_cv %>%
  mutate(model = map(train, model_one)) %>%
  print()

fn_misclass <- function(pred, resp){
  
  # pred is given as log odds
  # resp is given as T/F 
  
  is_misclass <- (sign(pred) == 1) != resp
  
}

df_logistic_resid_one <- 
  df_logistic_model_one %>%
  gather(key = split, value = data, train, test) %>%
  mutate(
    pred = map2(model, data, sample_prediction),
    resp = map2(model, data, sample_response),
    is_misclass = map2(pred, resp, fn_misclass)
  ) %>%
  select(.id, split, pred, resp, is_misclass) %>%
  unnest() %>%
  print()

ggplot(df_logistic_resid_one, aes(x = pred, color = split, group = paste(split, .id))) +
  stat_density(
    aes(y = ..density..), 
    geom = "line", 
    position = "identity",
    alpha = 0.3
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))

df_logistic_misclass_one <-
  df_logistic_resid_one %>%
  group_by(.id, split) %>%
  summarize(
    misclassification_rate = sum(is_misclass)/n()
  ) %>%
  ungroup() %>%
  print()

ggplot(df_logistic_misclass_one, aes(x = "one_one", y = misclassification_rate)) +
  geom_point(
    aes(color = split), 
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5),
    alpha = 0.75
  ) +
  ylim(0, NA) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))

df_model_logistic <-
  frame_data(
    ~model_name, ~model_creator,
    "one_one", make_model_logistic(order_x = 1, order_y = 1),
    "two_one", make_model_logistic(order_x = 2, order_y = 1),
    "one_two", make_model_logistic(order_x = 1, order_y = 2),
    "three_one", make_model_logistic(order_x = 3, order_y = 1),
    "two_two", make_model_logistic(order_x = 2, order_y = 2),
    "one_three",  make_model_logistic(order_x = 1, order_y = 3)
  ) %>% 
  print()

df_sample_logistic_model <- 
  expand.grid(.id = df_logistic_cv$.id, model_name = df_model_logistic$model_name, stringsAsFactors = FALSE) %>%
  as_data_frame() %>%
  left_join(df_logistic_cv, by = ".id") %>%
  left_join(df_model_logistic, by = "model_name") %>%
  mutate(model = map2(train, model_creator, make_model)) %>%
  print()

df_sample_logistic_resid <-
  df_sample_logistic_model %>%
  gather(key = split, value = data, train, test) %>%
  mutate(
    pred = map2(model, data, sample_prediction),
    resp = map2(model, data, sample_response),
    is_misclass = map2(pred, resp, fn_misclass)
  ) %>%
  select(.id, model_name, split, pred, is_misclass) %>%
  unnest() %>%
  print()

levels_model_logistic <- 
  c("one_one", "one_two", "two_one", "one_three", "two_two", "three_one")

df_sample_logistic_resid %>%
  mutate(model_name = factor(model_name, levels = levels_model_logistic)) %>%
  ggplot(aes(x = pred, color = split, group = paste(split, .id))) +
  stat_density(
    aes(y = ..density..), 
    geom = "line", 
    position = "identity",
    alpha = 0.3
  ) +
  facet_wrap(~ model_name) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))

df_logistic_misclass <-
  df_sample_logistic_resid %>%
  group_by(.id, model_name, split) %>%
  summarize(
    misclassification_rate = sum(is_misclass)/n()
  ) %>%
  ungroup() %>%
  print()

df_logistic_misclass %>%
  mutate(model_name = factor(model_name, levels = levels_model_logistic)) %>%
  ggplot(aes(x = model_name, y = misclassification_rate, color = split, group = split)) +
  stat_summary(geom = "line", fun.y = "mean") +
  geom_point(
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5),
    alpha = 0.75
  ) +
  ylim(0, NA) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))
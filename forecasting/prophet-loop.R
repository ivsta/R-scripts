# https://facebook.github.io/prophet/docs/quick_start.html
# https://www.kaggle.com/c/demand-forecasting-kernels-only/data

library(tidyverse)
library(magrittr)
library(prophet)
library(stringr)

# Inputs
train_file <- '/work/data/train.csv.zip'
test_file <- '/work/data/test.csv.zip'


# Outputs
output_dir <- '/work/plots/'
output_dir %>% dir.create(recursive = TRUE)


#----------------
# Functions
#----------------

read_file <- function(file){
  dt <- file %>%
    read_csv() %>%
    mutate(store = store %>% as.factor(), item = item %>% as.factor())
  return(dt)
}


plot_ts <- function(dt, st, it){
  dt %>%
    filter(store == st & item == it) %>%
    ggplot(aes(x = date, y = sales)) +
    geom_line() +
    ggtitle(str_c('Store: ', st, '\n', 'Item: ', it))
  ggsave(str_c(output_dir, 'ts_', st, '_', it, '.png'))
}


get_result <- function(dt, st, it){
  dt_select <- dt %>%
    filter(store == st & item == it) %>%
    select(ds = date, y = sales)

  model <- dt_select %>% prophet()

  cv <- cross_validation(model = model, horizon = 30, units = 'days')

  performance <- performance_metrics(cv)

  result <- performance %>%
    filter(horizon == 30) %>%
    summarise(mae = mean(mae), mape = mean(mape)) %>%
    mutate(store = st, item = it) %>%
    select(store, item, mae, mape)

  return(result)

}


#----------------
# Load data
#----------------

dt <- map(c(train_file, test_file), read_file)

dt %>% map(str) %>% invisible()
dt %>% map(summary)
dt %>% map(glimpse) %>% invisible()


#----------------
# Load data
#----------------

store_u <- dt[[1]] %>% distinct(store) %>% .$store %>% as.character()
item_u <- dt[[1]] %>% distinct(item) %>% .$item %>% as.character()

store_item_combos <- expand.grid(store = store_u, item = item_u) %>% as_tibble()


#----------------
# Plots
#----------------

dt[[1]] %>% plot_ts(1, 1)

map2(plot_ts, .x = store_item_combos$store[1:20],
  .y = store_item_combos$item[1:20], dt = dt[[1]])


#----------------
# Forecasts
#----------------

dt[[1]] %>% get_result(1, 1)

results <- map2_dfr(get_result, .x = store_item_combos$store[1:20],
  .y = store_item_combos$item[1:20], dt = dt[[1]])

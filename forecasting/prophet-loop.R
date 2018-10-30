# https://www.kaggle.com/c/demand-forecasting-kernels-only/data
# box rsync news:lab:gonzo.yellow.996 -- -aH ~/Desktop/ :~/work/data

library(tidyverse)
library(magrittr)
library(prophet)


train_file <- '/work/data/train.csv.zip'

dt <- train_file %>%
  read_csv() %>%
  mutate(store = store %>% as.factor(), item = item %>% as.factor())

dt %>% str()
dt %>% summary()
dt %>% glimpse()


dt %>%
  filter(store == '1' & item == '1') %>%
  ggplot(aes(x = date, y = sales)) +
  geom_line()

dt_select <- dt %>%
  filter(store == '1' & item == '1') %>%
  select(ds = date, y = sales)


model <- dt_select %>% prophet()

# forecast <- predict(model, test)

# forecast %>% head()

# plot(model, forecast)

# prophet_plot_components(model, forecast)

# forecast %>% select(ds, yhat)


cv <- cross_validation(model = model, horizon = 30, units = 'days')

performance <- performance_metrics(cv)

plot_cross_validation_metric(cv, metric = 'mape')

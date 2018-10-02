library(tidyverse)
library(lightgbm)

magic_number <- 1

set.seed(7)
train <- read_csv("~/R-scripts/lightgbm/demand-forecasting/input/train.csv")
test <- read_csv("~/R-scripts/lightgbm/demand-forecasting/input/test.csv")

train %>%
  sample_n(2e4) %>%
  ggplot(aes(x = date, y = sales)) +
  geom_point(size = 0.2) +
  facet_grid(store ~ item)

train %>%
  filter(store == 1) %>%
  sample_n(2e4) %>%
  ggplot(aes(x = date, y = sales)) +
  geom_point(size = 0.2) +
  facet_wrap( ~ item)

sels <- caret::createDataPartition(train$sales, p = 0.95, list = F) %>% c()

val_target <- train$sales[-sels]

##### preprocessing
train_num <- 1:nrow(train)
test$sales <- NA
train$sales[-sels] <- NA
train$id <- NA
train_test <- rbind(train, test)

train_test2 <- train_test %>%
  mutate(year = as.integer(format(date, "%Y")),
         month = as.integer(format(date, "%m")),
         weekday = as.integer(factor(weekdays(date, abbreviate = TRUE)) ),
         quoter = year * 4 + (month) %/% 3 - 8051
  )


train_test2 %>%
  filter(store == 1 & item == 1) %>%
  ggplot(aes(x = date, y = quoter)) +
  geom_point()

train_test3 <- train_test2 %>%
  group_by(item, store, month) %>%
  mutate(item_store_month_sales = mean(sales, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(store, item, weekday) %>%
  mutate(store_item_weekday_sales = mean(sales, na.rm = TRUE)) %>%
  ungroup()　%>%
  mutate(round_item_store_month_sales = round(item_store_month_sales), round_store_item_weekday_sales = round(store_item_weekday_sales))

train2 <- train_test3[train_num,]
test2 <- train_test3[-train_num,]

##### end preprocessing

usecol <- c(
  "month",
  "quoter",
  "item_store_month_sales",
  "store_item_weekday_sales",
  "round_item_store_month_sales",
  "round_store_item_weekday_sales"
  )

dtrain <- lgb.Dataset(as.matrix(train2[sels, usecol]) , label = train2$sales[sels], free_raw_data = FALSE)
dval <- lgb.Dataset(as.matrix(train2[-sels,usecol] ), label = val_target, free_raw_data = FALSE)

params <- list(objective = "regression", metric = "mape")
valids <- list(test = dval)
n_iteration <- 10000

model <- lgb.train(params,
                   dtrain,
                   nrounds = n_iteration,
                   valids = valids,
                   num_leaves = 7,
                   max_depth =3,
                   eval_freq = 25,
                   categorical_feature = "month",
                   learning_rate = 0.03,
                   early_stopping_round = 50
)


ptest  <- predict(model, as.matrix(test2 %>% select(usecol)), rawscore = FALSE) * magic_number

sample_submission <- read_csv("~/R-scripts/lightgbm/demand-forecasting/input/sample_submission.csv")
sample_submission$sales <- round(ptest)
sample_submission$id = as.character(sample_submission$id)
write_csv(sample_submission, path = paste0("~/R-scripts/lightgbm/demand-forecasting/output/sub_val", model$best_score, ".csv"))

# https://www.kaggle.com/c/demand-forecasting-kernels-only/data
# box rsync anz:lab:beaker.cyan.117 -- -aH ~/Desktop/ :~/work/data

library(tidyverse)
library(magrittr)
library(forecast)
library(prophet)


dt_file <- '/work/data/train.csv.zip'

dt <- train_file %>%
  read_csv() %>%
  mutate(store = store %>% as.factor(), item = item %>% as.factor())

dt %>% str()
dt %>% summary()
dt %>% glimpse()

dt %<>% mutate(set = ifelse(date < '2017-01-01', 'train', 'test'))

dt %>%
  filter(store == '1' & item == '1') %>%
  ggplot(aes(x = date, y = sales, colour = set)) +
  geom_line()

train <- dt %>%
  filter(set == 'train') %>%
  filter(store == '1' & item == '1') %>%
  select(date, sales)

test <- dt %>%
  filter(set == 'test') %>%
  filter(store == '1' & item == '1') %>%
  select(date, sales)


m <- train %>% rename(ds = date, y = sales) %>% prophet()
future <- m %>% make_future_dataframe(periods = nrow(test))
f0 <- predict(m, future)

plot(m, f0) +
  geom_point(data = test, aes(x = date %>% as.POSIXct(), y = sales), colour = 'red', alpha = 0.5)



train_ts <- train$sales %>% ts(frequency = 365)


ppy <- train_ts %>% frequency()

train_ts %>% SeasonalityTest(ppy)

dec <- train_ts %>% decompose()
des_input <- train_ts / dec$seasonal
SIout <- head(rep(dec$seasonal[(length(dec$seasonal)-366):length(dec$seasonal)], nrow(test)), nrow(test))

f1 <- train_ts %>% naive(h = nrow(test))
f2 <- train_ts %>% naive_seasonal(fh = nrow(test))
f3 <- train_ts %>% naive(h = nrow(test))$mean * SIout
f4 <- train_ts %>% ses(h = nrow(test))
f5 <- train_ts %>% holt(h = nrow(test), damped = FALSE)
f6 <- train_ts %>% holt(h = nrow(test), damped = TRUE)
f7 <- train_ts %>% Theta.classic(fh = nrow(test))


autoplot(f1)
autoplot(f2)
autoplot(f4)
autoplot(f5)
autoplot(f6)


results1 <- tibble(date = test$date, mean = f1$mean)
results2 <- tibble(date = test$date, mean = f2$mean)
results4 <- tibble(date = test$date, mean = f4$mean)
results4 <- tibble(date = test$date, mean = f5$mean)
results5 <- tibble(date = test$date, mean = f5$mean)
results6 <- tibble(date = test$date, mean = f6$mean)
results7 <- tibble(date = test$date, mean = f7$mean)


dt %>%
  filter(store == '1' & item == '1') %>%
  ggplot(aes(x = date, y = sales, colour = set)) +
  geom_line() +
  geom_line(data = results1, aes(x = date, y = mean), colour = 'blue') +
  geom_line(data = results2, aes(x = date, y = mean), colour = 'green') +
  geom_line(data = results4, aes(x = date, y = mean), colour = 'red') +
  geom_line(data = results6, aes(x = date, y = mean), colour = 'yellow') +
  geom_line(data = results7, aes(x = date, y = mean), colour = 'pink')








#======================================================

SeasonalityTest <- function(input, ppy){
  #Used to determine whether a time series is seasonal
  tcrit <- 1.645
  if (length(input)<3*ppy){
    test_seasonal <- FALSE
  }else{
    xacf <- acf(input, plot = FALSE)$acf[-1, 1, 1]
    clim <- tcrit/sqrt(length(input)) * sqrt(cumsum(c(1, 2 * xacf^2)))
    test_seasonal <- ( abs(xacf[ppy]) > clim[ppy] )

    if (is.na(test_seasonal)==TRUE){ test_seasonal <- FALSE }
  }

  return(test_seasonal)
}


naive_seasonal <- function(input, fh){
  #Used to estimate Seasonal Naive
  frcy <- frequency(input)
  frcst <- naive(input, h=fh)$mean
  if (frcy>1){
    frcst <- head(rep(as.numeric(tail(input,frcy)), fh), fh) + frcst - frcst
  }
  return(frcst)
}


Theta.classic <- function(input, fh){
  #Used to estimate Theta classic

  #Set parameters
  wses <- wlrl<-0.5 ; theta <- 2
  #Estimate theta line (0)
  observations <- length(input)
  xt <- c(1:observations)
  xf <- c((observations+1):(observations+fh))
  train <- data.frame(input=input, xt=xt)
  test <- data.frame(xt = xf)

  estimate <- lm(input ~ poly(xt, 1, raw=TRUE))
  thetaline0In <- as.numeric(predict(estimate))
  thetaline0Out <- as.numeric(predict(estimate,test))

  #Estimate theta line (2)
  thetalineT <- theta*input+(1-theta)*thetaline0In
  sesmodel <- ses(thetalineT, h=fh)
  thetaline2In <- sesmodel$fitted
  thetaline2Out <- sesmodel$mean

  #Theta forecasts
  forecastsIn <- (thetaline2In*wses)+(thetaline0In*wlrl)
  forecastsOut <- (thetaline2Out*wses)+(thetaline0Out*wlrl)

  #Zero forecasts become positive
  for (i in 1:length(forecastsOut)){
    if (forecastsOut[i]<0){ forecastsOut[i]<-0 }
  }

  output=list(fitted = forecastsIn, mean = forecastsOut,
              fitted0 = thetaline0In, mean0 = thetaline0Out,
              fitted2 = thetaline2In, mean2 = thetaline2Out)

  return(output)
}

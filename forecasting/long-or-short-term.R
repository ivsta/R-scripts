library(tidyverse)
library(extdplyr)

train_start <- '2016-01-01' %>% as.Date()
test_start <- '2018-01-29' %>% as.Date()
test_end_daily <- '2018-04-29' %>% as.Date()
test_end_weekly <- '2018-10-28' %>% as.Date()


get_dates <- function(test_end, freq){
  tibble(date = seq(train_start, test_end, by = freq)
         , value = 1) %>%
    mutate(period = ifelse(date < test_start, 'train', 'test'))
}


test_end_list <- c(test_end_daily, test_end_weekly)
freq_list <- c('1 day', '1 week')

dates <- map2(test_end_list, freq_list, get_dates)

dates[[1]] %>%
  ggplot(aes(x = date, y = value, fill = period)) +
  geom_col(width = 1)

dates[[2]] %>%
  ggplot(aes(x = date, y = value, fill = period)) +
  geom_col(width = 7)


dates %>% map(pct_routine, period, ret_name = 'prop')
# [[1]]
# # A tibble: 2 x 2
# period  prop
# <chr>  <dbl>
#   1 test   0.107
# 2 train  0.893
#
# [[2]]
# # A tibble: 2 x 2
# period  prop
# <chr>  <dbl>
#   1 test   0.264
# 2 train  0.736

dates %>% map(~filter(., period == 'test') %>% count())
# [[1]]
# # A tibble: 1 x 1
# n
# <int>
#   1    91
#
# [[2]]
# # A tibble: 1 x 1
# n
# <int>
#   1    39

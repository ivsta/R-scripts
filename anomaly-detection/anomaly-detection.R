# devtools::install_github('twitter/AnomalyDetection')
library(AnomalyDetection)
library(tidyverse)
library(magrittr)

help(AnomalyDetectionTs)
help(AnomalyDetectionVec)

data(raw_data)
raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
raw_data %<>% as_tibble() %<>% mutate(timestamp = as.POSIXct(timestamp))

raw_data %>% str()
raw_data %>% summary()

res <- AnomalyDetectionTs(raw_data, max_anoms = 0.02, direction = 'both'
                          , plot = FALSE)

res$anoms$timestamp <- as.POSIXct(res$anoms$timestamp)
res$anoms %>% str()
res$anoms %>% summary()

raw_data %>%
ggplot(aes(x = timestamp, y = count)) +
  geom_line(data = raw_data, aes(timestamp, count), color = 'blue') +
  geom_point(data = res$anoms, aes(timestamp, anoms), color = 'red')

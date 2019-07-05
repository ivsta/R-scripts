# This script turns a classifcation problem (vehicle dataset on UCI) into a
# contextual bandit one, following approach in these papers.
#
# https://arxiv.org/pdf/1103.4601.pdf
# https://arxiv.org/pdf/1802.03493.pdf

library(tidyverse)
library(magrittr)
library(purrr)
library(caret)
library(data.table)

dat_files <- list.files('~/vehicle-dataset', full.names = TRUE)



read_file <- function(file){
  file %>%
    read_delim(delim = ' ', col_names = FALSE) %>%
    mutate(Y = X19 %>% as.factor()) %>%
    select(-X19)
}




dat <- map_dfr(dat_files, read_file) %>%  select(-X20)

dat %>% dim()
# [1] 846  19

dat %>% glimpse()

dat %>% summary()

(k <- dat %>% select(Y) %>% n_distinct())
# [1] 4



not_gate <- function(x){
  x * (1 - x) + (1 - x)
}



dummy_transf <- dummyVars(~Y, dat, sep = '_')
y_dummies <- predict(dummy_transf, dat) %>% as_tibble()
y_loss <- y_dummies %>% mutate_all(not_gate)

names(y_loss) <- names(y_loss) %>% str_replace('Y', 'loss')






set.seed(666)
action <- sample(dat %>% .$Y %>% unique(), size = nrow(dat), replace = TRUE) %>%
  enframe(name = NULL) %>%
  rename(a = value)



dat_transform <- dat %>%
  select(-Y) %>%
  bind_cols(y_loss) %>%
  bind_cols(action)

dat_transform %>% glimpse()



dat_partial <- dat_transform %>%
  mutate(loss = NA) %>%
  mutate(loss = ifelse(a == 'bus', loss_bus, loss)) %>%
  mutate(loss = ifelse(a == 'opel', loss_bus, loss)) %>%
  mutate(loss = ifelse(a == 'saab', loss_bus, loss)) %>%
  mutate(loss = ifelse(a == 'van', loss_bus, loss)) %>%
  select(-loss_bus, -loss_opel, -loss_saab, -loss_van)

dat_partial %>% glimpse()


dat_partial %>%
  group_by(a) %>%
  summarise(n = n()) %>%
  mutate(p = n / sum(n))

# a         n     p
#<fct> <int> <dbl>
# 1 bus     211 0.249
# 2 opel    216 0.255
# 3 saab    214 0.253
# 4 van     205 0.242



dat_partial %>%
  group_by(loss) %>%
  summarise(n = n()) %>%
  mutate(p = n / sum(n))

# loss     n     p
# <dbl> <int> <dbl>
# 1     0   218 0.258
# 2     1   628 0.742


dat_partial %>% fwrite('~/vehicle-dataset/dat_as_cb.csv')

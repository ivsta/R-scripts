library(tidyverse)
library(magrittr)
library(purrr)
library(caret)


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





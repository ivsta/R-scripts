library(tidyverse)
library(magrittr)

iris %>% as_tibble() %>% glimpse

set.seed(666)

iris_train <- iris %>%
  as_tibble() %>% 
  sample_n(100)

iris_test <- iris %>%
  setdiff(iris_train)

iris %>% nrow
iris_train %>% nrow
iris_test %>% nrow

x.train <- iris_train %>% 
  select(-Species)

x.test <- iris_test %>% 
  select(-Species)

s <- svd( x.train, nu = 2, nv = 2 )

x.test.embeddings  <- t(s$v) %*% as.matrix(t(x.test))

t(x.test.embeddings) %*% t(s$v) %>% head

x.test %>% head

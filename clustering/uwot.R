# https://jlmelville.github.io/uwot/umap-examples.html
# https://www.kaggle.com/arjunbhasin2013/ccdata

# devtools::install_github("jlmelville/uwot")
# devtools::install_github("jlmelville/smallvis/smallvis")

library(uwot)
library(janitor)
library(RcppAnnoy)
library(tidyverse)
library(ggthemes)


source_https <- function(u, unlink.tmp.certs = FALSE) {
  # load package
  require(RCurl)

  # read script lines from website using a security certificate
  if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
  script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
  if(unlink.tmp.certs) unlink("cacert.pem")

  # parase lines and evealuate in the global environement
  eval(parse(text = script), envir= .GlobalEnv)
}

source_https('https://raw.githubusercontent.com/jlmelville/uwot/master/R/neighbors.R')

#-----------------
# iris 1
#-----------------

iris_umap <- umap(iris, n_neighbors = 50, alpha = 0.5, init = "random")

# Load mnist from somewhere, e.g.
# devtools::install_github("jlmelville/snedata")
mnist <- snedata::download_mnist()

# Use a specific number of threads
mnist_umap <- umap(mnist, n_neighbors = 15, min_dist = 0.001, verbose = TRUE, n_threads = 8)

# Use a different metric
mnist_umap_cosine <- mnist %>%
    umap(n_neighbors = 15, metric = "cosine", min_dist = 0.001, verbose = TRUE, n_threads = 8)

# Supervised dimension reduction
mnist_umap_s <- mnist %>%
    umap(n_neighbors = 15, min_dist = 0.001, verbose = TRUE, n_threads = 8,
                     y = mnist$Label, target_weight = 0.5)

# Add new points to an existing embedding
mnist_train <- head(mnist, 60000)
mnist_test <- tail(mnist, 70000)

# You must set ret_model = TRUE to return extra data we need
mnist_train_umap <- umap(mnist_train, verbose = TRUE, ret_model = TRUE, n_threads = 8)
mnist_test_umap <- umap_transform(mnist_test, mnist_train_umap, verbose = TRUE, n_threads = 8)


#-----------------
# iris 2
#-----------------

iris_umap <- umap(iris)

squash <- function(x, sd = 1e-4) {
  scale(x, scale = apply(x, 2, stats::sd) / sd)
}

# For iris only
iris_tsne <- smallvis::smallvis(iris, Y_init = squash(iris_umap), perplexity = 15)

# For all other datasets
mnist_raw <- read_csv("https://pjreddie.com/media/files/mnist_train.csv", col_names = FALSE)
mnist_raw[1:10, 1:10]

mnist <- mnist_raw
mnist_umap <- umap(mnist, verbose = TRUE, n_threads = 8)

mnist_tsne <- Rtsne::Rtsne(as.matrix(mnist[, -785]), perplexity = 15, Y_init = squash(mnist_umap))

# Neighbor preservation values
kin <- annoy_nn(as.matrix(iris[, -5]), k = 15)$idx
kout <- annoy_nn(iris_umap, k = 15)$idx
mean(quadra::nbr_pres_knn(kin, kout, k = 15))


mnist_umap %>%
  as_tibble() %>%
  mutate(label = mnist$X1 %>% as.factor()) %>%
  ggplot(aes(x = V1, y = V2, colour = label)) +
  geom_point() +
  theme_minimal()




#-----------------
# credit cards
#-----------------

setwd("~/R-scripts/clustering")
cc <- 'CC_GENERAL.csv' %>% read_csv() %>% clean_names()

cc <- cc %>% replace_na(list(credit_limit = median(cc$credit_limit, na.rm = TRUE)
                        , minimum_payments = median(cc$minimum_payments, na.rm = TRUE)))

cc %>% glimpse()
cc %>% summary()




cc_umap <- cc %>%
  select(-cust_id) %>%
  umap(verbose = TRUE
       , n_threads = 8
       , scale = TRUE
       , spread = 1e-2
       , min_dist = 1)

cc_umap %>%
  as_tibble() %>%
  ggplot(aes(x = V1, y = V2)) +
  geom_point()

library(tidyverse)
library(purrr)
library(magrittr)
library(cluster)
library(fpc)

# Inputs
dt_file <- '~/R-scripts/clustering/train-station-entries-and-exits.csv'


#-------------------
# Load data
#-------------------

dt <- dt_file %>% read_csv(col_names = TRUE, na = '-')

names(dt) %<>%
  str_to_lower() %>%
  str_replace_all(':', '') %>%
  str_replace_all('  ', '_') %>%
  str_replace_all(' ', '_')

dt %<>%
  mutate(year = year %>% as.factor()) %>%
  mutate(station = station %>% as.factor())

dt %>% summary()

dt %<>% replace_na(list(entries_0600_to_1000 = 0
                        , exits_0600_to_1000 = 0
                        , entries_1000_to_1500 = 0
                        , exits_1000_to_1500 = 0
                        , entries_1500_to_1900 = 0
                        , exits_1500_to_1900 = 0
                        , entries_1900_to_0600 = 0
                        , exits_1900_to_0600 = 0
                        , entries_24_hours = 0
                        , exits_24_hours = 0))




#-------------------
# Prepare data for clustering
#-------------------

dt_2018 <- dt %>% filter(year == 2018) %>% select(-year, -station)

dist_matrix <- dt_2018 %>% as.matrix() %>% dist()


#-------------------
# Do clustering
#-------------------

# Dendrogram
hc <- hclust(dist_matrix)
plot(hc)


# kmeans - Compute and plot wss for k = 1 to k = 12.
get_tot_withinss <- function(nc){
  tot_withinss <- kmeans(dist_matrix, nc) %>% .$tot.withinss
  return(tibble(nc, tot_withinss))
}

set.seed(666)
seq(1, 12) %>%
  map_dfr(get_tot_withinss) %>%
  ggplot(aes(x = nc, y = tot_withinss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 12))

num_clus <- 4

fit <- kmeans(dist_matrix, num_clus)
plotcluster(dt_2018, fit$cluster)

list_stations <- function(ind){
  tibble(cluster_id = fit$cluster) %>%
    mutate(station = dt %>% filter(year == 2018) %>% .$station) %>%
    filter(cluster_id == ind) %>%
    head()
}

seq(1:num_clus) %>% map(list_stations)

library(tidyverse)
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

dt_2018 <- dt %>% filter(year == 2018) %>% select(-year)

dist_matrix <- dt_2018 %>% as.matrix() %>% dist()


# Dendrogram
hc <- hclust(dist_matrix)
plot(hc)



# kmeans
fit <- kmeans(dist_matrix, 4)
plotcluster(dt, fit$cluster)



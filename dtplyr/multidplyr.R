library(dplyr)
library(multidplyr)
library(nycflights13)


flights1 <- partition(flights, flight)
#> Initialising 7 core cluster.
flights2 <- summarise(flights1, dep_delay = mean(dep_delay, na.rm = TRUE))
flights3 <- collect(flights2)
#------------------------------------------------------------------------------
# https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
#------------------------------------------------------------------------------

library(ggplot2)
library(ggthemes)
library(dplyr)
library(dtplyr)
library(nycflights13)

theme_set(theme_fivethirtyeight())


dim(flights)
#> [1] 336776     19

head(flights)
class(flights)

flights <- flights %>% tbl_dt(flights)
glimpse(flights)




#------------------
# filter
#------------------
filter(flights, month == 1, day == 1) # dplyr
# flights[flights$month == 1 & flights$day == 1, ] # Base-R
# flights[month == 1 & day == 1] # data.table

#------------------
# slice
#------------------
slice(flights, 1:10)


#------------------
# arrange
#------------------
arrange(flights, year, month, day)

arrange(flights, desc(arr_delay))
# flights[order(flights$year, flights$month, flights$day), ] # Base-R
# flights[order(flights$arr_delay, decreasing = TRUE), ] # Base-R
# flights[order(-flights$arr_delay), ] # Base-R
# flights[order(-arr_delay)] # data.table


#------------------
# select
#------------------
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))
select(flights, tail_num = tailnum)


#------------------
# rename
#------------------
rename(flights, tail_num = tailnum)


#------------------
# distinct
#------------------
distinct(flights, tailnum)
distinct(flights, origin, dest)


#------------------
# mutate
#------------------
mutate(flights
       , gain = arr_delay - dep_delay
       , speed = distance / air_time * 60)

mutate(flights
       , gain = arr_delay - dep_delay
       , gain_per_hour = gain / (air_time / 60))

transmute(flights
          , gain = arr_delay - dep_delay
          , gain_per_hour = gain / (air_time / 60))


#------------------
# summarise
#------------------
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))


#------------------
# sample
#------------------
sample_n(flights, 10)
sample_frac(flights, 0.01)


#===================================
# Grouped operations
#===================================
by_tailnum <- group_by(flights, tailnum)

delay <- summarise(by_tailnum, count = n()
                   , dist = mean(distance, na.rm = TRUE)
                   , delay = mean(arr_delay, na.rm = TRUE))

delay <- filter(delay, count > 20, dist < 2000)

ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()

destinations <- group_by(flights, dest)
summarise(destinations
          , planes = n_distinct(tailnum)
          , flights = n())

daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))


#------------------
# chaining
#------------------

flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ) %>%
  filter(arr > 30 | dep > 30)
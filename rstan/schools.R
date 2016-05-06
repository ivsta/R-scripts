library(rstan)
library(data.table)

setwd("~/R_scripts/rstan")

schools <- fread('schools.csv')

J <- nrow(schools)
y <- schools[, estimate]
sigma <- schools[, sd]

schools_fit <- stan(file = 'schools.stan', data = c('J', 'y', 'sigma'), iter = 1000, chains = 4)
print(schools_fit)
plot(schools_fit)

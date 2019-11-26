################################################################
# Bishop - Pattern Recocgnition And Machine Learning
# Figure 3.7
################################################################

library(rstan)
library(coda)
library(shinystan)
library(tidyverse)
library(mvtnorm)
library(reshape2)
options(mc.cores = 8)


######################
# Setup data
######################

num_instances <- 1e3
a_0 <- -0.3
a_1 <- 0.5
truth <- tibble(a_0 = a_0, a_1 = a_1)
sigma <- 0.2



alpha <- 2
beta <- (1/sigma)^2

df <- tibble(x = runif(num_instances, min = -1, max = 1),
             noise = rnorm(num_instances, mean = 0, sd = sigma),
             y = a_0 + a_1 * x +  noise)

df %>%
  ggplot(aes(x = x, y = y)) +
  geom_point()


# Run lm for later comparison
modlm <- lm(t ~ x, data = df)
summary(modlm)



w_len <- 200
w_0 <- seq(-1, 1, length.out = w_len)
w_1 <- seq(-1, 1, length.out = w_len)
w_array <- expand.grid('w_0' = w_0, 'w_1' = w_1)



##############################################
# Plot of the prior distribution in w space
##############################################

get_gaussian <- function(w_0, w_1){
  tibble(w_0 = w_0, w_1 = w_1, t = dmvnorm(cbind(w_0, w_1)))
}

w_df_0 <- map2_dfr(.x = w_array$w_0, .y = w_array$w_1, .f = get_gaussian)

w_df_0 %>%
  ggplot(aes(x = w_0, y = w_1, fill = t)) +
  geom_raster() +
  coord_equal() +
  scale_fill_gradientn(colours = rainbow(4)) +
  scale_x_continuous(name = 'w0', expand = c(0, 0)) +
  scale_y_continuous(name = 'w1', expand = c(0, 0)) +
  theme_bw() +
  theme(legend.position = 'None')


##############################################
# Samples of the function y(x, w)
##############################################

set.seed(666)
sample_func <- w_df_0 %>% sample_n(size = 6, replace = TRUE)

tibble(x = seq(-1, 1, length.out = 10), y = seq(-1, 1, length.out = 10)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_blank() +
  geom_abline(slope = sample_func$w_0[[1]], intercept = sample_func$w_1[[1]]) +
  geom_abline(slope = sample_func$w_0[[2]], intercept = sample_func$w_1[[2]]) +
  geom_abline(slope = sample_func$w_0[[3]], intercept = sample_func$w_1[[3]]) +
  geom_abline(slope = sample_func$w_0[[4]], intercept = sample_func$w_1[[4]]) +
  geom_abline(slope = sample_func$w_0[[5]], intercept = sample_func$w_1[[5]]) +
  geom_abline(slope = sample_func$w_0[[6]], intercept = sample_func$w_1[[6]])



#######################
# After 1 data point
#######################

df[1, c('x', 'y')]

tibble(x = seq(-1, 1, length.out = 10), y = seq(-1, 1, length.out = 10)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_blank() +
  geom_point(data = df[1,], aes(x = x, y = y), colour = 'blue', size = 3)


#####################################################################
# Likelihood function p(t|x, w) for this data point
#####################################################################

w_matrix_1 <- matrix(0, length(w_0), length(w_1))

for (i in 1:length(w_0)){
  a <- w_0
  b <- w_1[i]
  w_matrix_1[, i] <- dmvnorm(x = cbind(a, b), mean = c(b*df[1,]$x, 0), sigma = diag(2) / beta)
}

w_df_1 <- w_matrix_1 %>%
  melt() %>%
  as_tibble() %>%
  mutate('w0' = (Var1 - 100) / 100, 'w1' = (Var2 - 100) / 100) %>%
  select(w0, w1, value)

ggplot() +
  geom_raster(data = w_df_1, aes(x = w0, y = w1, fill = value)) +
  coord_equal() +
  scale_fill_gradientn(colours = rainbow(4)) +
  scale_x_continuous(name = 'w0', expand = c(0, 0)) +
  scale_y_continuous(name = 'w1', expand = c(0, 0)) +
  theme_bw() +
  theme(legend.position = 'None') +
  geom_point(data = truth, aes(x = a_0, y = a_1), shape = 3, size = 4)




#########################################
# Bishop - Pattern Recocgnition And Machine Learning
# Figure 3.7
#########################################

library(rstan)
library(coda)
library(shinystan)
library(tidyverse)
library(mvtnorm)
library(reshape2)
set.seed(666)
options(mc.cores = 8)


######################
# Setup data
######################

num_instances <- 1e3
a_0 <- -0.3
a_1 <- 0.5
sigma <- 0.2

alpha <- 2
beta <- (1/sigma)^2

df <- tibble(x = runif(num_instances, min = -1, max = 1),
             noise = rnorm(num_instances, mean = 0, sd = sigma),
             t = a_0 + a_1 * x +  noise)

df %>%
  ggplot(aes(x = x, y = t)) +
  geom_point()


# Run lm for later comparison
modlm <- lm(t ~ x, data = df)
summary(modlm)




#######################
# Plot prior
#######################

w_len <- 200
w_0 <- seq(-1, 1, length.out = w_len)
w_1 <- seq(-1, 1, length.out = w_len)
w_matrix <- matrix(0, length(w_0), length(w_1))

for (i in 1:length(w_0)) {
  a <- w_0
  b <- w_1[i]
  w_matrix[, i] <- dmvnorm(cbind(a, b))
}

w_matrix %>%
  melt() %>%
  as_tibble() %>%
  ggplot(aes(x = Var2 / w_len, y = Var1 / w_len, fill = value)) +
  geom_raster() +
  coord_equal() +
  scale_fill_gradientn(colours = rainbow(4)) +
  scale_x_continuous(name = 'w0') +
  scale_y_continuous(name = 'w1') +
  theme_bw() +
  theme(legend.position = 'None')


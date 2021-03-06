#########################################
# https://arxiv.org/abs/1802.09127
#########################################

library(rstan)
library(coda)
library(shinystan)
library(tidyverse)
set.seed(666)
options(mc.cores = 8)


######################
# Setup data
######################

num_instances <- 100
d <- 2

covariates <- replicate(d, rnorm(n = num_instances))
colnames(covariates) <- c('X1', 'X2')

# create the model matrix with intercept
context <- cbind(intercept = 1, covariates)

# create a normally distributed variable that is a function of the covariates
coefs <- c(5, 0.2, -1.5)
mu <- context %*% coefs
sigma <- 2
y <- rnorm(num_instances, mu, sigma)

# same as
# y = 5 + 0.2 * X1 - 1.5 * X2 + rnorm(N, mean = 0, sd = 2)

# Run lm for later comparison
modlm <- lm(y ~ ., data = as_tibble(context[, -1]))
summary(modlm)


######################
# Setup rstan
######################

# Create the data list object for stan inupt
dat <- list(n = num_instances, d = ncol(context), y = y, x = context)

str(dat)


# Create the stan model object using Stan's syntax
stanmodelcode = "
data {                        // Data block
	int<lower=1> n;            	// Sample size
	int<lower=1> d;            	// Dimension of model matrix
	matrix[n, d] context;       // Model Matrix
	vector[n] y;               	// Target variable
}

parameters {					        // Parameters block
	vector[d] beta;				      // Coefficient vector
	real<lower=0> sigma;		    // Error scale
}

model {							          // Model block
	vector[n] mu;
	mu = context * beta;				// Creation of linear predictor

	// priors
	beta ~ normal(0, 10);
	sigma ~ inv_gamma(2, 2);

	// likelihood
	y ~ normal(mu, sigma);
}
"


######################
# Run rstan
######################

fit <- stan(model_code = stanmodelcode, data = dat, iter = 1200, warmup = 200,
	thin = 10, chains = 3)

print(fit, digits_summary = 3, pars = c('beta', 'sigma'),
	probs = c(.025, .5, .975))


######################
# Compare results
######################

# True values
coefs
# [1]  5.0  0.2 -1.5


# MLE fit
modlm$coefficients %>% round(2)
# (Intercept)          X1          X2
#        4.82        0.15       -1.44


# Bayesian fit
betas <- rstan::extract(fit, pars = 'beta')$beta

betas %>%
	as_tibble() %>%
	summarise_all(mean)
#      V1    V2    V3
#   <dbl> <dbl> <dbl>
# 1  4.83 0.151 -1.45


######################
# Plots
######################

# traceplots
rstan::traceplot(fit, pars = c('beta[1]'), inc_warmup = TRUE)
rstan::traceplot(fit, pars = c('beta[2]'), inc_warmup = TRUE)
rstan::traceplot(fit, pars = c('beta[3]'), inc_warmup = TRUE)
rstan::traceplot(fit, pars = c('sigma'), inc_warmup = TRUE)


# coda
betas.mcmc <- as.mcmc(betas)

par(mar = c(1, 1, 1, 1))
plot(betas.mcmc)


# shinystan
my_sso <- launch_shinystan(fit)

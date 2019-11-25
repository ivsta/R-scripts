#----------------------------------------------------------------------------------------
# https://sites.google.com/a/umich.edu/micl/miscfiles/IntroBayes.pdf
#----------------------------------------------------------------------------------------

library(rstan)
library(coda)


# set seed for replicability
set.seed(8675309)

# create a N x k matrix of covariates
N = 250
K = 3
covariates = replicate(K, rnorm(n = N))
colnames(covariates) = c('X1', 'X2', 'X3')

# create the model matrix with intercept
X = cbind(Intercept = 1, covariates)

# create a normally distributed variable that is a function of the covariates
coefs = c(5, 0.2, -1.5, 0.9)
mu = X %*% coefs
sigma = 2
y <- rnorm(N, mu, sigma)

# same as
# y = 5 + .2*X1 - 1.5*X2 + .9*X3 + rnorm(N, mean=0, sd=2)

# Run lm for later comparison; but go ahead and examine now if desired
modlm = lm(y ~ ., data = data.frame(X[, -1]))
summary(modlm)






# Create the data list object for stan inupt
dat = list(N = N, K = ncol(X), y = y, X = X)

str(dat)

# Create the stan model object using Stan's syntax
stanmodelcode = "
data {                        // Data block
	int<lower=1> N;            	// Sample size
	int<lower=1> K;            	// Dimension of model matrix
	matrix[N, K] X;            	// Model Matrix
	vector[N] y;               	// Target variable
}

/*
transformed data { }			    // Transformed data block. Not used presently.
*/

parameters {					        // Parameters block
	vector[K] beta;				      // Coefficient vector
	real<lower=0> sigma;		    // Error scale
}

model {							          // Model block
	vector[N] mu;
	mu = X * beta;				      // Creation of linear predictor

	// priors
	beta ~ normal(0, 10);
	sigma ~ cauchy(0, 5);		    // With sigma bounded at 0, this is half-cauchy

	// likelihood
	y ~ normal(mu, sigma);
}

/*
generated quantities { }		  // Generated quantities block. Not used presently.
*/
"


### Run the model and examine results ###
fit = stan(model_code = stanmodelcode, data = dat, iter = 12000, warmup = 2000,
	thin = 10, chains = 3)

# summary
print(fit
      , digits_summary = 3
      , pars = c('beta','sigma')
      , probs = c(.025, .5, .975))


summary(modlm)
# Call:
#   lm(formula = y ~ ., data = data.frame(X[, -1]))
#
# Residuals:
#   Min      1Q  Median      3Q     Max
# -6.8632 -1.4696  0.2431  1.4213  5.0406
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  4.89777    0.12845  38.131  < 2e-16 ***
#   X1           0.08408    0.12960   0.649    0.517
# X2          -1.46861    0.12615 -11.642  < 2e-16 ***
#   X3           0.81959    0.12065   6.793 8.21e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 2.021 on 246 degrees of freedom
# Multiple R-squared:  0.4524,	Adjusted R-squared:  0.4458
# F-statistic: 67.75 on 3 and 246 DF,  p-value: < 2.2e-16


# Visualize
rstan::traceplot(fit, pars = c('beta[4]'), inc_warmup = TRUE)



betas = extract(fit, pars='beta')$beta
betas.mcmc = as.mcmc(betas)

par(mar=c(1,1,1,1))
plot(betas.mcmc)



library(shinystan)
my_sso <- launch_shinystan(fit)




# extract the simulated draws from the posterior and note the number for nsim
theta = extract(fit)
betas = theta$beta
sigmas = theta$sigma
nsims = length(theta$sigma)
# produce the replications and inspect
yRep = sapply(1:nsims, function(s) rnorm(250, X%*%betas[s,], sigmas[s]))
str(yRep)
##  num [1:250, 1:3000] 6.92 7.69 3.67 1.37 7.56 ...


min_rep = apply(yRep, 2, min)
min_y = min(y)
hist(min_rep, main='');
abline(v=min_y)
c(mean(min_rep), min_y)
## [1] -2.822617 -6.056495
prop.table(table(min_rep>min_y))
##
## FALSE  TRUE
## 0.011 0.989
sort(y)[1:5]
## [1] -6.0564952 -3.2320527 -2.6358579 -2.1649084 -0.8366149

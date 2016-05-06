#----------------------------------------------------------------------------------------
# https://sites.google.com/a/umich.edu/micl/miscfiles/IntroBayes.pdf
#----------------------------------------------------------------------------------------

# set seed for replicability
set.seed(8675309)

# create a N x k matrix of covariates
N = 250 
K = 3
covariates = replicate(K, rnorm(n=N))
colnames(covariates) = c('X1', 'X2', 'X3')

# create the model matrix with intercept
X = cbind(Intercept=1, covariates)

# create a normally distributed variable that is a function of the covariates
coefs = c(5,.2,-1.5,.9) 
mu = X %*% coefs
sigma = 2
y <- rnorm(N, mu, sigma)

# same as
# y = 5 + .2*X1 - 1.5*X2 + .9*X3 + rnorm(N, mean=0, sd=2)

# Run lm for later comparison; but go ahead and examine now if desired
modlm = lm(y~., data=data.frame(X[,-1])) # summary(modlm)
summary(modlm)






# Create the data list object for stan inupt
dat = list(N=N, K=ncol(X), y=y, X=X)


# Create the stan model object using Stan's syntax
stanmodelcode = "
data {                        // Data block
  int<lower=1> N;             // Sample size
  int<lower=1> K;             // Data block
  matrix[N, K] X; 
  vector[N] y;
}
/*
transformed data { }
*/
parameters {
vector[K] beta; real<lower=0> sigma;
}
model {
vector[N] mu; mu<-X* beta;
// priors
beta ~ normal(0, 10); sigma ~ cauchy(0, 5);
// likelihood
y ~ normal(mu, sigma); }
/*
generated quantities { }
*/
"
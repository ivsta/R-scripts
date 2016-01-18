#=====================================================
# http://www.magesblog.com/2015/05/hello-stan.html
#=====================================================

library(rstan)
stanmodelcode <- "
data {
int<lower=0> N;
int<lower=0, upper=1> y[N];
}
parameters {
real<lower=0, upper=1> theta;
}
model {
theta ~ beta(1, 1);
for (n in 1:N)
y[n] ~ bernoulli(theta);
}
"
N <- 5
y <- rep(0,5)
dat <- list(N = N, y = y); 
fit <- stan(model_code = stanmodelcode, 
            model_name = "Longley-Cook", 
            data = dat) 
# Review output
fit
## Inference for Stan model: Longley-Cook.
## 4 chains, each with iter=2000; warmup=1000; thin=1; 
## post-warmup draws per chain=1000, total post-warmup draws=4000.
## 
##        mean se_mean   sd 2.5%   25%   50%   75% 97.5% n_eff Rhat
## theta  0.14    0.00 0.12  0.0  0.05  0.11  0.21  0.45  1189    1
## lp__  -3.44    0.02 0.80 -5.7 -3.65 -3.14 -2.93 -2.87  1086    1
## 
## Samples were drawn using NUTS(diag_e) at Sat May  9 13:45:50 2015.
## For each parameter, n_eff is a crude measure of effective sample size,
## and Rhat is the potential scale reduction factor on split chains (at 
## convergence, Rhat=1).
traceplot(fit, 'theta')
plot(fit)

theta <- extract(fit, 'theta')
theta <- unlist(theta, use.names=FALSE)
summary(theta)
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000418 0.0495600 0.1118000 0.1436000 0.2045000 0.8277000
hist(theta, xlim=c(0,1), freq=FALSE)
curve(dbeta(x, 1, 1),
      from=0, to=1,
      add=TRUE, col='green', lwd=1.5)
curve(dbeta(x, sum(y)+1, N-sum(y)+1),
      from=0, to=1,
      add=TRUE, col='blue', lwd=1.5)
legend("topright", lty=1,
       legend = c("Prior", "Posterior"), 
       col=c("green", "blue"), bty="n")



library(shinyStan)
launch_shinystan(fit)

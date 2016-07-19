#----------------------------------------------------------
# http://data.library.virginia.edu/is-r-squared-useless/
#----------------------------------------------------------

x <- 1:20                        # independent variable
set.seed(1)                      # for reproducibility
y <- 2 + 0.5*x + rnorm(20,0,3)   # dependent variable; function of x with random error
mod <- lm(y~x)                   # simple linear regression
summary(mod)$r.squared           # request just the r-squared value

f <- mod$fitted.values       # extract fitted (or predicted) values from model
mss <- sum((f - mean(f))^2)  # sum of squared fitted-value deviations
tss <- sum((y - mean(y))^2)  # sum of squared original-value deviations
mss/tss                      # r-squared


r2.0 <- function(sig){
  x <- seq(1,10,length.out = 100)        # our predictor
  y <- 2 + 1.2*x + rnorm(100,0,sd = sig) # our response; a function of x plus some random noise
  summary(lm(y ~ x))$r.squared           # print the R-squared value
}

sigmas <- seq(0.5,20,length.out = 20)
rout <- sapply(sigmas, r2.0)             # apply our function to a series of sigma values
plot(rout ~ sigmas, type="b")


set.seed(1)
x <- rexp(50,rate=0.005)                     # our predictor is data from an exponential distribution
y <- (x-1)^2 * runif(50, min=0.8, max=1.2)   # non-linear data generation
plot(x,y)				     # clearly non-linear

summary(lm(y ~ x))$r.squared




x <- seq(1,10,length.out = 100)
set.seed(1)
y <- 2 + 1.2*x + rnorm(100,0,sd = 0.9)
mod1 <- lm(y ~ x)
summary(mod1)$r.squared
sum((fitted(mod1) - y)^2)/100 # Mean squared error

x <- seq(1,2,length.out = 100)       # new range of x
set.seed(1)
y <- 2 + 1.2*x + rnorm(100,0,sd = 0.9)
mod1 <- lm(y ~ x)
summary(mod1)$r.squared
sum((fitted(mod1) - y)^2)/100        # Mean squared error
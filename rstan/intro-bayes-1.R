#----------------------------------------------------------------------------------------
# https://sites.google.com/a/umich.edu/micl/miscfiles/IntroBayes.pdf
#----------------------------------------------------------------------------------------
library(ggplot2)
library(data.table)
library(plotly)

#--------------------
# A Hands-on example
#--------------------
drive = c('texting','texting','texting','not','not', 'texting','texting','not','not','texting')

# convert to numeric, arbitrarily picking texting=1, not=0
driveNum = ifelse(drive=='texting', 1, 0)    
N = length(drive)                            # sample size
nTexting = sum(drive=='texting')             # number of drivers texting
nNot = sum(drive=='not')                     # number of those not

x1 = rbinom(1000, size=10, p=.5)
x2 = rbinom(1000, size=10, p=.85)
mean(x1); hist(x1)
mean(x2); hist(x2)

# Theta
theta = seq(from=1/(N+1), to=N/(N+1), length=10)

#-----------------------
# Prior distribution
#-----------------------
# uniform
# pTheta = dunif(theta)

# triangular as in Kruschke
pTheta = pmin(theta, 1-theta) # beta prior with mean = .5

# beta prior with mean = .5
# pTheta = dbeta(theta, 10, 10)

pTheta = pTheta/sum(pTheta) # Normalize so sum to 1


qplot(pTheta, geom = 'density')

#-----------------------
# Likelihood
#-----------------------
pDataGivenTheta = choose(N, nTexting) * theta^nTexting * (1-theta)^nNot
qplot(pDataGivenTheta, geom = 'density')

#-----------------------
# Posterior
#-----------------------
# first we calculate the denominator from Bayes theorem; this is the marginal
# probability of y
pData = sum(pDataGivenTheta*pTheta)

pThetaGivenData = pDataGivenTheta*pTheta / pData # Bayes theorem


results <- data.table(theta
           , prior = pTheta
           , likelihood = pDataGivenTheta
           , posterior = pThetaGivenData)

round(results, 3)

results_melt <- melt(results, id.vars = 'theta')

p <- ggplot(data = results_melt, aes(x = theta, y = value, colour = variable)) +
  geom_line() + geom_point() + 
  theme_minimal() + theme(legend.title = element_blank())

ggplotly(p)

posteriorMean = sum(pThetaGivenData*theta)
posteriorMean
# 0.5623611








# 
# require(ggplot2)
# x <- seq(0, 1, len = 100)
# p <- qplot(x, geom = "blank")
# stat <- stat_function(aes(x = x, y = ..y..), fun = dbeta, colour="red", n = 100,
#                       args = list(shape1 = 10, shape2 = 10))
# p + stat





#--------------------
# Binomial Likelihood Example
#--------------------

x1 = rbinom(1000, size=10, p=.5) x2 = rbinom(1000, size=10, p=.85)
binomLL = function(theta, x) { -sum(dbinom(x, size=10, p=theta, log=T))
}
optimize(binomLL, x=x1, lower=0, upper=1); mean(x1)
## $minimum
## [1] 0.5043001
##
## $objective
## [1] 1902.557
## [1] 5.043
optimize(binomLL, x=x2, lower=0, upper=1); mean(x2)
## $minimum
## [1] 0.8568963
##
## $objective
## [1] 1438.786
## [1] 8.569
#=====================================================
# http://xavier-fim.net/packages/ggmcmc/
#=====================================================
library(ggmcmc)
data(radon)
s.radon.short <- radon$s.radon.short
S <- ggs(s.radon.short)
str(S)
ggmcmc(S, file = '~/R scripts/output/ggmc_output.pdf')

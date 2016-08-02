#--------------------------------------------------
# https://r-dir.com/blog/2014/03/cdfs-in-r.html
#--------------------------------------------------

# Since we're generating data, set the seed.
set.seed(1984)

# Create the data.
a <- rnorm(10000, 50.2, 1.5)
b <- rnorm(10000, 49.3, 0.92)
c <- rnorm(10000, 50.1, 0.5)

# Load the necessary packages.
library("reshape2")
library("plyr")
library("ggplot2")

# Create the data frame.
ggdata <- data.frame(a, b, c)

# Melt the data frame
ggdata <- melt(ggdata)

# Set the data frame, & add ecdf() data.
ggdata <- ddply(ggdata, .(variable), transform, ecd=ecdf(value)(value))

# Create the histogram using ggplot.
hist <- ggplot(ggdata, aes(x=value, fill=variable)) + geom_histogram(alpha=0.2, position="identity")

# Generate the histogram.
hist

# Create the CDF using ggplot.
cdf <- ggplot(ggdata, aes(x=value)) + stat_ecdf(aes(colour=variable))

# Generate the CDF.
cdf
cdf + facet_wrap(~ variable)

# Create the kde using ggplot.
kde <- ggplot(ggdata, aes(x=value)) + geom_density(aes(colour=variable))

# Generate the kde
kde
kde + facet_wrap(~ variable)

ggplot(ggdata, aes(x=value)) + 
  geom_density(aes(colour=variable)) + 
  stat_ecdf(aes(colour=variable), linetype = 'dashed') +
  facet_wrap(~ variable)

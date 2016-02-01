#-------------------------------------------------------------------------------------
# https://tgmstat.wordpress.com/2013/11/28/computing-and-visualizing-pca-in-r/
#-------------------------------------------------------------------------------------

library(caret)
library(ggplot2)

head(iris[,1:4])

trans <- preProcess(iris[,1:4], method=c("BoxCox", "center", "scale", "pca"))
PC <- predict(trans, iris[,1:4])
head(PC)

iris_PC <- data.table(iris, PC)

ggplot(data = iris_PC, aes(x = PC1, y = PC2, colour = Species)) + 
  geom_point() + theme_bw()



#-------------------------------------------------------------------------------------
# Try density equalisation
#-------------------------------------------------------------------------------------
source('/Users/wilsonpok/R_scripts/Rcartogram/from_dimitri/predict.cartogram.R')
source('/Users/wilsonpok/R_scripts/Rcartogram/from_dimitri/density_equalisation.R')

res <- density_equalisation(x_in = iris_PC[, PC1], y_in = iris_PC[, PC2], grid_size = 0.75, sigma = 1, sea = 0)

iris_res <- data.table(iris, res)

ggplot(data = iris_res, aes(x = x, y = y, colour = Species)) + 
  geom_jitter(width = 0.3, height = 0.3) + theme_bw()

ggplot(data = iris_res, aes(x = x, y = y, colour = Sepal.Length)) + 
  geom_jitter(width = 0.3, height = 0.3) + theme_bw() +
  scale_colour_continuous(low = 'orange', high = 'blue')


# Quantile binning
quantile_x <- quantile(iris_res[, x])
quantile_y <- quantile(iris_res[, y])

ggplot(data = iris_res, aes(x = x, y = y, colour = Species)) + 
  geom_jitter(width = 0.3, height = 0.3) + 
  geom_vline(xintercept = quantile_x[1]) +
  geom_vline(xintercept = quantile_x[2]) +
  geom_vline(xintercept = quantile_x[3]) +
  geom_vline(xintercept = quantile_x[4]) +
  geom_hline(yintercept = quantile_y[1]) +
  geom_hline(yintercept = quantile_y[2]) +
  geom_hline(yintercept = quantile_y[3]) +
  geom_hline(yintercept = quantile_y[4]) +
  theme_bw()

# More sensible binning
ggplot(data = iris_res, aes(x = x, y = y, colour = Species)) + 
  geom_jitter(width = 0.3, height = 0.3) + 
  geom_vline(xintercept = 0.4) +
  geom_vline(xintercept = 1.5) +
  geom_vline(xintercept = 3) +
  geom_hline(yintercept = 0.6) +
  geom_hline(yintercept = 1.5) +
  geom_hline(yintercept = 3.5) +
  theme_bw()

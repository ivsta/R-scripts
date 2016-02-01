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

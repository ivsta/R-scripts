library(data.table)
library(ggplot2)
library(Rcartogram)

filename = system.file("data", "uspop.dat", package = "Rcartogram")
pop = read.table(filename)

dim(pop) # 512 1024
head(pop)
summary(pop)

grid = cartogram(pop)
summary(grid$x)
summary(grid$y)

predict(grid, 3.1, 4.5)


# note that this is equivalent to using the interp application in
# the cart(ogram) distribution with inputs as 2.1 3.4
# i.e. echo "2.1 3.4" | interp 1024 512 output.dat
#        

# now predict lots of values.

new_x = runif(10000, 1, 1024)
new_y = runif(10000, 1, 512)

o = predict(grid, new_x, new_y)
head(o$x)
head(o$y)


#----------------------------------------------------------------



dt1 <- data.table(mtcars)
pop <- dt1[, list(disp, hp)]

ggplot(data = pop, aes(x = disp, y = hp)) +
  geom_point()

summary(pop)
# disp             hp       
# Min.   : 71.1   Min.   : 52.0  
# 1st Qu.:120.8   1st Qu.: 96.5  
# Median :196.3   Median :123.0  
# Mean   :230.7   Mean   :146.7  
# 3rd Qu.:326.0   3rd Qu.:180.0  
# Max.   :472.0   Max.   :335.0  


dim(pop) # 32  2

grid <- cartogram(pop)
summary(grid$x)
summary(grid$y)

transformed <- predict(grid, runif(1000, 1, 2), runif(1000, 1, 32))

tf <- data.table(x = transformed$x, y = transformed$y)

ggplot(data = tf, aes(x = x, y = y)) +
  geom_point()


# Try again using Dimitri's code
source('/Users/wilsonpok/R_scripts/Rcartogram/from_dimitri/predict.cartogram.R')

transformed <- predict.Cartogram(grid, runif(1000, 1, 2), runif(1000, 1, 32))

ggplot(data = data.table(transformed), aes(x = x, y = y)) +
  geom_point()

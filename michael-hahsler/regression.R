#' ---
#' title: "Introduction to Multiple Linear Regression in R"
#' author: "Michael Hahsler"
#' output:
#'  html_document:
#'    toc: true
#' ---

#' Additional material for the course "Introdction to Data Mining"
#'
#' ![CC](https://i.creativecommons.org/l/by/4.0/88x31.png)
#' This work is licensed under the
#' [Creative Commons Attribution 4.0 International License](http://creativecommons.org/licenses/by/4.0/). For questions please contact
#' [Michael Hahsler](http://michael.hahsler.net).
#'

set.seed(2000)

#' Load and shuffle data (flowers are in order by species)
data(iris)
x <- iris[sample(1:nrow(iris)),]
plot(x, col=x$Species)

#' Make the data a little messy and add a useless feature
x[,1] <- x[,1] + rnorm(nrow(x))
x[,2] <- x[,2] + rnorm(nrow(x))
x[,3] <- x[,3] + rnorm(nrow(x))
x <- cbind(x[,-5], useless = mean(x[,1]) + rnorm(nrow(x)), Species = x[,5])

plot(x, col=x$Species)
summary(x)
head(x)

#' Create some training and learning data
train <- x[1:100,]
test <- x[101:150,]

#' # (Multiple) Linear Regression
#' ## Introduction
#' Can we predict Petal.Width using the other variables?
#'
#' lm uses a formula interface see ?lm for description
model1 <- lm(Petal.Width ~ Sepal.Length
             + Sepal.Width + Petal.Length + useless,
             data = train)
model1
coef(model1)

#'Summary shows:
#'
#'* Which coefficients are significantly different from 0
#'* R-squared (coefficient of determination): Proportion of the variability of the dependent variable explained by the model. It is better to look at the adjusted R-square (adjusted for number of dependent vars.)

summary(model1)

#' ## Comparing Nested Models
#' We try simpler models
model2 <- lm(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length,
             data = train)
summary(model2)

#' No intercept
model3 <- lm(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length - 1,
             data = train)
summary(model3)

#' Very simple model
model4 <- lm(Petal.Width ~ Petal.Length -1,
             data = train)
summary(model4)


#' Compare *nested* models (Null hypothesis: all treatments=models have the same effect)
anova(model1, model2, model3, model4)
#' Models 1 is not significantly better than model 2. Model 2 is not significantly better than model 3. Model 3 is not significantly better than model 4! Use model 4 (simplest model)
#'
#' ## Finding the Best Model
#' Automatically looks for the smallest AIC= Akaike information criterion)
s1 <- step(lm(Petal.Width ~ . -Species, data=train))
summary(s1)

#' ## Model with Interaction Terms
#' What if two variables are only important together? Interaction terms
#' are modeled with * in the formula (they are literally multiplied)
model5 <- step(lm(Petal.Width ~ Sepal.Length * Sepal.Width * Petal.Length,
                  data = train))
summary(model5)
anova(model5, model4)
#' Model 5 is not significantly better than model 4
#'
#' ## Prediction
test[1:5,]
test[1:5,]$Petal.Width
predict(model4, test[1:5,])

#' Calculate the root-mean-square error (RMSE): less is better
RMSE <- function(predicted, true) mean((predicted-true)^2)^.5
RMSE(predict(model4, test), test$Petal.Width)

#' Compare predicted vs. actual values
plot(test[,"Petal.Width"], predict(model4, test),
     xlim=c(0,3), ylim=c(0,3), xlab = "actual", ylab = "predicted",
     main = "Petal.Width")
abline(0,1, col="red")
cor(test[,"Petal.Width"], predict(model4, test))

#' ## Using Nominal Variables
#' How do we incorporate nominal variables like Species?
model6 <- step(lm(Petal.Width ~ ., data=train))
model6
summary(model6)

#' is it better than model4?
anova(model6, model4)

RMSE(predict(model6, test), test$Petal.Width)
plot(test[,"Petal.Width"], predict(model6, test),
     xlim=c(0,3), ylim=c(0,3), xlab = "actual", ylab = "predicted",
     main = "Petal.Width")
abline(0,1, col="red")
cor(test[,"Petal.Width"], predict(model6, test))

#' # Alternative Regression Models
#' ## Regression Trees
#'
#' Many models we use for classification can also perform regression
#' to produce piece-wise predictors.
#' For example CART:
library(rpart)
library(rpart.plot)
model7 <- rpart(Petal.Width ~ ., data=train,
                control=rpart.control(cp=0.01))
model7
rpart.plot(model7)

RMSE(predict(model7, test), test$Petal.Width)
plot(test[,"Petal.Width"], predict(model7, test),
     xlim=c(0,3), ylim=c(0,3), xlab = "actual", ylab = "predicted",
     main = "Petal.Width")
abline(0,1, col="red")
cor(test[,"Petal.Width"], predict(model7, test))

#' __Note:__ This is not a nested model of the linear regressions so we cannot
#' do ANOVA to compare the models!
#'

#' ## Regularized Regression
#' LASSO and LAR try to reduce the number of parameters using a
#' regularization term (see `lars` in package lars and https://en.wikipedia.org/wiki/Elastic_net_regularization)

library(lars)

#' create a design matrix (with dummy variables and interaction terms).
#' `lm` did this automatically for us, but for this `lars` implementation
#' we have to do it manually.
x <- model.matrix(~ . + Sepal.Length*Sepal.Width*Petal.Length ,
                  data = train[, -4])
head(x)
y <- train[, 4]

model_lars <- lars(x, y)
summary(model_lars)
model_lars
plot(model_lars)
#' the plot shows how variables are added (from left to right to the model)
#'
#' find best model (using Mallows's Cp statistic, see https://en.wikipedia.org/wiki/Mallows's_Cp)
plot(model_lars, plottype = "Cp")
best <- which.min(model_lars$Cp)
coef(model_lars, s = best)

#' make predictions
x_test <- model.matrix(~ . + Sepal.Length*Sepal.Width*Petal.Length ,
                       data = test[, -4])
predict(model_lars, x_test[1:5,], s = best)
test[1:5, ]$Petal.Width

RMSE(predict(model_lars, x_test, s = best)$fit, test$Petal.Width)
plot(test[,"Petal.Width"],predict(model_lars, x_test, s = best)$fit,
     xlim=c(0,3), ylim=c(0,3), xlab = "actual", ylab = "predicted",
     main = "Petal.Width")
abline(0,1, col="red")
cor(test[,"Petal.Width"], predict(model_lars, x_test, s = best)$fit)

#' ## Other Types of Regression
#'
#' * Robust regression: robust against violation of assumptions like heteroscedasticity and outliers (`roblm` and `robglm` in package robustbase)
#' * Generalized linear models (`glm`)
#' * Nonlinear least squares (`nlm`)

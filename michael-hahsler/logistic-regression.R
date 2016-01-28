#' ---
#' title: "Introduction to Logistic Regression with R"
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

#' # Introduction
#' Logistic regression is a probabilistic statistical classification
#' model to predict a binary outcome (a probability) given a set of features
#'
#'$$logit(p) = ln(\frac{p}{1-p}) = \beta_0 + \beta_1 x_1 + \beta_2 x_2 + ...$$
#'
#' Logistic regression can be thought of as a linear regression with the
#' log odds ratio as the dependent variable
#'
#' Load and shuffle data. We also add a useless variable to see if the logistic regression removes it.
data(iris)
x <- iris[sample(1:nrow(iris)),]
x <- cbind(x, useless = rnorm(nrow(x)))

#' Make Species into a binary classification problem so we will
#' classify if a flower is of species Virginica
x$virginica <- x$Species == "virginica"
x$Species <- NULL
plot(x, col=x$virginica+1)

#' # Create a Logistic Regression Model
model <- glm(virginica ~ .,
             family = binomial(logit), data=x)
#' Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred means that the data is possibly linearely separable

model

#' Check which features are significant?
summary(model)
#' AIC can be used for model selection
#'
#' # Do Stepwise Variable Selection
model2 <- step(model, data = x)
summary(model2)

#' # Calculate Response
#' **Note:** we do here in-sample testing on the data we learned the data
#' from. To get a generalization error estimate you should use a test set or
#' cross-validation!
pr <- predict(model2, x, type="response")
round(pr, 2)
hist(pr, breaks=20)
hist(pr[x$virginica==TRUE], col="red", breaks=20, add=TRUE)

#' # Check Classification Performance
table(actual=x$virginica, predicted=pr>.5)
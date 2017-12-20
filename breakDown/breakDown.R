library(breakDown)
library(ggplot2)


#---------------------
# Logistic regression
#---------------------

model <- glm(left ~ ., data = HR_data, family = "binomial")

explain_1 <- broken(model, HR_data[11,], baseline = "intercept")

explain_1
#                                   contribution
# satisfaction_level = 0.45         0.670
# number_project = 2                0.570
# salary = low                      0.390
# average_montly_hours = 135       -0.290
# Work_accident = 0                 0.220
# time_spend_company = 3           -0.130
# last_evaluation = 0.54           -0.130
# promotion_last_5years = 0         0.030
# sales = sales                     0.014
# final_prognosis                   1.300
# baseline:  -1.601457

plot(explain_1, trans = function(x) (exp(x) / (1 + exp(x))))


#---------------------
# Linear regression
#---------------------
url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv'
wine <- read.table(url, header = T, sep=";")
head(wine, 3)

model <- lm(quality ~ ., data = wine)

br <- broken(model, wine[1,], baseline = "Intercept")
plot(br)

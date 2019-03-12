# https://modelplot.github.io/modelplotr.html

library(devtools)
install_github("modelplot/modelplotr")

library(tidyverse)

###################################
# download bank data and prepare
###################################

#zipname = 'https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank-additional.zip'
# we encountered that the source at uci.edu is not always available, therefore we made a copy to our repos.
zipname = 'https://modelplot.github.io/img/bank-additional.zip'
csvname = 'bank-additional/bank-additional-full.csv'
temp <- tempfile()
download.file(zipname,temp, mode = "wb")
bank <- read.table(unzip(temp,csvname), sep = ";", stringsAsFactors = FALSE,
                   header = T)
unlink(temp)

bank <- bank[, c('y','duration','campaign','pdays','previous','euribor3m')]

# rename target class value 'yes' for better interpretation
bank$y[bank$y == 'yes'] <- 'term.deposit'

# explore data
str(bank)

bank %>%
  as_tibble() %>%
  group_by(y) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  mutate(prop = num / sum(num))
# y              num  prop
# <chr>        <int> <dbl>
# 1 no           36548 0.887
# 2 term.deposit  4640 0.113

###################################
# prepare data for training and train models
###################################

test_size = 0.3
train_index =  sample(seq(1, nrow(bank)),size = (1 - test_size)*nrow(bank),
                      replace = F)

train = bank[train_index,]
test = bank[-train_index,]


###################################
# estimate some models with caret...
###################################

# setting caret cross validation, here tuned for speed (not accuracy!)
fitControl <- caret::trainControl(method = "cv", number = 2, classProbs = TRUE)

# mnl model using glmnet package
mnl = caret::train(y ~.,data = train, method = "glmnet",trControl = fitControl)

# random forest using ranger package, here tuned for speed (not accuracy!)
rf = caret::train(y ~., data = train, method = "ranger", trControl = fitControl,
                  tuneGrid = expand.grid(.mtry = 2,.splitrule = "gini",
                                         .min.node.size = 10))


###################################
# ... and estimate some models with mlr
###################################

mlr::configureMlr()
task = mlr::makeClassifTask(data = train, target = "y")

# discriminant model
lrn = mlr::makeLearner("classif.lda", predict.type = "prob")
lda = mlr::train(lrn, task)


# xgboost model
lrn = mlr::makeLearner("classif.xgboost", predict.type = "prob")
xgb = mlr::train(lrn, task)



###################################
# run modelplotr
###################################

library(modelplotr)

# transform datasets and model objects into scored data and calculate deciles
prepare_scores_and_deciles(datasets = list("train","test"),
                           dataset_labels = list("train data","test data"),
                           models = list("rf","mnl","xgb","lda"),
                           model_labels = list("random forest",
                                               "multinomial logit",
                                               "XGBoost","Discriminant"),
                           target_column = "y")

# transform data generated with prepare_scores_and_deciles into aggregated data
# for chosen plotting scope
plotting_scope(select_model_label = 'XGBoost',
               select_dataset_label = 'test data')

# plot the cumulative gains plot
plot_cumgains()

# plot the cumulative gains plot and annotate the plot at decile = 2
plot_cumgains(highlight_decile = 2)

# plot the cumulative lift plot and annotate the plot at decile = 2
plot_cumlift(highlight_decile = 2)

# plot the response plot and annotate the plot at decile = 1
plot_response(highlight_decile = 1)

# plot the cumulative response plot and annotate the plot at decile = 3
plot_cumresponse(highlight_decile = 3)

# plot all four evaluation plots and save to file
plot_all(save_fig = TRUE,save_fig_filename = 'Selection model Term Deposits')


# set plotting scope to model comparison
plotting_scope(scope = "compare_models")

# plot the cumulative response plot and annotate the plot at decile = 3
plot_cumresponse(highlight_decile = 3)

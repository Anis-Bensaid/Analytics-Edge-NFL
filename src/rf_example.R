library(tidyverse)
library(caret)
library(e1071)

source("get_functions.R")

train_full <- read.csv("../data/train_data.csv")
test_full <- read.csv("../data/test_data.csv")

train_subset = 1:50000
test_subset = 1:10000
train <- train_full[subset,]
test <- test_full[subset,]
nfolds=2
metric <- "Accuracy"
method <- "rf"
tuneGrid <- NULL
trControl <- trainControl(method='cv', number=nfolds)
tuneLength = 4
thresh_hold = 0
model_name <- "RF"


models = train_models(train, method, metric, tuneGrid, trControl, tuneLength, thresh_hold)

predictions = get_predictions(models, test, model_name="CART")

predictions = normalise_predictions(predictions)
distribution = get_distribution(predictions)

true_labels = test %>% select(contains("Yard_"))
heaviside = get_distribution(true_labels)

score = get_score(heaviside, distribution)
score


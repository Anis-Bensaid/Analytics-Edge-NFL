library(tidyverse)
library(caret)
library(e1071)

source("get_functions.R")

train_full <- read.csv("../data/train_data.csv")
test_full <- read.csv("../data/test_data.csv")

train_subset = 1:1000
test_subset = 1:500
train <- train_full[train_subset,]
test <- test_full[test_subset,]
nfolds <- 2
metric <- "Accuracy"
method <- "rpart"
tuneGrid <- expand.grid(cp = c(0.001, 0.005))
trControl <- trainControl(method='cv', number=nfolds)
tuneLength = 4
thresh_hold <- 0
model_name <- "CART"


models = train_models(train, method, metric, tuneGrid, trControl, tuneLength, thresh_hold)

predictions = get_predictions(models, test, model_name="CART")

predictions = normalise_predictions(predictions)
distribution = get_distribution(predictions)

true_labels = test %>% select(contains("Yard_"))
heaviside = get_distribution(true_labels)

score = get_score(heaviside, distribution)
score


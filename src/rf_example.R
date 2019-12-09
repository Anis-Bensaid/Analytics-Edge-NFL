library(tidyverse)
library(caret)
library(e1071)

source("get_functions.R")

train_full <- readRDS("../data/train_data.RDS")
test_full <- readRDS("../data/test_data.RDS")

percent <- 0.1
train_subset = 1:round(nrow(train_full)*percent)
test_subset = 1:round(nrow(test_full)*percent)

train <- train_full[train_subset,]
test <- test_full[test_subset,]

method <- "glmnet"
model_name <- "logistic"

thresh_hold <- 0

nfolds <- 2
metric <- "Accuracy"
tuneGrid <- NULL
tuneGrid <- expand.grid(family = "binomial")
tuneLength = 3

trControl <- trainControl(method='cv', number=nfolds)

models = train_models(train, method, metric, tuneGrid, trControl, tuneLength, thresh_hold)
saveRDS(models, file=paste0("../models/", model_name,".RDS",sep=""))

predictions = get_predictions(models, test, model_name=model_name)

predictions = normalise_predictions(predictions)
distribution = get_distribution(predictions)

true_labels = test %>% select(contains("Yard_"))
heaviside = get_distribution(true_labels)

score = get_score(heaviside, distribution)

plot_d_h(distribution, heaviside, 2, yards=-99:99)

save.image(file=paste0("../processed/",model_name,".RData",sep=""))

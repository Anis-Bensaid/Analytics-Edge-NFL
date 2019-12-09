library(tidyverse)
library(caret)
library(e1071)

source("get_functions.R")

train <- readRDS("../data/train_data.RDS")
test <- readRDS("../data/test_data.RDS")

method <- "rf"
model_name <- "RF"

thresh_hold <- 0

nfolds <- 2
metric <- "Accuracy"
tuneGrid <- NULL
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
print(score)

rm(train)
rm(test)
save.image(file=paste0("../processed/",model_name,".RData",sep=""))

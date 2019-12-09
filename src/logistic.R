library(tidyverse)
library(caret)
library(e1071)

source("get_functions.R")

train <- readRDS("../data/train_data.RDS")
test <- readRDS("../data/test_data.RDS")

method <- "glmnet"
model_name <- "logistic"

thresh_hold <- 0

nfolds <- 2
metric <- "Accuracy"
tuneGrid <- expand.grid(alpha = c(0,0.5,1), lambda = seq(0.0001, 1, length = 5))
tuneLength = 3

trControl <- trainControl(method='cv', number=nfolds, classProbs = TRUE)

models = train_models(train, method, metric, tuneGrid, tuneLength, thresh_hold)
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



library(tidyverse)
library(caret)
library(e1071)
library(xgboost)
library(glmnet)

# Set wd
setwd("~/Dropbox (MIT)/Edge Project/Edge/src")
source("get_functions.R")

# Get models
rf_mod <- readRDS("../models/RF.RDS")
xgboost_mod <- readRDS("../models/XGBoost.RDS")

# Read in data
train <- readRDS("../data/train_data.RDS")
test <- readRDS("../data/test_data.RDS")

# Create a train set
predictions_cart_train <- readRDS("../models/cartpred_train.RDS")
predictions_rf_train = get_predictions(rf_mod, train, model_name="RF")
predictions_xgboost_train = get_predictions(xgboost_mod, train, model_name="XGBoost")
train_yards <- train[, which(grepl("Yard_", colnames(train)))]
meta_df_train <- cbind(predictions_cart_train, predictions_rf_train, predictions_xgboost_train, train_yards)

# Create a test set
predictions_cart_test <- readRDS("../models/cartpred_test.RDS")
predictions_rf_test = get_predictions(rf_mod, test, model_name="RF")
predictions_xgboost_test = get_predictions(xgboost_mod, test, model_name="XGBoost")
test_yards <- test[, which(grepl("Yard_", colnames(test)))]
meta_df_test <- cbind(predictions_cart_test, predictions_rf_test, predictions_xgboost_test, test_yards)

# Train ensemble models
# CART
method <- "rpart"
model_name <- "CART_ENS"
thresh_hold <- 0
nfolds <- 2
metric <- "Accuracy"
tuneGrid <- expand.grid(cp=c(0.00001))
tuneLength = 3
trControl <- trainControl(method='cv', number=nfolds)

cart_ens_mod = train_models(meta_df_train, method, metric, tuneGrid, trControl, tuneLength, thresh_hold)

cart_ens_predictions = get_predictions(cart_ens_mod, meta_df_test, model_name=model_name)
cart_ens_predictions = normalise_predictions(cart_ens_predictions)
cart_ens_distribution = get_distribution(cart_ens_predictions)

true_labels = test %>% select(contains("Yard_"))
heaviside = get_distribution(true_labels)

cart_ens_score = get_score(heaviside, cart_ens_distribution)
print(cart_ens_score)


# Logistic Ensemble
method <- 'glm'
model_name <- "LOGISTIC_ENS"
thresh_hold <- 0
nfolds <- 2
metric <- "Accuracy"
family <- "binomial"
tuneGrid <- NULL #expand.grid(alpha = c(0,0.5,1), lambda = seq(0.0001, 1, length = 5))
tuneLength = 3
trControl <- trainControl(method='cv', number=nfolds)

log_ens_mod = train_models(meta_df_train, method, metric, tuneGrid, trControl, tuneLength, thresh_hold)

log_ens_predictions = get_predictions(log_ens_mod, meta_df_test, model_name=model_name)
log_ens_predictions = normalise_predictions(log_ens_predictions)
log_ens_distribution = get_distribution(log_ens_predictions)

log_ens_score = get_score(heaviside, log_ens_distribution)
print(log_ens_score)


# Logistic: Everything + Ensemble
method <- 'glm'
model_name <- "LOGISTIC_ENS2"
thresh_hold <- 0
nfolds <- 1
metric <- "Accuracy"
family <- "binomial"
tuneGrid <- NULL #expand.grid(alpha = c(0,0.5,1), lambda = seq(0.0001, 1, length = 5))
tuneLength = 3
trControl <- trainControl(method='cv', number=nfolds)

big_train <- cbind(train, predictions_cart_train, predictions_rf_train, predictions_xgboost_train)
big_test <- cbind(test, predictions_cart_test, predictions_rf_test, predictions_xgboost_test)

log_ens_mod2 = train_models(big_train, method) #metric, tuneGrid, trControl, tuneLength, thresh_hold)

log_ens_predictions2 = get_predictions(log_ens_mod, meta_df_test, model_name=model_name)
log_ens_predictions2 = normalise_predictions(log_ens_predictions)
log_ens_distribution2 = get_distribution(log_ens_predictions)

log_ens_score2 = get_score(heaviside, log_ens_distribution2)
print(log_ens_score2)


# Logistic 2nd attempt
log_ens <- glm_models(big_train)
  
  glm_models <- function(data){
    Y_COL_NAME = "Yard_"
    nb.yards = sum(grepl(paste(Y_COL_NAME,"\\d",sep=""), colnames(data)))
    nrows.data = nrow(data)
    models <- vector(mode="list", length=nb.yards)
    for (i in 1:nb.yards) {
      target_col = paste(Y_COL_NAME, toString(i), sep="")
      if (sum(as.numeric(data[,target_col])) > thresh_hold){
        form <- generate_formula(data, i)
        mod <- glm(form = form, 
                     data = data, 
                     #subset = subset,
                     #metric = metric, 
                     #method = method, 
                     # tuneGrid = tuneGrid, 
                     # trControl= trControl,
                     family = family,
                     # tuneLength = tuneLength,
                     #na.action = na.exclude
        )
        models[[i]] = mod
        print(paste("Model", toString(i), "trained."))
      } else {
        print(paste("Yardage", toString(i), "is equal to 0 in all the training set."))
      }
    }
    models
  }

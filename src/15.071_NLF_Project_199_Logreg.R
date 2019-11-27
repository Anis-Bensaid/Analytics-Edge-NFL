library(dplyr)
library(ggplot2)
library(caret)
library(ROCR)
library(class)
library(glmnet)
library(fastDummies)
library(caTools)

data = read_csv("train_features_nov7.csv")

data$Yards = as.factor(train$Yards)

data <- dummy_cols(data, select_columns = "Yards")

N <- nrow(data)
idx = sample.split(data$Yards, 0.75)
train <- data[idx,]
test = data[!idx,]

ys = train %>% 
  select(contains("Yards_"))
x = train %>% 
  select(-contains("Yards_"))

logreg.mod <- glm(TenYearCHD~., data=train, family="binomial")
summary(logreg.mod)


setwd("C:/Users/jmcke/Desktop/Kaggle NFL")

library(tidyverse)
library(caret)
library(e1071)
df <- read_csv("train_features_nov7.csv")


run_199_models <- function(df, sample_size, model_type){

  df <- df %>%
    select(-X1)
  df1 <- df[sample(nrow(df)),]
  train <- df1[1:sample_size,]
  test <- df1[(sample_size+1):nrow(df1),]
  
  #binary expansion
  m_train <- matrix(NA,nrow(train),199)
  m_test <- matrix(NA,nrow(test),199)
  for (i in 1:199){
    m_train[,i] = ifelse(train[,'Yards']==(i-100),1,0)
  }
  for (i in 1:199){
    m_test[,i] = ifelse(test[,'Yards']==(i-100),1,0)
  }
  
  
  #training models
  train_Y <- train %>%
    select(Yards)
  test_Y <- test %>%
    select(Yards)
  train_X <- train %>%
    select(-Yards)
  test_X <- test %>%
    select(-Yards)
  
  #find columns with two levels
  li <- matrix()
  for (i in 1:199){
    indicator <- ifelse(sum(m_train[,i])>0,T,F)
    if (indicator==T){
      li <- rbind(li, i)
    }
  }
  li <- li[2:nrow(li)]
  
  
  probs <- matrix(NA,nrow(test_X),1)
  for (i in li){
    rf <- train(x = train_X,
                    y = as.factor(m_train[,i]),
                    method=model_type)
    print("Congrats... you did it")
    preds <- predict(rf, newdata = test_X, type = "prob")
    probs <- cbind(probs, preds[2])
  }
  
  #making pdf and cdf
  probs <- probs[,2:ncol(probs)]
  colnames(probs) <- li
  
  pdf <- matrix(NA, nrow(test_X), 199)
  for (i in 1:199){
    if (i %in% li){
      pdf[,i] <- probs[,paste(i)]
      } else {
      pdf[,i] <- 0
    }
  }
  
  cdf <- matrix(NA, nrow(test_X), 199)
  for (i in 1:199){
    if (i != 1){
      cdf[,i] <- pdf[,i] + cdf[,(i-1)]
    } else{
      cdf[,i] <- pdf[,i]
    }
  }
  
  #normalize cdf
  for (i in 1:199){
    cdf[i,] <- cdf[i,]/max(cdf[i,])
  }
  
  #generate score
  score <- 0
  for (i in 1:nrow(cdf)){
    if (test_Y[[i,1]]+101 < 199){
      score <- score + sum((cdf[i,1:(test_Y[[i,1]]+100)]-0)^2) + sum((1-cdf[i,(test_Y[[i,1]]+101):199])^2)
    } else {
      score <- score + sum((cdf[i,1:(test_Y[[i,1]]+100)]-0)^2)
    }
  }
  
  return (score/(199*nrow(test_X)))
}



#run code
#use ctree or ctree2 for CART
#the below is a link with all possible models
#https://topepo.github.io/caret/available-models.html
run_199_models(df, 10000, "rf")




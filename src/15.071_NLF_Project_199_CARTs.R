# Set working directory, and load packages: 
library(caret) # for randomly splitting training/test 
library(rpart) # for building CART model
library(rpart.plot) # a library for an alternative way of plotting CART trees.
library(caTools)
library(dplyr)
library(normalr)
library(e1071)

data <- read.csv("train_features_nov7.csv")

# Split data in training and testing sets
split = sample.split(data$Yards, SplitRatio = 0.1)
dataTest = data[!split,]
dataTrain = data[split,]

dataTrain_x <- dataTrain %>%
  select(-Yards)

dataTest_x <- dataTest %>%
  select(-Yards)

n_train_rows = nrow(dataTrain)
n_train_cols = ncol(dataTrain)

binary_df <- matrix(, nrow=n_train_rows, ncol=199)

# Create binary columns for each possible Yard gain/loss
for (i in 1:199) {
  binary_df[,i] <- as.integer(dataTrain$Yards == i-100)
}

binary_df_true <- matrix(, nrow=n_train_rows)

# Find out which Yard gains actually occurred in the data
for (i in 1:199) {
  indicator = ifelse(sum(binary_df[,i]) > 0, TRUE, FALSE)
  if (indicator){
    binary_df_true <- cbind(binary_df_true, binary_df[,i])
  }
}
binary_df_true <- binary_df_true[,2:ncol(binary_df_true)] # Cut out first NA column

predictions_matrix <- matrix(, nrow=n_train_rows)

# Tree model loop
for (i in 1:199){
  # Build model
  mod <- train(x=dataTrain_x[,], y=as.factor(binary_df[,i]), method="ctree")
                                         #minbucket = 50,
                                         #cp=0.001)
  # Make predictions
  if (sum(binary_df[,i]) > 0){ # if Yard value i occurred:
    predictions_matrix <- cbind(predictions_matrix, predict(mod, newdata=dataTest_x, type="prob"))
  } else{ # else add a column of zero predictions
    predictions_matrix <- cbind(predictions_matrix, rep(0, n_train_rows))
  }
    
  }


# Normalize probabilities for each row in the predictions matrix
predictions_matrix2 <- apply(predictions_matrix, MARGIN =1, FUN = scaling)

scaling <- function(x){
  return(x/sum(x))
}

# Loop solution
for (m in 1:N){
  predictions_df[m,] <- scaling(predictions_df[m,])
}

# Add in column for actual Yard values
predictions_matrix <- cbind(predicitons_matrix, dataTest$Yards)

# CRPS function for evaluation
CRPS <- function(prediction_data){
  squared_sum = 0
  
  for (m in 1:N) {
    for (n in 1:199) {
      # CDF for your prediction
      P_y <- 0
      for (i in 1:n){
        P_y <- P_y + prediction_data[m, i-100]
      }
      
      if (n - prediction_data[m,200] >= 0){
        H = 1} else {
          H = 0}
      
      squared_sum <- squared_sum + (P_y - H)^2
    }
  }
  return(squared_sum/(199*N))
}



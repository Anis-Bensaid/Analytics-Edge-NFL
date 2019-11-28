library(tidyverse)
library(caret)
library(e1071)
library(dplyr)

train_ratio = 0.6
valid_ratio = 0.2
  
  
df <- read_csv("train_features_nov7.csv")

df <- df %>%
  select(-X1)

df1 <- df[sample(nrow(df)),]

train_size <- round(train_ratio*nrow(df))
valid_size <- round(valid_ratio*nrow(df))

train <- df1[1:train_size,]
valid <- df1[(train_size+1):(train_size+valid_size),]
test <- df1[(train_size+valid_size+1):nrow(df1),]

#binary expansion
m_train <- matrix(NA,nrow(train),199)
m_test <- matrix(NA,nrow(test),199)
m_valid <- matrix(NA,nrow(valid),199)
for (i in 1:199){
  m_train[,i] = ifelse(train[,'Yards']==(i-100),1,0)
}
for (i in 1:199){
  m_test[,i] = ifelse(test[,'Yards']==(i-100),1,0)
}
for (i in 1:199){
  m_valid[,i] = ifelse(valid[,'Yards']==(i-100),1,0)
}

# Convert matrices to data frames
m_train <- data.frame(m_train)
m_test <- data.frame(m_test)
m_valid <- data.frame(m_valid)

# Rename "Xi" colmumns
for (i in 1:199){
  colnames(m_train)[colnames(m_train)==paste("X", toString(i), sep="")] <- paste("Yard_", toString(i), sep="")
  colnames(m_test)[colnames(m_test)==paste("X", toString(i), sep="")] <- paste("Yard_", toString(i), sep="")
  colnames(m_valid)[colnames(m_valid)==paste("X", toString(i), sep="")] <- paste("Yard_", toString(i), sep="")
}


# Concatenate binary expansion to dataframes
train <- cbind(train, m_train)
test <- cbind(test, m_test)
valid <- cbind(valid, m_valid)

# Take out Yards column
train <- train[, !(names(train) %in% "Yards")]
test <- test[, !(names(test) %in% "Yards")]
valid <- valid[, !(names(valid) %in% "Yards")]


# Write to csv
write.csv(train, "/Users/desireewaugh/Dropbox (MIT)/Edge Project/train_data.csv", row.names=FALSE)
write.csv(valid, "/Users/desireewaugh/Dropbox (MIT)/Edge Project/valid_data.csv", row.names=FALSE)
write.csv(test, "/Users/desireewaugh/Dropbox (MIT)/Edge Project/test_data.csv", row.names=FALSE)

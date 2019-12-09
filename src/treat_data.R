library(tidyverse)
library(caret)
library(e1071)

train_full <- read.csv("../data/train_data.csv")
test_full <- read.csv("../data/test_data.csv")

saveRDS(train_full, "../data/train_data.RDS")
saveRDS(test_full, "../data/test_data.RDS")

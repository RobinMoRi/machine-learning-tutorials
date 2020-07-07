rm(list = ls())
library(dslabs)
library(caret)
library(tidyverse)
library(ggplot2)
data("tissue_gene_expression")

set.seed(1991)
#fit <- rpart(x, y)
train_knn <- with(tissue_gene_expression, 
                  train(x, y, method = "rpart",
                        tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))
train_knn$results
max_acc_cp <- train_knn$result$cp[which.max(train_knn$result$Accuracy)]
max_acc_cp
ggplot(train_knn)
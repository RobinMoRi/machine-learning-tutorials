
library(dslabs)
library(caret)
library(tidyverse)
library(ggplot2)
library(rpart)
data("tissue_gene_expression")

set.seed(1991)
fit <- with(tissue_gene_expression,
                  train(x, y, method = "rpart",
                        tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),
                        control = rpart.control(minsplit = 0)))
fit$results
max_acc_cp <- fit$result$cp[which.max(fit$result$Accuracy)]
max_acc_cp
ggplot(fit)
confusionMatrix(fit)
# 
# 
# # fit <- with(tissue_gene_expression, 
# #                   train(x, y, method = "rpart",
# #                         tuneGrid = data.frame(cp = 0),
# #                         control = rpart.control(minsplit = 0)))
# 
plot(fit$finalModel)
text(fit$finalModel)

tree_terms <- as.character(unique(fit$finalModel$frame$var[!(fit$finalModel$frame$var == "<leaf>")]))
tree_terms

imp_rpart <- varImp(fit)
imp_rprt
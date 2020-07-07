rm(list = ls())
library(dslabs)
library(caret)
library(tidyverse)
library(ggplot2)
library(rpart)
data("tissue_gene_expression")

set.seed(1991, sample.kind = "Rounding")
fit <- with(tissue_gene_expression,
            train(x, y, method = "rf",
                  tuneGrid = data.frame(mtry = seq(50, 200, 25)),
                  nodesize = 1))
fit$results
max_acc_mtry <- fit$result$mtry[which.max(fit$result$Accuracy)]
max_acc_mtry

imp <- varImp(fit)
imp

data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)
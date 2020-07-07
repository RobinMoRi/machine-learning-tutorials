rm(list = ls())
library(dslabs)
library(caret)
library(tidyverse)
library(ggplot2)
data("tissue_gene_expression")

set.seed(1993) #if using R 3.6 or later set.seed(1993, sample.kind="Rounding")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

train_lda <- train(x, y, method = "lda", preProcess = "center")
train_lda$results

confusionMatrix(train_lda)
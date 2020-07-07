rm(list = ls())
library(dslabs)
library(caret)
library(tidyverse)
library(ggplot2)
data("tissue_gene_expression")

set.seed(1993) #if using R 3.6 or later set.seed(1993, sample.kind="Rounding")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

train_lda <- train(x, y, method = "lda", preProcess = "center")
train_lda$results

df_fm_means <- as.data.frame(t(train_lda$finalModel$means))

t(train_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text()
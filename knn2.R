rm(list = ls())

library(caret)
library(dslabs)
library(MLmetrics)
data("tissue_gene_expression")

set.seed(1, sample.kind="Rounding") # set.seed(1, sample.kind="Rounding") if using R 3.6 or later
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)


test_set_x <- as.data.frame(tissue_gene_expression$x[test_index,])
test_set_y <- tissue_gene_expression$y[test_index]

train_set_x <- as.data.frame(tissue_gene_expression$x[-test_index,])
train_set_y <- tissue_gene_expression$y[-test_index]


k <- seq(1, 11, 2)

accuracies <- sapply(k, function(k){
  knn_fit <- knn3(train_set_x, train_set_y, k = k)
  y_hat <- predict(knn_fit, test_set_x, type = "class")

  Accuracy(y_hat, test_set_y)
})
data.frame(k,accuracies)
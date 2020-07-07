rm(list = ls())

library(caret)
library(dslabs)
library(MLmetrics)
library(e1071)
data(heights)

set.seed(1, sample.kind="Rounding") # set.seed(1, sample.kind="Rounding") if using R 3.6 or later
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

k <- seq(1, 101, 3)

F1_scores <- sapply(k, function(k){
  knn_fit <- knn3(sex ~ ., data = train_set, k = k)
  y_hat <- predict(knn_fit, test_set, type = "class")
  #print(knn_fit)
  F_meas(test_set$sex, y_hat)
})

#max f1 score
max(F1_scores)

#Which k gives max f1-score
k[which.max(F1_scores)]
plot(k, F1_scores)

# train_knn <- train(sex ~ ., method = "knn",
#                    data = train_set,
#                    tuneGrid = data.frame(k = seq(1, 101, 3)))
# train_knn$bestTune
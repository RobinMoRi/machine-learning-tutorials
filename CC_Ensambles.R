# models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")
# library(caret)
# library(dslabs)
# set.seed(1) # use `set.seed(1, sample.kind = "Rounding")` in R 3.6 or later
# data("mnist_27")
# 
# fits <- lapply(models, function(model){ 
#   print(model)
#   train(y ~ ., method = model, data = mnist_27$train)
# }) 
# 
# names(fits) <- models

pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)

acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)

#ensamble
# x <- colSums("7")/ncol(pred)
# ifelse(proportion_sevens > 0.5, 7, 2)
pred_ensamble <- apply(pred, 1, function(row){
  proportion_seven <- sum(row == "7")/length(row)
  ifelse(proportion_seven > 0.5, 7, 2)
})

acc_ensamble <- mean(pred_ensamble == mnist_27$test$y)
acc_ensamble

votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]

min_accuracies <- sapply(fits, function(object){ 
  min(object$results$Accuracy)
})
mean(mean_accuracies)

#mean of ensamble of models with acc over or equal to 0.8
indx <- which(min_accuracies >= 0.8)
new_pred <- pred[,indx]
new_votes <- rowMeans(new_pred == "7")
y_hat_new <- ifelse(new_votes >= 0.5, "7", "2")
mean(y_hat_new == mnist_27$test$y)
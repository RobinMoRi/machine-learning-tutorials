rm(list=ls())

options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

#Samples
nrow(brca$x)

#Number of predictors
ncol(brca$x)

#Proportion of malignant samples
mean(brca$y == "M")

#Column with highest mean
which.max(colMeans(brca$x))

#Column with lowest std
which.min(colSds(brca$x))

#Q2
x_mean_0 <- sweep(brca$x, 2, colMeans(brca$x), FUN = "-")
x_standardized <- sweep(x_mean_0, 2, colSds(brca$x), FUN = "/")
sd(x_standardized[,1])
median(x_standardized[,1])

#Distance
indx_B <- which(brca$y == "B")
indx_M <- which(brca$y == "M")
all_dist <- as.matrix(dist(x_standardized))
mean(all_dist[indx_B[2:length(indx_B)]]-all_dist[1])
mean(all_dist[indx_M[1:length(indx_M)]]-all_dist[1])

d_features <- dist(t(x_standardized))
heatmap(as.matrix(d_features), labRow = NA, labCol = NA)

plot(hclust(d_features))
h <- hclust(d_features)
groups <- cutree(h, k = 5)
split(names(groups), groups)


pca <- prcomp(as.matrix(x_standardized))
summary(pca)

data.frame(pca$x[,1:2], tumor_type=brca$y) %>% 
  ggplot(aes(PC1, PC2, fill = tumor_type))+
  geom_point(cex=3, pch=21)


# data.frame(pca$x[,1:10], tumor_type=brca$y) %>%
#   ggplot(aes(pca$x,pca$y, group = tumor_type))+
#   geom_boxplot() 

data.frame(type = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()


set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_standardized[test_index,]
test_y <- brca$y[test_index]
train_x <- x_standardized[-test_index,]
train_y <- brca$y[-test_index]

mean(train_y == "B")
mean(test_y == "B")

predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

set.seed(3, sample.kind = "Rounding")
k <- kmeans(train_x, 2)
y_hat <- predict_kmeans(test_x, k)
y_hat <- ifelse(y_hat == 1, "B", "M")
accuracy <- mean(y_hat == test_y)
accuracy

sum(y_hat == "B" & y_hat == test_y)/sum(test_y == "B")
sensitivity(factor(y_hat), test_y, positive = "B")
sum(y_hat == "M" & y_hat == test_y)/sum(test_y == "M")
sensitivity(factor(y_hat), test_y, positive = "M")

#logistic regression
fit_lr <- train(y ~ ., method = "glm", data = data.frame(x = train_x, y = train_y))
y_hat_lr <- predict(fit_lr, data.frame(x = test_x, y = test_y))
glmacc <- mean(y_hat_lr == test_y)

#lda
fit_lda <- train(y ~ ., method = "lda", data = data.frame(x = train_x, y = train_y))
y_hat_lda <- predict(fit_lda, data.frame(x = test_x, y = test_y))
ldaacc <- mean(y_hat_lda == test_y)

#qda
fit_qda <- train(y ~ ., method = "qda", data = data.frame(x = train_x, y = train_y))
y_hat_qda <- predict(fit_qda, data.frame(x = test_x, y = test_y))
qdaacc <- mean(y_hat_qda == test_y)

set.seed(5, sample.kind = "Rounding")
#loess
fit_loess <- train(y ~ ., method = "gamLoess", data = data.frame(x = train_x, y = train_y))
y_hat_loess <- predict(fit_loess, data.frame(x = test_x, y = test_y))
loessacc <- mean(y_hat_loess == test_y)

set.seed(7, sample.kind = "Rounding")
#knn
fit_knn <- train(y ~ ., method = "knn", 
                data = data.frame(x = train_x, y = train_y), 
                tuneGrid = data.frame(k = seq(3, 21, 2)))
y_hat_knn <- predict(fit_knn, data.frame(x = test_x, y = test_y))
knnacc <- mean(y_hat_knn == test_y)
fit_lr$bestTune

set.seed(9, sample.kind = "Rounding")
#random forrest
fit_rf <- train(y ~ ., method = "rf", 
                data = data.frame(x = train_x, y = train_y), 
                tuneGrid = data.frame(mtry = seq(3, 9, 2)),
                importance = TRUE)
y_hat_rf <- predict(fit_rf, data.frame(x = test_x, y = test_y))
rfacc <- mean(y_hat_rf == test_y)
fit_rf$bestTune
varImp(fit_rf)

preds <- data.frame(glm = y_hat_lr, 
                    lda = y_hat_lda, 
                    qda = y_hat_qda,
                    loess = y_hat_loess,
                    knn = y_hat_knn, 
                    rf = y_hat_rf)
pred_ensamble <- apply(preds, 1, function(row){
  proportion_malignant <- sum(row == "M")/length(row)
  ifelse(proportion_malignant > 0.5, "M", "B")
})
ensacc <- mean(pred_ensamble == test_y)

accs <- data.frame(glm = glmacc, 
                    lda = ldaacc, 
                    qda = qdaacc,
                    loess = loessacc,
                    knn = knnacc, 
                    rf = rfacc,
                    ensamble = ensacc)
#print accuracies of all models
accs

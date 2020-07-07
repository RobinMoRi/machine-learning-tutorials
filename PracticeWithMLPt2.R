library(caret)
library(tidyverse)
library(caret)
library(dslabs)
data(heights)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

#Q7
set.seed(2)    # if using R 3.6 or later, use set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

#Q8
trainFeatures <- list(train$Sepal.Length, train$Sepal.Width, train$Petal.Length, train$Petal.Width)
testFeatures <- list(test$Sepal.Length, test$Sepal.Width, test$Petal.Length, test$Petal.Width)
featuresString <- list("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
# trainFeature <- train$Sepal.Length
# testFeature <- test$Sepal.Length
trainTarget <- train$Species
testTarget <- test$Species
iter <- 1
for(trainFeature in trainFeatures){
  testFeature <- testFeatures[iter]
  print(featuresString[iter])
  cutoff <- seq(min(trainFeature), max(trainFeature))
  accuracy <- map_dbl(cutoff, function(x){
    y_hat <- ifelse(trainFeature > x, "virginica", "versicolor") %>%
      factor(levels = levels(testTarget))
    mean(y_hat == trainTarget)
  })
  print(paste("max accuracy: ",max(accuracy)))
  best_cutoff <- cutoff[which.max(accuracy)]
  print(paste("best cutoff: ",best_cutoff))
  
  
  iter <- iter + 1
}

#Q9
# predictions <- foo(train[,3])
# rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
# cutoffs <-rangedValues[which(predictions==max(predictions))]
# 
# y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
# mean(y_hat==test$Species)

#Q10
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}
predictions <- apply(test[,-5],2,foo)
sapply(predictions,max)	

#Q11
plot(iris,pch=21,bg=iris$Species)
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)	

#Q12
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

plot(iris,pch=21,bg=iris$Species)

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)

length_predictions <- sapply(petalLengthRange,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7

width_predictions <- sapply(petalWidthRange,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5

y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)
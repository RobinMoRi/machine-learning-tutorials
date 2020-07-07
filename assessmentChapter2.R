rm(list=ls())
library(caret)
library(magrittr)
library(dplyr)
library(tidyverse)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species


max_accuracies <- vector(mode="double", length=0)
step <- 0.1

set.seed(2)    # if using R 3.6 or later, use set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]



# Using min and max values as limits for cutoffs
#Sepal Length:
cutoff <- seq(min(train$Sepal.Length),max(train$Sepal.Length), step)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  return(mean(y_hat == train$Species))

})
max_accuracies[1] <- max(accuracy)

#Sepal Width
cutoff2 <- seq(min(train$Sepal.Width),max(train$Sepal.Width), step)
accuracy2 <- map_dbl(cutoff2, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  return(mean(y_hat == train$Species))
  
})
max_accuracies[2] <- max(accuracy2)

#Petal Length
cutoff3 <- seq(min(train$Petal.Length),max(train$Petal.Length), step)
accuracy3 <- map_dbl(cutoff3, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  return(mean(y_hat == train$Species))
  
})
max_accuracies[3] <- max(accuracy3)

#Petal Width
cutoff4 <- seq(min(train$Petal.Width),max(train$Petal.Width), step)
accuracy4 <- map_dbl(cutoff4, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  return(mean(y_hat == train$Species))
  
})
max_accuracies[4] <- max(accuracy4)

max_accuracy <- 0;
for(i in 1:length(max_accuracies)){
  if(max_accuracies[i] > max_accuracy){
    max_accuracy <- max_accuracies[i]
    index <- i
  }
}
if(index == 1){
  print("Sepal Length feature produces the highest accuracy")
} else if(index == 2){
  print("Sepal Width feature produces the highest accuracy")
} else if(index == 3){
  print("Petal Length feature produces the highest accuracy")
} else if(index == 4){
  print("Petal Width feature produces the highest accuracy")
}

#Efterkonstruerat
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff2 <- cutoff2[which.max(accuracy2)]
best_cutoff3 <- cutoff3[which.max(accuracy3)]
best_cutoff4 <- cutoff4[which.max(accuracy4)]
y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor") %>% factor(levels = levels(test$Species))
y_hat2 <- ifelse(test$Petal.Length > best_cutoff2, "virginica", "versicolor") %>% factor(levels = levels(test$Species))
y_hat3 <- ifelse(test$Petal.Length > best_cutoff3, "virginica", "versicolor") %>% factor(levels = levels(test$Species))
y_hat4 <- ifelse(test$Petal.Length > best_cutoff4, "virginica", "versicolor") %>% factor(levels = levels(test$Species))

accuracy_test_set <- mean(y_hat == test$Species)
print(max_accuracies[1])
print(accuracy_test_set)

accuracy_test_set2 <- mean(y_hat2 == test$Species)
print(max_accuracies[2])
print(accuracy_test_set2)

accuracy_test_set3 <- mean(y_hat3 == test$Species)
print(max_accuracies[3])
print(accuracy_test_set3)

accuracy_test_set4 <- mean(y_hat4 == test$Species)
print(max_accuracies[4])
print(accuracy_test_set4)

y_hat_final <- ifelse((test$Petal.Length > best_cutoff3) | (test$Petal.Width > best_cutoff4), "virginica", "versicolor")
accuracy_final <- mean(y_hat_final == test$Species)
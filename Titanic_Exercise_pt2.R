rm(list = ls())

library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

set.seed(42, sample.kind = "Rounding")
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
test_set <- titanic_clean[test_index, ]
train_set <- titanic_clean[-test_index, ]

#lda model
set.seed(1, sample.kind = "Rounding")
fit_lda <- train(Survived ~ Fare, method = "lda", data = train_set)
y_hat_lda <- predict(fit_lda, test_set)
lda_acc <- mean(y_hat_lda == test_set$Survived)
lda_acc

#qda model
set.seed(1, sample.kind = "Rounding")
fit_qda <- train(Survived ~ Fare, method = "qda", data = train_set)
y_hat_qda <- predict(fit_qda, test_set)
qda_acc <- mean(y_hat_qda == test_set$Survived)
qda_acc

#logistic regression model
set.seed(1, sample.kind = "Rounding")
fit_glm <- train(Survived ~ Age, method = "glm", data = train_set)
y_hat_glm <- predict(fit_glm, test_set)
glm_acc <- mean(y_hat_glm == test_set$Survived)
glm_acc

#logistic regression model - 4 predictors
set.seed(1, sample.kind = "Rounding")
fit_glm2 <- train(Survived ~ Sex + Pclass + Fare + Age, method = "glm", data = train_set)
y_hat_glm2 <- predict(fit_glm2, test_set)
glm_acc2 <- mean(y_hat_glm2 == test_set$Survived)
glm_acc2

#logistic regression model - all predictors
set.seed(1, sample.kind = "Rounding")
fit_glm3 <- train(Survived ~ ., method = "glm", data = train_set)
y_hat_glm3 <- predict(fit_glm3, test_set)
glm_acc3 <- mean(y_hat_glm3 == test_set$Survived)
glm_acc3


#knn
set.seed(6, sample.kind = "Rounding")
fit_knn <- train(Survived ~ ., method = "knn", data = train_set, tuneGrid = data.frame(k = seq(3, 51, 2)))
fit_knn$results
best_k <- fit_knn$results$k[which.max(fit_knn$results$Accuracy)]
best_k
fit_knn$bestTune

#plot knn model
ggplot(fit_knn)

#accuracy of model on test set
y_hat_knn <- predict(fit_knn, test_set)
knn_acc <- mean(y_hat_knn == test_set$Survived)
knn_acc


#cross validation: using 10 samples with 10% of the observations each
set.seed(8, sample.kind = "Rounding")
control <- trainControl(method = "cv", number = 10, p = .9)
fit_knn_cv <- train(Survived ~ ., 
                  method = "knn", 
                  data = train_set,
                  tuneGrid = data.frame(k = seq(3, 51, 2)),
                  trControl = control)
fit_knn_cv$bestTune
y_hat_knn_cv <- predict(fit_knn_cv, test_set)
knn_cv_acc <- mean(y_hat_knn_cv == test_set$Survived)
knn_cv_acc

#Classification tree model
set.seed(10, sample.kind = "Rounding")
fit_rpart <- train(Survived ~ .,
                   method = "rpart",
                   data = train_set,
                   tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
#best tune
fit_rpart$bestTune
y_hat_rpart <- predict(fit_rpart, test_set)
rpart_acc <- mean(y_hat_rpart == test_set$Survived)
rpart_acc

#final model
plot(fit_rpart$finalModel, margin = 0.1)
text(fit_rpart$finalModel)

#Random forest model
set.seed(14, sample.kind = "Rounding")
fit_rf <- train(Survived ~ .,
                   method = "rf",
                   data = train_set,
                   tuneGrid = data.frame(mtry = seq(1, 7, 1)),
                ntree = 100)
#bestTune
fit_rf$bestTune
y_hat_rf <- predict(fit_rf, test_set)
rf_acc <- mean(y_hat_rf == test_set$Survived)
rf_acc


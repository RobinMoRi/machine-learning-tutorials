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

#number of observations in each set
obs_train <- nrow(train_set)
obs_train
obs_test <- nrow(test_set)
obs_test

#proportion of survived in training set
mean(train_set$Survived==1)

set.seed(3, sample.kind = "Rounding")
#Guess outcome
y_hat <- sample(c(0, 1), length(test_index), replace = TRUE)
#accuracy
mean(y_hat==test_set$Survived)

#Proportion female survived
female_survivor_set <- train_set %>% filter(Sex == "female" & Survived == 1)
nrow(female_survivor_set)/sum(train_set$Sex == "female")

#Proportion male survived
male_survivor_set <- train_set %>% filter(Sex == "male" & Survived == 1)
nrow(male_survivor_set)/sum(train_set$Sex == "male")

train_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "male") %>%
  pull(Survived)

#Predict survival based on sex (based on previously learned numbers)
y_hat2 <- ifelse(test_set$Sex == "male", 0, 1) %>%
  factor(levels = levels(test_set$Survived))

accuracy2 <- mean(y_hat2 == test_set$Survived)
accuracy2

#Proportion deaths by passenger class
train_set %>%
  group_by(Pclass) %>%
  summarize(Survived = mean(Survived == 1))

#Predict survival by pclass
y_hat3 <- ifelse(test_set$Pclass == 1, 1, 0) %>%
  factor(levels = levels(test_set$Survived))
accuracy3 <- mean(y_hat3 == test_set$Survived)
accuracy3

#Group passenger by sex and pclass
train_set %>%
  group_by(Pclass, Sex) %>%
  summarize(Survived = mean(Survived == 1))

#Predict survival by pclass and sex
y_hat4 <- ifelse(((test_set$Pclass == 1 | test_set$Pclass == 2) & (test_set$Sex == "female")), 1, 0) %>%
  factor(levels = levels(test_set$Survived))
accuracy4 <- mean(y_hat4 == test_set$Survived)
accuracy4

#confusion matrices
cm1 <- confusionMatrix(y_hat2, test_set$Survived)
cm2 <- confusionMatrix(y_hat3, test_set$Survived)
cm3 <- confusionMatrix(y_hat4, test_set$Survived)
cm1$byClass
cm2$byClass
cm3$byClass

#f1 scores
f1_1 <- F_meas(y_hat2, test_set$Survived)
f1_2 <- F_meas(y_hat3, test_set$Survived)
f1_3 <- F_meas(y_hat4, test_set$Survived)
f1_1
f1_2
f1_3
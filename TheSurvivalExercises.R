options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

# titanic %>% ggplot() +
#   geom_density(aes(Age, fill = Sex), position = "stack", alpha = 0.2)


# params <- titanic %>%
#   filter(!is.na(Age)) %>%
#   summarize(mean = mean(Age), sd = sd(Age))

# titanic %>%
#   filter(!is.na(Age)) %>%
#   ggplot(aes(sample = Age)) +
#   geom_qq(dparams = params) +
#   geom_abline()


# titanic %>%
#   filter(!is.na(Age)) %>%
#   ggplot() +
#   geom_bar(aes(Survived, fill = Sex), position = position_dodge())

# titanic %>%
#   filter(!is.na(Age)) %>%
#   ggplot() +
#   geom_density(aes(Age, fill = Survived), alpha = 0.2)

# titanic %>%
#   filter(!is.na(Age), Fare > 0) %>%
#   ggplot() +
#   scale_y_continuous(trans = "log2") +
#   geom_jitter(aes(Survived, Fare)) +
#   geom_boxplot(aes(Survived, Fare))
  
# titanic %>%
#   # filter(!is.na(Age)) %>%
#   ggplot() +
#   geom_bar(aes(Pclass, fill = Survived))
# 
# titanic %>%
#   # filter(!is.na(Age)) %>%
#   ggplot() +
#   geom_bar(aes(Pclass, fill = Survived), position = position_fill())
# 
# titanic %>%
#   # filter(!is.na(Age)) %>%
#   ggplot() +
#   geom_bar(aes(Survived, fill = Pclass), position = position_fill())

titanic %>%
  filter(!is.na(Age)) %>%
  ggplot() +
  geom_density(aes(Age, fill = Survived), alpha = 0.2) +
  facet_grid(Sex ~ Pclass)


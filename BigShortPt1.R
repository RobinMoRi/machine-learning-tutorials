rm(list = ls())

options(digits = 3)
library(tidyverse)
library(dslabs)
data(death_prob)


#Question 1a
p_death_50_female <- death_prob %>%
      filter(age == 50, sex == "Female") %>%
      pull(prob)
p_death_50_female

loss <- -150000
gain <- 1150
n <- 1000

#Question 1b
exp_val_fem_50 <- p_death_50_female*loss + (1-p_death_50_female)*gain
exp_val_fem_50

#Question 1c
se_fem_50 <- abs(loss-gain)*sqrt(p_death_50_female*(1-p_death_50_female))
se_fem_50

#Question 1d
exp_val_sum <- n*exp_val_fem_50
exp_val_sum

#Question 1e
se_sum <- sqrt(n)*se_fem_50
se_sum

#Question 1f
limit_losing_money <- 0
pnorm(limit_losing_money, exp_val_sum, se_sum)

#Question 2a
p_death_50_male <- death_prob %>%
  filter(age == 50, sex == "Male")  %>%
  pull(prob)
p_death_50_male

#Question 2b
mu <- 700000
a <- -150000 #Cost, therefore it should be negative
b <- ((mu/n)-a*p_death_50_male)/(1-p_death_50_male)
b #premium rat4e

#Question 2c
se <- abs(a-b)*sqrt(n*p_death_50_male*(1-p_death_50_male))
se

#Question 2d
pnorm(limit_losing_money, mu, se)
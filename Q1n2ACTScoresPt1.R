rm(list = ls())

x <- 16 *#Seed variable
set.seed(x, sample.kind = "Rounding")

tests <- 10000
avg <- 20.9
std_dev <- 5.7
act_scores <- rnorm(tests, avg, std_dev)

#Question 1a
mean(act_scores)

#Question 1b
sd(act_scores)

#Question 1c
sum(act_scores >= 36)

#Question 1d
1-pnorm(30, avg, std_dev)
sum(act_scores >= 30)/tests

#Question 1e
pnorm(10, avg, std_dev)
sum(act_scores <= 10)/tests

#Question 2
x <- seq(1, 36)
f_x <- dnorm(x, avg, std_dev)
plot(x, f_x)
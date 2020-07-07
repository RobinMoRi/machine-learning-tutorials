rm(list = ls())

options(digits = 3)
library(tidyverse)
library(dslabs)
data(death_prob)

#Question 3a
n <- 1000
loss <- -150000
gain <- 1150
prob_death <- 0.015
mu <- n*(prob_death*loss + (1-prob_death)*gain)
mu

#question 3b
sigma <- abs(loss-gain)*sqrt(n*prob_death*(1-prob_death))
sigma

#Question 3c
pnorm(0, mu, sigma)

#Question 3d
pnorm(-1e6, mu, sigma)

#Question 3e
p <- seq(.01, .03, .001)
probs <- sapply(p, function(x){
  mu <- n*(x*loss + (1-x)*gain)
  sigma <- abs(loss-gain)*sqrt(n*x*(1-x))
  pnorm(0, mu, sigma)
})

min(p[which(probs >= 0.9)])


#Question 3f
p <- seq(.01, .03, .0025)
probs <- sapply(p, function(x){
  mu <- n*(x*loss + (1-x)*gain)
  sigma <- abs(loss-gain)*sqrt(n*x*(1-x))
  pnorm(-1e6, mu, sigma)
})

min(p[which(probs >= 0.9)])

#Question 4a
p_loss <- 0.015
p_gain <- 1-p_loss
set.seed(25, sample.kind = "Rounding")
profits <- sample(c(loss, gain), n, replace = TRUE, c(p_loss, p_gain))
sum(profits)/1e6

#Question 4a
set.seed(27, sample.kind = "Rounding")
B <- 1e4
profits <- replicate(B, {
  money <- sample(c(loss, gain), n, replace = TRUE, c(p_loss, p_gain))
  sum(money)
})
mean(profits <= -1e6)
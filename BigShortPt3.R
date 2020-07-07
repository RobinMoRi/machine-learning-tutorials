rm(list = ls())

options(digits = 3)
library(tidyverse)
library(dslabs)
data(death_prob)

p_death = 0.015
n <- 1000
loss_per_claim <- -150000
z <- qnorm(0.05)
x <- -loss_per_claim*(n*p_death-z*sqrt(n*p_death*(1-p_death)))/(n*(1-p_death)+z*sqrt(n*p_death*(1-p_death)))
x

expected_val <- (loss_per_claim*p_death + x*(1-p_death))
expected_val

expected_val*n

set.seed(28, sample.kind = "Rounding")
B <- 1e4
p_claim <- 0.015
p_premium <- 1-p_claim
earnings <- replicate(B, {
  outcome <- sample(c(loss_per_claim, x), n, replace = TRUE, c(p_claim, p_premium))
  sum(outcome)
})
mean(earnings <= 0)

set.seed(29, sample.kind = "Rounding")
earnings <- replicate(B, {
  p <- 0.015 + sample(seq(-0.01, 0.01, length = 100), 1)
  outcome <- sample(c(loss_per_claim, x), n, replace = TRUE, c(p, 1-p))
  sum(outcome)
})

mean(earnings)
mean(earnings <= 0)
mean(earnings <= -1e6)
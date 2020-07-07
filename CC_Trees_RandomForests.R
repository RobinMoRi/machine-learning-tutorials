rm(list = ls())
library(dslabs)
library(caret)
library(tidyverse)
library(ggplot2)
library(rpart)

n <- 1000
sigma <- 0.25
set.seed(1) #set.seed(1, sample.kind = "Rounding") if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
dat
fit <- rpart(y ~ ., data = dat) 
plot(fit)
text(fit)

dat %>%
  mutate(y_hat = predict(fit)) %>%
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)
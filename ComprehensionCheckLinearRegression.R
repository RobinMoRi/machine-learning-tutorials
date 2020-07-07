rm(list = ls())
library(Metrics)
library(caret)

set.seed(1, sample.kind="Rounding") # set.seed(1, sample.kind="Rounding") if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
set.seed(1, sample.kind="Rounding")
rmse_vector <- replicate(n, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  test_set <- dat[test_index, ]
  train_set <- dat[-test_index, ]

  mu_x <- mean(train_set$x)
  mu_y <- mean(train_set$y)
  s_x <- sd(train_set$x)
  s_y <- sd(train_set$y)
  r <- cor(train_set$x, train_set$y)

  m <-  r * s_y / s_x
  b <- mu_y - m*mu_x

  #lMod <- lm(y ~ x, data = train_set)
  y_hat <- b+m*test_set$x
  rmse(y_hat, test_set$y)
})

mean(rmse_vector)
sd(rmse_vector)
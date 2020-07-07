rm(list = ls())
library(Metrics)

set.seed(1, sample.kind="Rounding") # set.seed(1, sample.kind="Rounding") if using R 3.6 or later
#n <- 100
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1, sample.kind="Rounding")


test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
test_set <- dat[test_index, ]
train_set <- dat[-test_index, ]


fit_1 <- lm(y ~ x_1, data = train_set)
y_hat_1 <- predict(fit_1, newdata = test_set)


fit_2 <- lm(y ~ x_2, data = train_set)
y_hat_2 <- predict(fit_2, newdata = test_set)

fit_3 <- lm(y ~ x_1 + x_2, data = train_set)
y_hat_3 <- predict(fit_3, newdata = test_set)

rmse(y_hat_1, test_set$y)
rmse(y_hat_2, test_set$y)
rmse(y_hat_3, test_set$y)
rm(list = ls())
library(Metrics)
library(dplyr)
library(purrr)

set.seed(1, sample.kind="Rounding") # set.seed(1, sample.kind="Rounding") if using R 3.6 or later
n <- c(100, 500, 1000, 5000, 10000)

list <- map(n, function(x){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(x, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))

  rmse_vector <- replicate(100, {
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
    
    y_hat <- b+m*test_set$x
    rmse(y_hat, test_set$y)
  })
  
  my_list <- list(mean = mean(rmse_vector), sd = sd(rmse_vector))
  #mean(rmse_vector)
  #sd(rmse_vector)
})

data.frame(list)
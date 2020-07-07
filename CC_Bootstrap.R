rm(list = ls())

library(caret)
library(dslabs)
library(MLmetrics)

set.seed(1995, sample.kind="Rounding")
indexes <- createResample(mnist_27$train$y, 10)
sum(indexes$Resample01 == 3)
sum(indexes$Resample01 == 4)
sum(indexes$Resample01 == 7)

sumVar <- 0
for (resampledIndx in indexes) {
  sumVar = sumVar + sum(resampledIndx == 3)
}
sumVar


set.seed(1, sample.kind="Rounding")
B <- 10^4
quantiles <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(quantiles)
sd(quantiles)


set.seed(1, sample.kind="Rounding")
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind="Rounding")
indx_bootstrap <- createResample(y, 10)
# mean(indx_bootstrap)
# sd(indx_bootstrap)
q_75 <- sapply(indx_bootstrap, function(indx_bootstrap){
  y_sample <- y[indx_bootstrap]
  quantile(y_sample, 0.75)
})
mean(q_75)
sd(q_75)


set.seed(1, sample.kind="Rounding")
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind="Rounding")
indx_bootstrap <- createResample(y, 10^4)
# mean(indx_bootstrap)
# sd(indx_bootstrap)
q_75 <- sapply(indx_bootstrap, function(indx_bootstrap){
  y_sample <- y[indx_bootstrap]
  quantile(y_sample, 0.75)
})
mean(q_75)
sd(q_75)

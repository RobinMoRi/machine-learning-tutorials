rm(list = ls())

x <- 16 *#Seed variable
set.seed(x, sample.kind = "Rounding")

tests <- 10000
avg <- 20.9
std_dev <- 5.7
act_scores <- rnorm(tests, avg, std_dev)

z_scores <- (act_scores-mean(act_scores))/sd(act_scores)

#Question 3a
mean(z_scores > 2)

#Question  Take values around 2 (good approximation), the 2nd line gives exact
mean(act_scores[which(z_scores >= 1.9 & z_scores <= 2.1)])
2*sd(act_scores) + mean(act_scores)

#Question 3c
qnorm(0.975, mean(act_scores), sd(act_scores))
#To get percentile:
pnorm(32.1, mean(act_scores), sd(act_scores))

#Question 4
x <- seq(1, 36)
act_score_cdf <- function(n){
  mean(act_scores <= n)
}

p_scores <- sapply(x, act_score_cdf)

#Question 4a
min(x[which(p_scores >= 0.95)])

#Question 4b
qnorm(0.95, avg, std_dev)

#Question 4c
p <- seq(0.01 , 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
names(sample_quantiles[max(which(sample_quantiles <= 26.1))])

#Question 4d
theoretical_quantiles <- qnorm(p, avg, std_dev)
qqplot(theoretical_quantiles, sample_quantiles)
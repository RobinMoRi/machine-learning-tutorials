rm(list = ls())

set.seed(1986, sample.kind="Rounding")
n <- round(2^rnorm(1000, 8, 1))


set.seed(1, sample.kind="Rounding")
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))
# 
# schools %>% top_n(10, quality) %>% arrange(desc(quality))

set.seed(1, sample.kind="Rounding")
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))
schools %>% top_n(10, score) %>% arrange(desc(score))
median(schools$size)
schools %>% top_n(10, score) %>% .$size %>% median()

schools %>% top_n(-10, score) %>% arrange(desc(score))
schools %>% top_n(-10, score) %>% .$size %>% median()

schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2)

overall <- mean(sapply(scores, mean))
alpha <- 25
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

alphas <- seq(10, 250, 1)

rmses <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
  schools <- schools %>% mutate(score_reg = score_reg)
  return(RMSE(schools$score_reg, schools$quality))
})

best_alpha <- alphas[which.min(rmses)]
best_alpha

score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+best_alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))


rmses <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x)  sum(x)/(length(x)+alpha))
  schools <- schools %>% mutate(score_reg = score_reg)
  return(RMSE(schools$score_reg, schools$quality))
})

best_alpha <- alphas[which.min(rmses)]
best_alpha

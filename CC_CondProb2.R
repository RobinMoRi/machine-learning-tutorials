library(dplyr)
library(dslabs)
library(ggplot2)
data("heights")
# heights %>% 
#   mutate(height = round(height)) %>%
#   group_by(height) %>%
#   summarize(p = mean(sex == "Male")) %>%
#   qplot(height, p, data =.)

# ps <- seq(0, 1, 0.1)
# heights %>% 
#   mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
#   group_by(g) %>%
#   summarize(p = mean(sex == "Male"), height = mean(height)) %>%
#   qplot(height, p, data =.)

Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y =(y), x =(x)) %>%
  qplot(x, y, data =.)
library(dslabs)
library(tidyverse)
library(broom)
data("mnist_27")
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()

#Below my own code
span <- 0.65 #length(dat$day) #about 2 months span
fit <- loess(as.numeric(y) ~ x_2, degree=1, span = span, data=mnist_27$train)
mnist_27$train %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(x_2, y)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(x_2, smooth), color="red")

#Below curve from exercise
# mnist_27$train %>% 
#   mutate(y = ifelse(y=="7", 1, 0)) %>%
#   ggplot(aes(x_2, y)) + 
#   geom_smooth(method = "loess")
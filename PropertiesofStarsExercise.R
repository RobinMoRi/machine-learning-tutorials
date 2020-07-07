library(tidyverse)
library(dslabs)
library(zoom)
data(stars)
options(digits = 3)   # report 3 significant digits

# mean(stars$magnitude)
# sd(stars$magnitude)
# 
# stars %>% ggplot() +
#   geom_density(aes(temp))

# 
# stars %>% ggplot() +
#   geom_point(aes(temp, magnitude))

stars %>% ggplot() +
  geom_point(aes(temp, magnitude, color = type)) +
  scale_y_reverse()
  # scale_x_reverse()
 
# stars %>% ggplot() +
#   geom_text(aes(temp, magnitude, label = star)) +
#   scale_y_reverse()
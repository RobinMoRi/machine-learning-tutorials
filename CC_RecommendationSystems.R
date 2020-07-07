rm(list = ls())

library(dplyr)
library(lubridate)
library(dslabs)
library(caret)
data("movielens")

movie_count <- movielens %>%
  dplyr::count(movieId)

years <- movielens$year[movie_count$movieId]

data.frame(year = years, n = movie_count$n, movieId = movie_count$movieId) %>%
  ggplot(aes(year, n, color = movieId)) +
  geom_point()

# movielens %>% group_by(movieId) %>%
#   summarize(n = n(), year = as.character(first(year))) %>%
#   qplot(year, n, data = ., geom = "boxplot") +
#   coord_trans(y = "sqrt") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

tab <- movielens %>%
  filter(year %in% c(1993:2018)) %>% 
  select(movieId, title, rating, year)

mean(tab$rating[which(tab$title == "Shawshank Redemption, The")])
mean(tab$rating[which(tab$title == "Forrest Gump")])

movie_count_2 <- movielens %>%
  filter(year %in% c(1993:2018)) %>%
  filter(title == "Forrest Gump") %>% 
  dplyr::count(movieId)

movie_count_2$n/(2018-1994)


movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate)) %>%
  ggplot(aes(rating, rate)) +
  geom_point() +
  geom_smooth()
  
movielens <- mutate(movielens, date = as_datetime(timestamp))
movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()


movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
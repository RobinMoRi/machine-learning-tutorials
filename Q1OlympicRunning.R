library(gtools)
library(tidyverse)
options(digits = 3)    # report 3 significant digits

jamaica_competitors <- 3
other_competitors <- 5
total <- jamaica_competitors+other_competitors
number_of_medals <- 3

permutations <- factorial(total)/factorial(total-number_of_medals)
permutations2 <- nrow(permutations(total, number_of_medals))

perm3 <- nrow(permutations(jamaica_competitors, number_of_medals))

prob_all_jamaica <- (jamaica_competitors/total)*((jamaica_competitors-1)/(total-1))*((jamaica_competitors-2)/(total-2))

prob_all_jamaica_2 <- perm3/permutations2


B <- 1e4
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")

set.seed(1, sample.kind = "Rounding")

result <- replicate(B, {
  medal_winners <- sample(runners, 3, replace = FALSE, prob = NULL)
  all(medal_winners == "Jamaica")
})

mean(result)
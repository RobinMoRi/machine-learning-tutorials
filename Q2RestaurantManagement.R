library(gtools)
library(tidyverse)
options(digits = 3)    # report 3 significant digits

no_entrees <- 6
no_sides <- 6
no_drinks <- 3


tot_combination <- function(n){
  comb_entrees <- nrow(combinations(n, 1))
  comb_sides <- nrow(combinations(no_sides, 2))
  comb_drinks <- nrow(combinations(no_drinks, 1))
  
  comb_entrees*comb_sides*comb_drinks
}

entree_range = 1:12

result <- sapply(entree_range, tot_combination)

above_365 <- which(result >= 365)

min_number_entrees <- entree_range[above_365[1]]

print(min_number_entrees)


side_choice <- function(n){
  comb_entrees <- nrow(combinations(no_entrees, 1))
  comb_sides <- nrow(combinations(n, 2))
  comb_drinks <- nrow(combinations(no_drinks, 1))
  
  comb_entrees*comb_sides*comb_drinks
}

side_range = 2:12

result <- sapply(side_range, side_choice)

above_365 <- which(result >= 365)

min_number_sides <- side_range[above_365[1]]

print(min_number_sides)

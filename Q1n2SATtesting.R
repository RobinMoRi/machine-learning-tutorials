options(digits = 3)
penalty <- -0.25
award <- 1
no_questions <- 44
no_choices <- 5

#Questions 1a
prob_guess_one <- 1/no_choices
prob_guess_one

#Question 1b
expected_value <- penalty*(1-prob_guess_one)+award*prob_guess_one
expected_value

#Question 1c
expected_score <- no_questions*expected_value
expected_score

#Questions 1d
std_err <- sqrt(no_questions)*abs(penalty-award)*sqrt(prob_guess_one*(1-prob_guess_one))
std_err

#Question 1e - guessing 8 or higher on the test
p_guess_8 <- pnorm(8, expected_score, std_err, lower.tail = FALSE)
p_guess_8

#Question 1f
seed_val <- 21
set.seed(seed_val, sample.kind = "Rounding")
B <- 1e4
test_scores <- replicate(B, {
  test_score <- sample(c(penalty, award), no_questions, replace = TRUE, c(1-prob_guess_one, prob_guess_one))
  sum(test_score)
})
p_score <- mean(test_scores >= 8)
p_score

#Question 2a
no_choices_new <- 4
penalty_new <- 0
p_guess_one_new <- 1/no_choices_new
new_expected_score <- no_questions*(penalty_new*(1-p_guess_one_new)+award*p_guess_one_new)
new_expected_score

#Question 2b
std_err_new <- sqrt(no_questions)*abs(penalty_new-award)*sqrt(p_guess_one_new*(1-p_guess_one_new))
p_guess_30 <- pnorm(30, new_expected_score, std_err_new, lower.tail = FALSE)
p_guess_30

#Question 2c
p <- seq(0.25, 0.95, 0.05)
#CLT to compute the probabilities
test_score_function <- function(x){
  score <- 35
  mu <- no_questions*(penalty_new*(1-x)+award*x)
  se <- sqrt(no_questions)*abs(penalty_new-award)*sqrt(x*(1-x))
  pnorm(35, mu, se, lower.tail = FALSE)
}
test_probabilities <- sapply(p, test_score_function)

p[min(which(test_probabilities >= 0.8))]
rm(list = ls())
losing_bet <- -1
winning_bet <- 6
bets <- 500

no_pockets <- 38
no_bet_pockets <- 5

p_win <- no_bet_pockets/no_pockets
p_lose <- 1-p_win

#Question 3a
expected_val <- p_win*winning_bet+p_lose*losing_bet
expected_val

#Question 3b
std_err <- abs(losing_bet-winning_bet)*sqrt(p_win*p_lose)
std_err

#Question 3c - expected value of the AVERAGE payout
expected_val_500 <- expected_val
expected_val_500

#Question 3d
std_err_avg_500 <- std_err/sqrt(bets)
std_err_avg_500

#Question 3e - expected value of the sum
expected_val_500_sum <- expected_val*bets
expected_val_500_sum

#Question 3f
std_err_sum_500 <- std_err*sqrt(bets)
std_err_sum_500

#Question 3g
pnorm(0, expected_val_500_sum, std_err_sum_500)
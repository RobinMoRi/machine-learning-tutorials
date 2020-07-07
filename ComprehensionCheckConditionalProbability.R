set.seed(1)
#Make a vector of diseases based on probability, 0=no disease, 1=disease
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
#test negative or positive 0=negative, 1=positive
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

prob_test_pos <- mean(test)
sprintf("Probability that a test is positive: %f", prob_test_pos)

prob_test_neg <- 1-prob_test_pos
prob_dis <- mean(disease)

prob_test_neg_disease <- 1-mean(test[disease==1])

prob_disease_test_neg <- (prob_test_neg_disease*prob_dis)/prob_test_neg
sprintf("Probability that an individual has the disease if the test is negative: %f", prob_disease_test_neg)

prob_disease_test_pos <- mean(disease[test==1])
sprintf("Probability that an individual has the disease if the test is positive: %f", prob_disease_test_pos)

prevalence <- mean(disease)
RR <- prob_disease_test_pos/prevalence
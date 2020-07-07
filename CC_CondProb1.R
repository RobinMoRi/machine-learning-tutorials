set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

#Q2
test_pos <- mean(test)
test_pos

#Q3
test_dis_neg <- mean(disease[test==0])
test_dis_neg

#Q4
test_dis_pos <- mean(disease[test==1])
test_dis_pos

#Q5
test_dis_pos/mean(disease)
rm(list=ls())
library(caret)
library(dslabs)
library(dplyr)

set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

# x_subset <- x[ ,sample(p, 100)]
# fit <- train(x_subset, y, method = "glm")
# fit$results

#install.packages("BiocManager")
#BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)
pvals <- tt$p.value

ind <- which(pvals <= 0.01)
length(ind)

x_subset <- x[,ind]

fit <- train(x_subset, y, method = "glm")
fit$results


fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)
rm(list=ls())
library(dslabs)
data(tissue_gene_expression)

#Q1
d <- dist(tissue_gene_expression$x)
d[1]
d[2]
d[1]-d[2]

d[39]
d[40]
d[39]-d[40]

d[73]
d[74]
d[73]-d[74]

ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]
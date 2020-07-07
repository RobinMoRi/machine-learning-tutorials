data("tissue_gene_expression")
dim(tissue_gene_expression$x)

#Q1
pc <- prcomp(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

#Q2
average <- rowMeans(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], av = average, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, av, color = tissue)) +
  geom_point()
cor(pc$x[,1], average)

#Q3
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2],
           pc_3 = pc$x[,3], pc_4 = pc$x[,4], 
           pc_5 = pc$x[,5], pc_6 = pc$x[,6], 
           pc_7 = pc$x[,7],
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_7, color = tissue)) +
  geom_boxplot()

# for(i in 1:10){
#   boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
# }

data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2],
           pc_3 = pc$x[,3], pc_4 = pc$x[,4], 
           pc_5 = pc$x[,5], pc_6 = pc$x[,6], 
           pc_7 = pc$x[,7],
           tissue = tissue_gene_expression$y) %>% summary()

plot(summary(pc)$importance[3,])
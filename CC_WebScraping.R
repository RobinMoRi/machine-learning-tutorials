library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

nodes <- html_nodes(h, "table")
html_text(nodes[[8]])
html_table(nodes[[8]])

#Q1
tables <- sapply(nodes[1:4], html_table)


#Q2
tables2 <- sapply(nodes[(length(nodes)-2):length(nodes)], html_table)

#Q3
tab_1 <- html_table(nodes[[10]])
tab_1 <- tab_1[-1, -1]
tab_2 <- html_table(nodes[[19]])
tab_2 <- tab_2[-1,]

col_names <- c("Team", "Payroll", "Average")
colnames(tab_1) <- col_names
colnames(tab_2) <- col_names

final <- full_join(tab_1, tab_2, by = c("Team"))
dim(final)

                           
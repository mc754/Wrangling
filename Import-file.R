#install.packages("readr", dependencies = TRUE)
#install.packages("Lahman")
library(tidyverse)
library(readr)
library(dslabs)
library(Lahman)
library(rvest)

url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
#sapply(nodes[1:4], html_table)

#tail(html_table(nodes), n = 3)

tab1 <- html_table(nodes[[10]])
tab1 <- tab1[-1, -1]                  #delete first row and column
names(tab1) <- c("Team", "Payroll", "Average")

tab2 <- html_table(nodes[[19]])
tab2 <- tab2[-1,]                     #delete first row
names(tab2) <- c("Team", "Payroll", "Average")
full_join(tab1, tab2, by = "Team")
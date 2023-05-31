library(tidyverse)
library(rvest)

# Question a)

# there are 8 tables on the site  
url <- read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")
nodes = html_nodes(url, "table")
head(nodes)
table = html_table(nodes, header = 1, trim = FALSE, dec = ".") 


# table stores a list of a list containing the tables
class(table)
class(table[1])
class(table[1][[1]])
ans = table[1][[1]]

# tidying unnecessary columns
ans[1] = NULL
ans[14] = NULL
ans[13] = NULL
ans


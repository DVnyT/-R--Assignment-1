library(rvest)
library(tidyverse)

# Question b)

bTask <- function(bUrl){
    bNode = html_nodes(bUrl, "table")
    head(bNode)
    bTable = html_table(bNode, header = 1, trim = FALSE, dec = ".")
    
    sales = bTable[1][[1]]
    names(sales) <- sales[1,]
    sales = sales[-(1:5),]
    sales = sales[, -(12:14)]
    
    ROE = bTable[3][[1]]
    names(ROE) <- ROE[1,]
    ROE = ROE[,-(12:13)]
    ROE = ROE[-1,]
    
    sales = rbind(sales, ROE)
    return(sales)
}
url1 = read_html("https://www.moneyworks4me.com/indianstocks/large-cap/oil-gas/refineries/reliance-industries/company-info")
ans1 = bTask(url1)

url2 = read_html("https://www.moneyworks4me.com/indianstocks/large-cap/metals-mining/metal-non-ferrous/hindalco/company-info")
ans2 = bTask(url2)

url3 = read_html("https://www.moneyworks4me.com/indianstocks/large-cap/power/power-generation-distribution/ntpc/company-info")
ans3 = bTask(url3)

url4 = read_html("https://www.moneyworks4me.com/indianstocks/large-cap/oil-gas/oil-exploration/ongc/company-info")
ans4 = bTask(url4)

url5 = read_html("https://www.moneyworks4me.com/indianstocks/large-cap/construction-infrastructure/engineering-construction/larsen-toubro/company-info")
ans5 = bTask(url5)

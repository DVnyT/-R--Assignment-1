library(tidyverse)
library(rvest) # isn't rvest a subset of tidyverse; do we need to mention both?

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


# Question c)
  
tennis <- function(prob){
  scoreA = 0
  scoreB = 0
  matches = 0
  while(scoreA < 3 && scoreB < 3){
    matches = matches + 1
    if (rbinom(1, 1, prob)){
      # 1 indicates a win by player A
      scoreA = scoreA + 1
    }
    else {
      scoreB = scoreB + 1
    }
  }
  return(matches)
}

constProb = 0.70
matches <- rep(1, times = 1000)
for(i in 1: 1000){
  matches[i] <- tennis(constProb)
} 
ansc <- mean(matches)

# Question d)
MontyHall <- function(){
  # Set the winning door to 2 to simplify bit arithmetic later
  winDoor = 2
  
  # Choose randomly out of 0, 1 and 2
  firstChoice = sample(c(0,1,2), 1)
  
  if(firstChoice != 2){
    # Open the door with the only other goat remaining
    # On second viewing this statement of opening the door is unnecessary, but
    # tells us a lot qualitatively
    openDoor = as.integer(!firstChoice)
    
    # For the strategy, we switch to the door not opened and not chosen first
    firstChoice = 2
  }
  else{
    # Host can open either door with a goat
    openDoor = sample(c(0,1), 1)
    
    # We still switch
    firstChoice = as.integer(!openDoor)
  }
  return(firstChoice == winDoor)
}

dVector <- rep(1, times = 1000)
for(i in 1:1000){
  dVector[i] = MontyHall()
}
ansd <- mean(dVector)

# Question e)

netflix = read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")
movieNames = netflix %>% html_elements(".article_movie_title a") %>% html_text()
movieYear = netflix %>% html_elements(".start-year") %>% html_text()
movieScore = netflix %>% html_elements(".tMeterScore") %>% html_text()
movieRanking = netflix %>% html_elements(".countdown-index") %>% html_text()
movie = as_tibble(cbind(movieRanking, movieNames, movieScore, movieYear))
names(movie) = c("Ranking", "Name of Movie", "Tomato % Score", "Year of movie")

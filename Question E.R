library(rvest)
library(tidyverse)

# Question e)

netflix = read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")
movieNames = netflix %>% html_elements(".article_movie_title a") %>% html_text()
movieYear = netflix %>% html_elements(".start-year") %>% html_text()
movieScore = netflix %>% html_elements(".tMeterScore") %>% html_text()
movieRanking = netflix %>% html_elements(".countdown-index") %>% html_text()
movie = as_tibble(cbind(movieRanking, movieNames, movieScore, movieYear))
names(movie) = c("Ranking", "Name of Movie", "Tomato % Score", "Year of movie")

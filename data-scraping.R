library(rvest)
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(plotly)

# ----------Scraping----------

# Read the web page as html
url <- read_html('https://www.imdb.com/list/ls026158184/')

# Scrape titles, ranks, running times, star ratings, metascores, gross, and genres
movie_titles <- url %>%
  html_nodes(".lister-item-header") %>%
  html_nodes("a") %>%
  html_text()

movie_ranks <- url %>%
  html_nodes(".text-primary") %>%
  html_text()
movie_ranks <- sapply(movie_ranks, function(x) as.integer(parse_number(x)))

movie_runtime <- url %>%
  html_nodes('.runtime') %>%
  html_text()
movie_runtime <- sapply(movie_runtime, function(x) as.integer(parse_number(x)))

movie_stars <- url %>%
  html_nodes(".ipl-rating-star.small") %>%
  html_nodes(".ipl-rating-star__rating") %>%
  html_text()
movie_stars <- sapply(movie_stars, function(x) as.numeric(x))

movie_metascores <- url %>%
  html_nodes(".lister-item-content")
movie_metascores <- sapply(movie_metascores, function(x) html_node(x, ".metascore"))
movie_metascores <- sapply(movie_metascores, function(x) html_text(x))
movie_metascores[!is.na(movie_metascores)] <- sapply(movie_metascores[!is.na(movie_metascores)], function(x) as.numeric(parse_number(x)))
movie_metascores <- as.numeric(movie_metascores)

movie_gross <- url %>%
  html_nodes(".lister-item-content")
movie_gross <- sapply(movie_gross, function(x) html_node(x, ".ghost+ .text-muted+ span"))
movie_gross <- sapply(movie_gross, function(x) html_text(x))
movie_gross[!is.na(movie_gross)] <- sapply(movie_gross[!is.na(movie_gross)], function(x) parse_number(x)*1000000)
movie_gross <- as.numeric(movie_gross)

movie_genre <- url %>%
  html_nodes(".genre") %>%
  html_text()
movie_genre <- sapply(movie_genre , function(x) str_sub(x, 2, nchar(x)))
movie_genre <- sapply(movie_genre , function(x) str_sub(x, 1, nchar(x)-12))
genres <- unique(unlist(sapply(movie_genre , function(x) strsplit(x, ", "))))

df <- data.frame("Title" = movie_titles,
                 "Rank" = movie_ranks,
                 "MovieRuntime" = movie_runtime,
                 "MovieStar" = movie_stars,
                 "MovieMetascore" = movie_metascores,
                 "MovieGenere" = movie_genre,
                 "MovieGross" = movie_gross)
#write.csv(df, file = "imdb-top100.csv")
#saveRDS(df, "imdb-top100.rds")

counts <- c()
for(genre in genres){
  sum <- 0
  for(i in movie_genre){
    if(grepl(genre, i))
      sum = sum + 1
  }
  counts[genre] <- sum
}
# genre counts
g_df <- data.frame("genre"=names(counts), "count"=counts)
#write.csv(g_df, file = "genre-counts.csv")
#saveRDS(g_df, file = "genre-counts.rds")

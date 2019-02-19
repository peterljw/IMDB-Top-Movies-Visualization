# ----------One Variable Visualization----------

# obtain the count of each genre
counts <- c()
for(genre in genres){
  sum <- 0
  for(i in movie_genre){
    if(grepl(genre, i))
      sum = sum + 1
  }
  counts[genre] <- sum
}
# genre piet chart
g_df <- data.frame("genre"=names(counts), "count"=counts)
genre_pie <- plot_ly(g_df, labels = ~genre, values = ~count, textposition = 'outside',
                     textinfo = 'label') %>%
  add_pie(hole = 0.6) %>%
  layout(title = 'Distribution of Movie Genres', showlegend = F)

# runningtime histogram
runtime_hist <- ggplotly(ggplot(df, aes(x=MovieRuntime)) + geom_histogram(bins = 10, aes(fill = ..count..)) + 
                           xlab("Running time") + ylab("Count"))
runtime_box <- plot_ly(df, y = ~MovieRuntime, type = "box", name = "Movie Running Time") %>%
  layout(yaxis = list(title = "Running Time (minutes)"))

# movie star histogram
star_hist <- ggplotly(ggplot(df, aes(x=MovieStar)) + geom_histogram(bins = 10, aes(fill = ..count..)) + 
                        xlab("Star Rating") + ylab("Count"))
star_box <- plot_ly(df, y = ~MovieStar, type = "box", name = "Movie Star Rating") %>%
  layout(yaxis = list(title = "Star Points (out of 10)"))

# metascore histogram
meta_hist <- ggplotly(ggplot(df, aes(x=MovieMetascore)) + geom_histogram(bins = 10, aes(fill = ..count..)) + 
                        xlab("Metascore") + ylab("Count"))
meta_box <- plot_ly(df, y = ~MovieMetascore, type = "box", name = "Metascore") %>%
  layout(yaxis = list(title = "Score Points (out of 100)"))

# gross histogram
gross_hist <- ggplotly(ggplot(df, aes(x=MovieGross)) + geom_histogram(bins = 10, aes(fill = ..count..)) + 
                         xlab("Gross") + ylab("Count"))
gross_box <- plot_ly(df, y = ~MovieGross, type = "box", name = "Gross") %>%
  layout(yaxis = list(title = "Box Office Gross (in dollars)"))

# ----------Two-Variable Visualization----------

meta_star <- ggplotly(ggplot(df, aes(x=MovieStar, y=MovieMetascore, color=MovieMetascore)) +
  geom_point(size=2.5) +
  xlab("Star Rating") + ylab("Metascore"))

meta_runtime <- ggplotly(ggplot(df, aes(x=MovieRuntime, y=MovieMetascore, color=MovieMetascore)) +
  geom_point(size=2.5) +
  xlab("Running Time") + ylab("Metascore"))

meta_gross <- ggplotly(ggplot(df, aes(x=MovieGross, y=MovieMetascore, color=MovieMetascore)) +
  geom_point(size=2.5) +
  xlab("Box Office Gross") + ylab("Metascore"))

star_meta <- ggplotly(ggplot(df, aes(x=MovieMetascore, y=MovieStar, color=MovieStar)) +
  geom_point(size=2.5) +
  xlab("Metascore") + ylab("Star Rating"))

star_runtime <- ggplotly(ggplot(df, aes(x=MovieRuntime, y=MovieStar, color=MovieStar)) +
  geom_point(size=2.5) +
  xlab("Running Time") + ylab("Star Rating"))

star_gross <- ggplotly(ggplot(df, aes(x=MovieGross, y=MovieStar, color=MovieStar)) +
  geom_point(size=2.5) +
  xlab("Box Office Gross") + ylab("Star Rating"))

# ----------Three-Variable Visualization----------

p <- plot_ly(df, x = ~MovieRuntime, y = ~MovieStar, z = ~MovieGross, color = ~MovieGross) %>%
  add_markers() %>%
  colorbar(title = "title") %>%
  layout(scene = list(xaxis = list(title = 'Running Time (minute)'),
                      yaxis = list(title = 'Star Rating (out of 10)')))

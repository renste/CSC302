
library(ggplot2)

read.csv("~/Downloads/Most Streamed Spotify Songs 2024 (1).csv")

spotify_data <- data.frame(
  track = paste("Track", 1:100),
  genre = sample(c("Pop", "Hip-Hop", "Rock", "EDM", "Country", "Jazz", "Classical"), 100, replace = TRUE)
)

# Calculate the count of each genre
genre_distribution <- as.data.frame(table(spotify_data$genre))

# Create the pie chart
ggplot(genre_distribution, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(
    title = "Distribution of Genres Among Spotify Tracks",
    fill = "Genre"
  ) +
  theme_void() +  # Removes all axes and gridlines
  theme(legend.position = "right")

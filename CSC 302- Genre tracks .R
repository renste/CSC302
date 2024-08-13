
library(ggplot2)

read.csv("~/Downloads/Most Streamed Spotify Songs 2024 (1).csv")

spotify_data <- data.frame(
  track = paste("Track", 1:100),
  genre = sample(c("Pop", "Hip-Hop", "Rock", "EDM", "Country", "Jazz", "Classical"), 100, replace = TRUE)
)

# Summarize the genres
genre_distribution <- as.data.frame(table(spotify_data$genre))

# Calculate the percentages
genre_distribution$percentage <- round(100 * genre_distribution$Freq / sum(genre_distribution$Freq), 1)

# Create the pie chart with percentages
ggplot(genre_distribution, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5)) +
  labs(
    title = "Distribution of Genres Among Spotify Tracks",
    fill = "Genre"
  ) +
  theme_void() +  # Removes all axes and gridlines
  theme(legend.position = "right")

library(ggplot2)
library(tidyr)
library(dplyr)

flights <- read.csv("~/Downloads/CSC 302 /DATA/flights.csv")

str(flights)

if (!("year" %in% names(flights) && "month" %in% names(flights))) {
  if ("date" %in% names(flights)) {
    flights$date <- as.Date(flights$date, format = "%Y-%m-%d")
    flights$year <- as.numeric(format(flights$date, "%Y"))
    flights$month <- as.numeric(format(flights$date, "%m"))
  } else {
    stop("The 'year' and/or 'month' columns are missing, and the 'date' column is also missing in the flights data.")
}
flights <- flights %>% filter(!is.na(year) & !is.na(month))

monthly_flights <- flights %>%
  group_by(year, month) %>%
  summarise(passengers = sum(passengers, na.rm = TRUE))

p1 <- ggplot(data = monthly_flights) + 
  geom_line(aes(x = month, y = passengers, group = year, color = as.factor(year))) + 
  theme_bw() + 
  xlab('Month') + 
  ylab('Number of Passengers') + 
  ggtitle('Monthly Flights Over Years')


ggsave("~Downloads/CSC 302 /DATA/flights.jpg, plot = p1)




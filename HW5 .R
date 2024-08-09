library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)

#load dataset
load("~/Downloads/CSC 302 /DATA/house_prices.rda")

#line plot
ggplot(house_prices, aes(x = date, y = house_price_index)) +
  geom_line() +
  facet_wrap(~ state) +
  scale_x_continuous(breaks = as.Date(c("1980-01-01", "2000-01-01", "2020-01-01")), labels = c("80", "00", "20")) +
  labs(title = "Trend of House Price Index Over Years for Each State",
       x = "Year",
       y = "House Price Index") +
  theme_minimal()

#reshape the data
house_reshaped <- house_prices %>%
  gather(key = "measure", value = "value", -c(house_price_index, date, state))

#plot the data with two lines for house_price_index and unemploy_perc

ggplot(house_reshaped, aes(x = date, y = value, color = measure)) +
  geom_line() +
  facet_wrap(~ state) +
  scale_x_continuous(breaks = as.Date(c("1980-01-01", "2000-01-01", "2020-01-01")), labels = c("80", "00", "20")) +
  labs(title = "Trends of House Price Index and Unemployment Percentage Over Years for Each State",
       x = "Year",
       y = "Value") +
  theme_minimal()




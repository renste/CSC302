library(ggplot2)
library(dplyr)
library(lubridate)

load("~/Downloads/CSC 302 /DATA/preprint_growth.rda") #please change the path if needed

# Inspect the data
print(head(preprint_growth))

# Filter and process the data
biorxiv_growth <- preprint_growth %>%
  filter(archive == "bioRxiv") %>%
  filter(count > 0)

# Verify biorxiv_growth is created correctly
print(head(biorxiv_growth))

preprints <- preprint_growth %>%
  filter(archive %in% c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")) %>%
  filter(count > 0) %>%
  mutate(archive = factor(archive, levels = c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")))

print(head(preprints))

preprints_final <- filter(preprints, date == ymd("2017-01-01"))

# Verify preprints_final is created correctly
print(head(preprints_final))

start_date <- ymd("2015-01-01") # Modified start date
colors <- c("#FF5733", "#C70039", "#900C3F")





plot <- ggplot(preprints) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) +
  scale_y_continuous(
    limits = c(0, 600), expand = c(0, 0),
    name = "preprints / month",
    sec.axis = dup_axis(
      breaks = preprints_final$count,
      labels = c("arXiv q-bio", "PeerJ Preprints", "bioRxiv"),
      name = NULL)
  ) +
  scale_x_date(name = "year", limits = c(start_date, ymd("2017-01-01"))) +
  scale_color_manual(values = colors, name = NULL) +
  theme(legend.position = "none")



print(plot)

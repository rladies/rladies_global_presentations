# Figures originally created by Nicola Rennie for 2024 IWD
# https://github.com/rladies/IWD/commit/aad48b9822128d0a9495f26b81e71e55856f2719#diff-21b41dee63e010a47320caa5f797e3458c1fbfc58deda9cec64d14f31abf1833

# Modifying with slightly different colors

library(tidyverse)
library(jsonlite)
library(maps)
library(sf)


chapters_data <- jsonlite::fromJSON("https://raw.githubusercontent.com/rladies/meetup_archive/main/data/chapters_meetup.json")
events_data <- jsonlite::fromJSON("https://raw.githubusercontent.com/rladies/meetup_archive/main/data/events.json")

# Date of extraction assumed to be roughly today's date given github actions
# updates json daily
date_extraction <- paste(format(Sys.Date(), "%B"), year(Sys.Date()))

year(Sys.Date())


# Map of chapters ---------------------------------------------------------

num_chapters <- nrow(chapters_data)

chapters <- chapters_data |>
  as_tibble() |>
  select(lat, lon)

world <- map_data("world") |>
  filter(region != "Antarctica")

ggplot() +
  geom_map(
    data = world,
    mapping = aes(
      x = long,
      y = lat,
      map_id = region
    ),
    # #d3d3d3
    map = world, fill = "#d3d3d3",
    colour = "white", linewidth = 0.1
  ) +
  geom_point(
    data = chapters,
    mapping = aes(x = lon, y = lat),
    col = "#88398A", size = 0.4,
    fill = alpha("#88398A", 0.6),
    pch = 21
  ) +
  labs(
    title = paste(num_chapters, "R-Ladies Chapters"),
    caption = paste0("Data from meetup.com (", date_extraction, ") | Originally created by Nicola Rennie for IWD Campaign")
  ) +
  coord_sf() +
  theme_void(base_size = 10) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(
      face = "bold", size = rel(2),
      family = "Montserrat",
      colour = "#88398A",
      margin = margin(
        t = 0, r = 0, b = 6.6,
        l = 0, unit = "pt"
      )
    ),
    plot.caption = element_text(hjust = 0, family = "Montserrat")
  )

ggsave(paste0("figures/", Sys.Date(),"-chapter-map.png"), width = 5.5, height = 3, bg = "white")

# Events bar chart --------------------------------------------------------

events_data_clean <- events_data |>
  mutate(year = as.character(year(date))) |>
  filter(year != 2038)

num_events <- prettyNum(nrow(events_data_clean), big.mark = ",")


events_bar_data <- events_data_clean |>
  count(year)

ggplot(events_bar_data) +
  geom_col(aes(x = year, y = n), fill = "#88398A") +
  geom_text(aes(x = year, y = n + 40, label = n), colour = "#88398A", family = "Montserrat") +
  labs(title = paste(num_events, "R-Ladies Events"),
       x = "", y = "Number of events",
       caption = paste0("Data from meetup.com (", date_extraction, ") | Originally created by Nicola Rennie for IWD Campaign")
       ) +
  theme_void(base_size = 10) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(
      face = "bold", size = rel(2),
      colour = "#88398A",
      margin = margin(
        t = 0, r = 0, b = 6.6,
        l = 0, unit = "pt"
      ),
      family = "Montserrat"
    ),
    plot.caption = element_text(hjust = 0,    family = "Montserrat"),
    axis.text.x = element_text(family = "Montserrat", angle = 45),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000))

ggsave(paste0("figures/", Sys.Date(),"-events-bar.png"), width = 5.5, height = 3, bg = "white")


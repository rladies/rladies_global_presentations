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
      face = "bold", size = rel(1.4),
      family = "Montserrat",
      colour = "#88398A",
      margin = margin(
        t = 0, r = 0, b = 6.6,
        l = 0, unit = "pt"
      )
    ),
    plot.caption = element_text(hjust = 0, family = "Montserrat")
  )

ggsave("plots/chapters_maps/2024-chapter-map.png", width = 5.5, height = 3, bg = "white")

# Events bar chart --------------------------------------------------------

events_bar_data <- events_data |>
  mutate(year = as.character(year(date))) |>
  count(year) |>
  filter(year != 2038)

ggplot(events_bar_data) +
  geom_col(aes(x = year, y = n), fill = "#88398A") +
  geom_text(aes(x = year, y = n + 40, label = n), colour = "#88398A") +
  labs(title = "R-Ladies Events", x = "", y = "Number of events",
       caption = paste0("Source: meetup.com (", date_extraction, ")")) +
  theme_minimal(base_size = 10) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(
      face = "bold", size = rel(1.4),
      colour = "#88398A",
      margin = margin(
        t = 0, r = 0, b = 6.6,
        l = 0, unit = "pt"
      )
    ),
    plot.caption = element_text(hjust = 0)
  )

ggsave("plots/chapters_maps/2024-events-bar.png", width = 5.5, height = 3, bg = "white")

# Events bar chart --------------------------------------------------------

first_events_bar_data <- events_data |>
  group_by(group_urlname) |> 
  slice_min(date) |> 
  ungroup() |> 
  mutate(year = as.character(year(date))) |>
  count(year) |>
  filter(year != 2038)

ggplot(first_events_bar_data) +
  geom_col(aes(x = year, y = n), fill = "#88398A") +
  geom_text(aes(x = year, y = n + 8, label = n), colour = "#88398A") +
  labs(title = "New R-Ladies Chapters", x = "", 
       y = "Number of new chapters",
       caption = "Source: meetup.com (March 2024)") +
  theme_minimal(base_size = 10) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(
      face = "bold", size = rel(1.4),
      colour = "#88398A",
      margin = margin(
        t = 0, r = 0, b = 6.6,
        l = 0, unit = "pt"
      )
    ),
    plot.caption = element_text(hjust = 0)
  )

ggsave("plots/chapters_maps/2024-first-events-bar.png", width = 5.5, height = 3, bg = "white")

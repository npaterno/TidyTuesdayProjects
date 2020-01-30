library(tidyverse)
library(ggmap)
library(gganimate)
library(gifski)

altrenderer <- gifski_renderer(loop = FALSE)

sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

sf_map <- get_map(location = c(left = -122.53, 
                               bottom = 37.6993,
                               right = -122.35,
                               top = 37.8116),
                  zoom = 16,
                  color = "bw")

sf_plot_data <- sf_trees %>% 
  filter(!is.na(date)) %>% 
  separate(date, 
           into = c("year","month","day"),
           sep = "-")

num_years <- length(unique(sf_plot_data$year))

base_map <- ggmap(sf_map)+
  geom_point(data = sf_plot_data, mapping = aes(longitude, latitude, color = year),
             size = 0.05,
             alpha = 0.2, 
             na.rm = TRUE)+
  guides(color = FALSE)
  
  
animated_map <- base_map+
  transition_states(year)+
  ease_aes('linear')+
  labs(title = "Trees of San Francisco",
       subtitle = "Year: {closest_state}",
       x = "",
       y = "",
       caption = "Source: San Francisco Open Data Portal | Graph: @mathl3t3")
  
gif_map <- animate(animated_map,
                   renderer = altrenderer,
                   nframes = num_years*10,
                   fps = 10,
                   height = 800,
                   width = 800,
                   rewind = FALSE)

anim_save(animation = gif_map,
          filename = here::here("trees.gif"))

#NOT SAVING AS ANIMATION!!! SAVES PNG FILES

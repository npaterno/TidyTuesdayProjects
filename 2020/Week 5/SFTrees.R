library(tidyverse)
library(ggmap)
library(gganimate)

sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

sf_map <- get_map(location = c(left = -122.53, 
                               bottom = 37.6993,
                               right = -122.35,
                               top = 37.8116),
                  zoom = 16,
                  color = "bw")

#sf_trees_clean <- sf_trees %>% 
#  separate(site_info, 
#           into = c("site_main","site_sub_1","site_sub_2"), 
#           sep = ":") %>% 
#  separate(species, 
#           into = c("species","sub_species"),
#           sep = "::") %>% 
#  select(-c("address","tree_id","dbh","plot_size"))

base_plot <- ggmap(sf_map)+
  geom_point(data = sf_trees, mapping = aes(longitude, latitude),
             color = "lightgreen",
             size = 0.05,
             alpha = 0.2, 
             na.rm = TRUE)
  
  
base_plot+
  transition_states(date,
                    transition_length = 3,
                    state_length = 9)+
  ease_aes('linear')+
  labs(title = "Trees of San Francisco",
       subtitle = "Year: {round(frame_along,0)}",
       x = "",
       y = "",
       caption = "Source: San Francisco Open Data Portal | Graph: @mathl3t3")
  

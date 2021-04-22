# Load Packages
library(tidyverse)
library(lubridate)

# Load Data
netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')

# Wrangle
clean_data <- netflix_titles %>% 
  filter(country == "United States") %>% 
  mutate(actor = strsplit(cast, ", ")) %>% 
  unnest(actor) %>% 
  select(c(type, actor)) %>% 
  filter(actor != "NA") %>% 
  group_by(type, actor) %>% 
  summarize(appearances = n()) %>% 
  slice_max(appearances, n = 10, with_ties = FALSE)

year_added <- netflix_titles %>% 
  filter(country == "United States") %>% 
  mutate(actor = strsplit(cast, ", "),
         year_added = year(mdy(date_added))) %>% 
  unnest(actor) %>% 
  filter(actor %in% clean_data$actor) %>% 
  select(year_added, actor)

plot_data <- left_join(year_added, clean_data, by = "actor") %>% 
  fct_reorder(actor, appearance) #need to reorderby appearance

# Plot: Top Appearing Actors by Rating
ggplot(plot_data,
       aes(actor, fill = type))+
  geom_bar()+
  coord_flip()#+
  #facet_wrap(~year_added)



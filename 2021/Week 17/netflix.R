# Load Packages
library(tidyverse)
# Load Data
netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')

# Wrangle
clean_data <- netflix_titles %>% 
  filter(country == "United States" & type == "Movie") %>% 
  mutate(actor = strsplit(cast, ", ")) %>% 
  unnest(actor) %>% 
  select(c(actor)) %>% 
  filter(actor != "NA") %>% 
  group_by(actor) %>% 
  summarize(appearances = n()) %>% 
  slice_max(appearances, n = 10, with_ties = TRUE) %>% 
  mutate(actor = fct_reorder(actor, appearances))

# Plot: Top Appearing Actors by Rating
ggplot(clean_data, aes(actor, appearances))+
  geom_col(fill = "lightgoldenrod2",
           color = "goldenrod",
           size = 1.2)+
  coord_flip()+
  labs(
    title = "Top 10* Actors in Movies on Netflix",
    subtitle = "*including ties",
    x = "Actor",
    y = "Appearances",
    caption = "Source: Flixable | Graph: @mathl3t3"
  )+
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(size = 14,
                              face = "bold"),
    plot.subtitle = element_text(size = 8),
    plot.background = element_rect(fill = "cadetblue3"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "cadetblue3"),
    axis.text = element_text(color = "black",
                             face = "italic"),
    axis.title = element_text(face = "bold")
  )





library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2020, week = 28)

coffee_ratings <- tuesdata$coffee_ratings

country_summaries <- coffee_ratings %>% 
  mutate(
    country = case_when(
      str_detect(country_of_origin, "United States") ~ "United States",
      TRUE ~ country_of_origin 
    )
  )%>% 
  group_by(country, species) %>% 
  summarize(count = n(),
            mean_overall = mean(total_cup_points)
    
  )

ggplot(country_summaries, aes(count, mean_overall))+
  geom_point()

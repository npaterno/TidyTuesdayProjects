library(tidyverse)

hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')


night_summary <- hotels %>% 
  mutate(stay_id = row_number(),
         kids = case_when(children + babies < 1 ~ "no",
                          TRUE ~ "yes"),
         num_guests = adults + children + babies) %>% 
rename(week = "stays_in_week_nights",
         weekend = "stays_in_weekend_nights") %>% 
  pivot_longer(cols = c("week","weekend"),
               names_to = "night",
               values_to = "num_nights")

ggplot(night_summary, aes(num_guests, num_nights, group = stay_id, color = kids))+
  geom_point(alpha = 0.2)
library(dplyr)
library(ggplot2)
library(stringr)

sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

private_v_public <- sports %>% 
  mutate(type = case_when(
    str_detect(sector_name, "Public") == T ~ "Public",
    str_detect(sector_name, "Private") == T ~ "Private")
  )

test <- private_v_public %>% 
  group_by(type, sports) %>% 
  summarize(
    avg_men_rev = mean(rev_men, na.rm = T),
    avg_women_rev = mean(rev_women, na.rm = T)
  )
            
            
            
            
            
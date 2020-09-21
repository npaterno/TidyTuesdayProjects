library(tidyverse)

kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')

left_coast <- kids %>% 
  filter(state %in% c("California","Oregon","Washington"))

ggplot(left_coast, 
       aes(year, inf_adj_perchild))+
  geom_smooth(aes(color = state), na.rm = TRUE, se = FALSE)

ggplot(left_coast, aes(state, inf_adj_perchild))+
  geom_boxplot(aes(color = state))+
  facet_wrap(~year)

#Load Libraries
library(tidyverse)
library(usmap)

#Load Data
dog_moves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_moves.csv')
#dog_travel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_travel.csv')
#dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')

exports <- dog_moves %>% 
  filter(inUS == TRUE) %>% 
  select(c("location","exported")) %>% 
  rename(state = location) %>% 
  replace_na(list(exported=0))

imports <- dog_moves %>% 
  filter(inUS == TRUE) %>% 
  select(c("location","imported")) %>% 
  rename(state = location) %>% 
  replace_na(list(imported=0))  

plot_data <- dog_moves %>% 
  filter(inUS == TRUE) %>% 
  replace_na(list(imported = 0, exported = 0)) %>% 
  mutate(net_change = imported - exported) %>% 
  rename(state = location)

#Plot base map
map <- plot_usmap(data = plot_data, values = "net_change")+
  scale_fill_viridis_c(name = "Net change in adoptable \ndog population", option = "plasma")+
  labs(subtitle = "Where are adoptable dogs being sent?", 
       caption = "Source: PetFinder | Graph: @Mathl3t3")+
  theme(plot.background = element_rect(fill = "lightblue"),
        plot.subtitle = element_text(size = 14, 
                                     face = "italic"),
        legend.background = element_rect(fill = "lightblue"),
        legend.position = "bottom")

ggsave("adpotdogs.png",
       plot = map)

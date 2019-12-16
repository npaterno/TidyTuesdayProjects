#Load Libraries
library(tidyverse)
library(usmap)

#Load Data
dog_moves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_moves.csv')

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

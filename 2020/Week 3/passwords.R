library(tidyverse)

passwords <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

ggplot(passwords %>% filter(offline_crack_sec<20), 
       aes(x = category, y = offline_crack_sec))+
  geom_boxplot()+
  coord_flip()


ggplot(passwords,
       aes(str_length(password), strength, color = category))+
  geom_point()

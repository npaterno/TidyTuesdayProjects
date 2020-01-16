library(tidyverse)
library(hexbin)

passwords <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')



ggplot(passwords,
       aes(str_length(password)))+
  geom_histogram(binwidth = 1, color = "black", fill = "lightcyan")+
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(face = "bold",
                                  size = 18,
                                  family = "serif"),
        plot.subtitle = element_text(face = "italic",
                                     size = 12,
                                     family = "serif"),
        plot.background = element_rect(fill = "lightgoldenrod1"))+
  labs(title = "Is your password long enough?",
       subtitle = "Distribution of the length of the most common passwords",
       x = "Character Length",
       y = "Frequency")

ggsave(filename = "histogram.png")

cat_summary <- passwords %>% 
  na.omit() %>% 
  mutate(password_length = str_length(password)) %>% 
  group_by(category, password_length) %>% 
  summarize(count = n())

ggplot(cat_summary,
       aes(category, password_length, fill = count))+
  geom_tile()+
  coord_flip()+
  scale_fill_viridis_c(option = "plasma")+
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "peachpuff"), 
        legend.background = element_rect(fill = "peachpuff"),
        legend.text = element_text(face = "italic",
                                   family = "mono"),
        legend.title = element_text(face = "italic",
                                    family = "mono"))+
  labs(title = "Most Common Passwords",
       subtitle = "By Length and Category",
       x = "Password Length",
       y = "Category",
       fill = "Frequency")

ggsave("tile_plot.png")

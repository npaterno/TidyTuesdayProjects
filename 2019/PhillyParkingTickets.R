library(tidyverse)
library(hexbin)
library(lubridate)
library(ggdark)

tickets <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")

plot_a <- ggplot(tickets, 
                 aes(date(issue_datetime), hour(issue_datetime)))+
  geom_hex(bins = 10)+
  scale_fill_continuous(low = "Light Green", 
                        high = "Dark Red")+
  labs(title = "Density of Parking Tickets by Hour",
       subtitle = "Philly 2017",
       x = "Date",
       y = "Hour",
       fill = "Number of Tickets",
       caption = "Source: Open Data Philly | Graph: @mathl3t3")+
  dark_theme_minimal()+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   color = "Red"),
        axis.title.x = element_text(color = "White"),
        axis.text.y = element_text(color = "Red"),
        axis.title.y = element_text(color = "White"),
        text = element_text(color = "Red"),
        plot.title = element_text(color = "White"),
        plot.subtitle = element_text(color = "White"),
        plot.caption = element_text(color = "White"),
        legend.title = element_text(color = "White"))

        
ggsave("TicketDensity.png",
       plot = plot_a,
       height = 5,
       width = 7)

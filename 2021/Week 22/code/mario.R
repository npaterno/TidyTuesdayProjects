# Load Packages
library(tidyverse)
library(extrafont)

# Load Data
records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv')

# Wrangle Data: filter to top 10 drivers and add cumulative column
plot_data <- drivers %>% 
  filter(position <=10) %>% 
  mutate(records = replace_na(records, 0)) %>% 
  group_by(player) %>% 
  mutate(c_records = cumsum(records))

# Add font for plots: credit @fsuarez913 
# Source: https://www.dafont.com/super-mario-256.font
font_import(path = here::here("2021/Week 22/font"),
            pattern = "SuperMario256.ttf",
            prompt = FALSE)

loadfonts() 

# Add SuperMario color palette: black and white plus super smash bros (original) colors
mario_colors = c("#FFFFFF","#ED3636","#4E4EE9", "#FFDF1A", "#4EB94E", "#ACACAC", "#24D4C4", "#D41CE5", "#4A4559", "#616161")


# Plot: timeline progression of cumulative records
ggplot(plot_data, 
       aes(year, c_records, color = player))+
  geom_line()+
  labs(title = "Top 10 MarioKart 64 Record Holders",
       subtitle = "Cumulative Number of World Records",
       x = "Year", 
       y = "Cumulative Records",
       color = "Player",
       caption = "Source: Mario Kart World Records | Graph: @mathl3t3")+
  scale_color_manual(values = mario_colors)+
  theme(
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    plot.title.position = "plot", 
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "black"),
    text = element_text(color = "white",
                        family = "Super Mario 256"),
    title = element_text(size = 12),
    axis.text = element_text(color = "white"),
    axis.ticks = element_line(color = "white"),
    legend.background = element_rect(fill = "black"),
    legend.key = element_rect(fill = "black")
  )

ggsave(filename = "cumulative_records.png", 
       path = here::here("2021/Week 22/images"),
       height = 4,
       width = 6)

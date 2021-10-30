# Load Packages
library(dplyr)
library(ggplot2)
library(stringr) 
library(tidyr)
library(tidyselect)

# Load data
scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

# Filter/summarize caught and captured data 
caught_captured <- scoobydoo %>% 
  select(c(date_aired,starts_with("caught_"), starts_with("captured_"))) %>% 
  select(-c("caught_other", "caught_not")) %>% 
  pivot_longer(cols = starts_with("caught_"), names_to = "caught", values_to = "caught_tf") %>% 
  pivot_longer(cols = starts_with("captured_"), names_to = "captured", values_to = "captured_tf") %>% 
  mutate(year = lubridate::year(date_aired)) %>% 
  #mutate(year = as.numeric(str_replace(year, "^[0-9]{2}", ""))) %>% 
  select(-date_aired)

caught <- caught_captured %>% 
  count(year, caught, caught_tf) %>% 
  filter(caught_tf == TRUE) %>% 
  select(-caught_tf) %>% 
  separate(caught, into = c("caught_cap", "character"), sep = "_")

captured <- caught_captured %>% 
  count(year, captured, captured_tf) %>% 
  filter(captured_tf == TRUE) %>% 
  select(-captured_tf) %>% 
  separate(captured, into = c("caught_cap", "character"), sep = "_")

plot_data <- full_join(caught, captured, by = c("year", "caught_cap", "character", "n")) %>% 
  mutate(character = str_to_title(character))

# Plot
ggplot(plot_data, aes(year, n, color = caught_cap))+
  geom_point(size = 2, alpha = 0.7)+
  labs(
    title = "Who's catching the monster and\n who's getting captured?", 
    x = "Year", 
    y = "Frequency",
    caption = "Source: Kaggle via TidyTuesday | plot: @mathl3t3",
    color = "Caught or Captured?"
  )+
  theme(
    panel.background = element_rect(fill = "#FED38C"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#41525C"),
    plot.background = element_rect(fill = "#FED38C"),
    plot.title = element_text(face = "italic"),
    legend.background = element_rect(fill = "#E9AF10"),
    legend.box.background = element_rect(fill = "#E9AF10"),
    legend.key = element_rect(fill = "#E9AF10"),
    legend.position = "bottom",
    text = element_text(size = 18, family = "mono", color = "#41525C"),
    axis.text = element_text(color = "#41525C"),
    axis.ticks = element_blank(),
    strip.background.x = element_rect(fill = "#E9AF10"),
    strip.text.x = element_text(color = "#41525C")
  )+
  scale_color_manual(labels = c("Captured", "Caught"),
                     values = c("#B8BE19", "#A44138"))+
  facet_wrap(~character, ncol = 5)+
  coord_flip()

ggsave(here::here("2021/Week 29/images/caught_or_captured.png"))

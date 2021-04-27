#Load Packages
library(tidyverse)

#Load Data
departures <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')

#Wrangle
cause_data <- departures %>% 
  filter(departure_code <= 6) %>%
  mutate(
    classification = case_when(departure_code < 5 ~ "Involuntary",
                          TRUE ~ "Voluntary"),
    description = case_when(departure_code <= 2 ~ "Death or illness",
                            departure_code == 3 ~ "Job performance",
                            departure_code == 4 ~ "Legal violation or concern",
                            departure_code == 5 ~ "Retired",
                            TRUE ~ "New opportunity")
  ) %>% 
  group_by(classification, description, fyear_gone) %>% 
  summarize(count = n()) %>% 
  filter(fyear_gone > 1992 & fyear_gone < 2019)

#Plot
ggplot(cause_data, 
       aes(fyear_gone, count, color = description))+
  geom_line()+
  scale_x_continuous(breaks = c(1995, 2000, 2005, 2010, 2015))+
  labs(
    title = "Cause of CEO Departures",
    subtitle = "by year",
    x = "Year", 
    y = "Number of Departures",
    color = "Cause",
    caption = "Source: Gentry et al. via DataIsPlural | Graph: @mathl3t3"
  )+
  scale_color_viridis_d()+
  theme(
    panel.grid.major.y = element_line(color = "papayawhip"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "burlywood3"),
    plot.background = element_rect(fill = "burlywood3"),
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    legend.background = element_rect(fill = "papayawhip"),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black"),
    legend.key = element_rect(fill = "burlywood3"),
    plot.caption.position = "plot"
  )









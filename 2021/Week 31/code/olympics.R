# Load Packages -------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

# Load Data -----------------------------------------------------------
olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

# Wrangle Data --------------------------------------------------------
top_5_summer <- olympics %>% 
  filter(!is.na(medal) & season == "Summer" & year > 1999) %>% 
  group_by(noc) %>% 
  summarize(total_medals = n()) %>% 
  slice_max(order_by = total_medals, n = 5, with_ties = FALSE)

top_5_winter <- olympics %>% 
  filter(!is.na(medal) & season == "Winter") %>% 
  group_by(noc) %>% 
  summarize(total_medals = n()) %>% 
  slice_max(order_by = total_medals, n = 5, with_ties = FALSE)

summer_plot_data <- olympics %>% 
  filter(noc %in% top_5_summer$noc & year > 1999) %>% 
  select(-c(id, name, games, city)) %>% 
  mutate(bronze = case_when(medal == "Bronze" ~ 1,
                            TRUE ~ 0),
         silver = case_when(medal == "Silver" ~ 1,
                            TRUE ~ 0),
         gold = case_when(medal == "Gold" ~ 1,
                          TRUE ~ 0)) %>% 
  select(-medal) %>% 
  pivot_longer(cols = c(bronze, silver, gold),
    names_to = "medal", 
    values_to = "total") %>% 
 mutate(medal = ordered(medal, levels = c("bronze", "silver", "gold"))) %>% 
  group_by(noc, medal, year) %>% 
  summarize(count = sum(total)) 

# Custom Colors -------------------------------------------------------
medal_colors <- c("#824A02", "#D7D7D7", "#D6AF36")
oly_colors <- c("#0085C7", "#F4C300", "#000000", "#009F3D", "#DF0024")

# Plots ---------------------------------------------------------------

ggplot(summer_plot_data, aes(year, count, fill = noc))+
  geom_col(position = "dodge")+
  scale_fill_manual(values = oly_colors)+
  theme_void()+
  facet_wrap(~medal)
















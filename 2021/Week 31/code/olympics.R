# Load Packages -------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

# Load Data -----------------------------------------------------------
olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

# Wrangle Data --------------------------------------------------------
plot_data <- olympics %>% 
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
  mutate(medal = ordered(medal, levels = c("bronze", "silver", "gold")))

# Custom Colors -------------------------------------------------------
medal_colors <- c("#824A02", "#D7D7D7", "#D6AF36")
oly_colors <- c("#0085C7", "#F4C300", "#000000", "#009F3D", "#DF0024")

# Plots ---------------------------------------------------------------

ggplot(plot_data, aes(age, total, fill = medal))+
  geom_col()+
  scale_fill_manual(values = medal_colors)+
  facet_grid(sex ~ season, switch = "y")

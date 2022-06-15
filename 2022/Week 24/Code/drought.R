# Load Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(ggthemes)

# Load Data --------------------------------------------------------------------
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv')
drought_fips <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv')

# View Data --------------------------------------------------------------------
View(drought)
View(drought_fips)

# Exploratory Data Analysis ----------------------------------------------------
wa_drought <- drought %>% 
  filter(state == "washington") %>% 
  janitor::clean_names() %>% 
  separate(col = date, into = c("drop", "date"), sep = "_", convert = FALSE) %>% 
  mutate(date = ymd(date))

test <- drought %>% 
  filter(`-9` == 100) 
## The -9 category is an indicator for entries with no data
## The 0 variable is the percent of the state that is not abnormally wet 

wa_drought <- wa_drought %>% 
  filter(x9 == 0) %>% 
  select(-c(x9, drop, state)) %>% 
  pivot_longer(cols = c("d0", "d1", "d2", "d3", "d4"), names_to = "drought_category", values_to = "percent_of_state") %>% 
  group_by(date) %>% 
  mutate(total = sum(percent_of_state)) %>% 
  ungroup()
  
# Data Viz ---------------------------------------------------------------------
plot_data <- wa_drought %>% 
  filter(year(date) >= 2000)

ggplot(plot_data, aes(date, percent_of_state, fill = drought_category))+
  geom_col(position = "stack")

ggplot(plot_data, aes(date, percent_of_state, color = drought_category, fill = drought_category))+
  geom_ribbon(aes(ymin = 0, ymax = pmax(percent_of_state, 0))) + 
  scale_fill_brewer(palette = "Reds", 
                    labels = c("Abnormally dry", "Moderate", "Severe",
                               "Extreme", "Exceptional")) + 
  scale_color_brewer(palette = "Reds") + 
  theme_clean()+
  labs(
    title = "Drought in Washington",
    subtitle = "2000 - 2022",
    x = "Date", 
    y = "Percent of State",
    caption = "Source: drought.gov | Viz: @mathl3t3",
    fill = "Drought Conditions"
  ) + 
  guides(color = "none")

ggsave("drought.png")


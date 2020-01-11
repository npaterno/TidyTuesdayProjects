library(tidyverse)
library(lubridate)

rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')
temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

temp_city_year <- temperature %>% 
  mutate(yr = year(date),
         city_name = str_to_title(city_name)) %>% 
  select(-c(date, site_name)) %>% 
  #filter(temp_type == "max") %>% 
  group_by(city_name, yr) %>% 
  summarize(mean_temp = mean(temperature, na.rm = TRUE))

rainfall_city_yr <- rainfall %>% 
  rename(yr = year,
         rain = rainfall)%>% 
  mutate(rain = replace_na(rain,0)) %>% 
  select(city_name, yr, rain) %>% 
  group_by(city_name, yr) %>% 
  summarize(mean_rain = mean(rain, na.rm = TRUE))

plot_data <- left_join(rainfall_city_yr,temp_city_year, by = c("city_name", "yr"))

ggplot(plot_data %>% filter(yr>1999), aes(yr, mean_temp, group = city_name, color = city_name))+
  geom_smooth(na.rm = TRUE, se = FALSE)

library(tidyverse)
library(lubridate)

rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')
temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

temp_city_year <- temperature %>% 
  mutate(yr = year(date),
         city_name = str_to_title(city_name)) %>% 
  select(-c(date, site_name)) %>% 
  filter(temp_type == "max") %>% 
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

ggplot(plot_data, aes(yr, mean_temp, group = city_name, color = city_name))+
  geom_smooth(na.rm = TRUE)


# Mapping NSW Current Incidents in R -------------------------------------------
# AUTHOR: DEAN MARCHIORI
library(sf)
library(mapview)
library(tidyverse)

#' Current Incidents Feed (GeoJSON)
#' This feed contains a list of current incidents from the NSW RFS, 
#' and includes location data and Major Fire Update summary information where available. 
#' Click through from the feed to the NSW RFS website for full details of the update. 
#' GeoJSON is a lightweight data standard that has emerged to support the sharing of 
#' information with location or geospatial data. 
#' It is widely supported by modern applications and mobile devices.

url <- "http://www.rfs.nsw.gov.au/feeds/majorIncidents.json"

fires <- st_read(url)

fires

mapview(fires)

#' Hacky way to get rid of points within geometry collections
fire_poly <- fires %>% 
  st_buffer(dist = 0) %>% 
  st_union(by_feature = TRUE)

mapview(fire_poly)

fires %>% 
  mutate(pubdate = as.character(pubDate),
         pubdate = as.Date(pubdate))

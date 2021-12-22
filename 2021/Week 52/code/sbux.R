# Load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# Load data
starbucks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv')

## Helper to filter standard sizes
st_size <- c("short", "tall", "grande", "venti")

## Filter to standard drink sizes and calculate calories per ounce
st_drinks <- starbucks %>% 
  filter(size %in% st_size) %>% 
  mutate(cal_ounce = case_when(
    size == "short" ~ calories/8, 
    size == "tall" ~ calories/12,
    size == "grande" ~ calories/16,
    size == "venti" ~ calories/20
  ))

## Check if data is normal
ggplot(st_drinks, aes(cal_ounce, fill = size))+
  geom_histogram(binwidth = 5, color = "white", fill = openintro::IMSCOL["blue", "full"])+
  facet_wrap(~size)+
  labs(
    title = "Calories per ounce in Starbucks drinks",
    x = "Calories per ounce",
    y = "Number of drinks",
    caption = "Source: Starbucks Coffee Company | Viz @ mathl3t3"
  )+
  theme_minimal()

ggsave("histo.png", path = here::here("2021/Week 52/images"))

## Boxplots w/means 
ggplot(st_drinks, aes(cal_ounce, size))+
  geom_boxplot()+
  stat_summary(fun = "mean", geom = "point", col = openintro::IMSCOL["blue","full"])+
  stat_summary(fun = "mean", geom = "text", col = openintro::IMSCOL["blue","full"], vjust = -1, hjust = -1, aes(label = paste("Mean: ", round(..x.., digits = 2))))+
  theme_minimal()+
  labs(
    title = "Calories per ounce in Starbucks drinks",
    x = "Calories per ounce",
    y = "Size",
    caption = "Source: Starbucks Coffee Company | Viz @ mathl3t3"
  )
  
ggsave("box.png", path = here::here("2021/Week 52/images"))

summary(aov(cal_ounce ~ size, st_drinks))

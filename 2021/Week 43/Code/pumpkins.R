# Load Packages ----------------------------------------------------------------
library(dplyr)
library(magick)
library(readr)
library(ggplot2)
library(forcats)
library(showtext)
library(tidyr)

# Load Data --------------------------------------------------------------------
pumpkins <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')

# Wrangle Data -----------------------------------------------------------------
top_wa <- pumpkins %>% 
  separate(id, into = c("year", "type"), sep = "-") %>% 
  mutate(across(c(year, weight_lbs, ott, place), parse_number)) %>%
  filter(state_prov == "Washington" & type == "P") %>% 
  count(grower_name) %>% 
  rename(count = n) %>% 
  slice_max(count, n = 20) %>% 
  mutate(grower = fct_reorder(grower_name, count))

top_wa_summary <- pumpkins %>% 
  separate(id, into = c("year", "type"), sep = "-") %>% 
  mutate(across(c(year, weight_lbs, ott, place), parse_number)) %>%
  filter(grower_name %in% top_wa$grower_name & type =="P") %>% 
  group_by(grower_name) %>% 
  summarize(five_num = fivenum(weight_lbs)) %>% 
  mutate(names = c("min", "q1", "med", "q3", "max")) %>% 
  pivot_wider(names_from = names, values_from = five_num)

top_wa_boxplot_data <- pumpkins %>% 
  separate(id, into = c("year", "type"), sep = "-") %>% 
  mutate(across(c(year, weight_lbs, ott, place), parse_number)) %>%
  filter(grower_name %in% top_wa$grower_name & type =="P") %>% 
  group_by(grower_name) %>% 
  mutate(med = median(weight_lbs)) %>% 
  ungroup() %>% 
  mutate(grower = fct_reorder(grower_name, med))

# Font for plots ---------------------------------------------------------------
font_add_google(name = "Rock Salt", family = "rock")
showtext_auto()

# Plots ------------------------------------------------------------------------
p <- ggplot(top_wa, aes(grower, count))+
  geom_col(fill = "purple4", color = "black")+
  coord_flip()+
  labs(
    title = "Back for more?!",
    subtitle = "Most frequent giant pumpkin growers\nWashington state, \n2013 - 2021",
    y = "Number of Giant Pumpkins Grown", 
    x = "", 
    caption = "Source: Great Pumpkin Commonwealth via TidyTuesday | Plot: @mathl3t3"
  ) +
  theme(
    text = element_text(color = "orange", family = "rock"),
    title = element_text(size = 16, face = "bold"),
    panel.background = element_rect(fill = "darkslateblue"), 
    plot.background = element_rect(fill = "darkslateblue"),
    axis.text = element_text(color = "orange"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "orange"),
    panel.grid.minor.x = element_line(color = "orange")
  )

ggplot(top_wa_boxplot_data, aes(grower, weight_lbs)) +
  geom_boxplot()+
  stat_summary(fun = mean, geom = "point", color = "red")+
  coord_flip()

# Add image --------------------------------------------------------------------
pumpkin_original <- image_read("2021/Week 43/Images/pumpkin.png")

grob <- grid::rasterGrob(pumpkin_original, x = 0.04, y = 0.99, just = c('left', 'top'), 
                         width = unit(1, 'inches'))
g <- ggplotGrob(p)

g <- gtable::gtable_add_grob(
  g, grob, t = 1, l = 1, b = dim(g)[1], r = dim(g)[2]
)

ggplot() +
  annotation_custom(g) +
  theme_void()

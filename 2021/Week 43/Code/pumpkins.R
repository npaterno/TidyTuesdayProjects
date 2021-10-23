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

# Font for plots ---------------------------------------------------------------
font_add_google(name = "Rock Salt", family = "rock")
showtext_auto()

# Plots ------------------------------------------------------------------------
p <- ggplot(top_wa, aes(grower, count))+
  geom_col(fill = "purple4", color = "darkorange")+
  coord_flip()+
  labs(
    title = "Back for more?!",
    subtitle = "Most frequent giant pumpkin growers in Washington state, \n 2013 - 2021",
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

# Add image --------------------------------------------------------------------
# NEED  TO GET THIS PART TO WORK!
pumpkin_original <- image_read("2021/Week 43/Images/pumpkin.jpeg")
pumpkin_resized <- image_resize(pumpkin_original, "0.5x", filter = "Triangle")

figure <- image_graph(width = 1600, height = 900, res = 150)
p
dev.off()

out <- figure %>% image_composite(pumpkin_resized, offset = "+20+20")
out

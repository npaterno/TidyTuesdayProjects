# Load Packages
library(tidyverse)
library(grid)
library(ggpubr)

# Load data
tuesdata <- tidytuesdayR::tt_load(2020, week = 28)
coffee_ratings <- tuesdata$coffee_ratings

# Remove NA data
coffee_ratings <- coffee_ratings %>% 
  na.omit() %>% 
  pivot_longer(
    cols = c("flavor","aftertaste","acidity","body","balance","uniformity","clean_cup","sweetness","cupper_points", "moisture"),
    names_to = "category",
    values_to = "score"
  )


# Plot mean altitude against acidity
ggplot(
  coffee_ratings,
  aes(x = aroma, y = score)
)+
  geom_point()+
  geom_smooth(
    method = lm,
   se = FALSE
  )+
  stat_cor(method = "pearson", label.x = 6, label.y = 10)+
  labs(
    title = "Can coffee aroma predict the other categories?",
    x = "Aroma",
    y = "Score",
    caption = "Source: Coffee Qualit Database | Graph:@mathl3t3"
  )+
  facet_wrap(~category)


# Load packages
library(tidyverse)
library(tidymodels)
library(hexbin)

# Load data
mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')
landline <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv')

# Filter and merge data sets
mobile_merge <- mobile %>% 
  select(-code) %>% 
  rename(mobile_pop = total_pop,
         country = entity)

landline_merge <- landline %>% 
  select(code) %>% 
  rename(landline_pop = total_pop,
         country = entity)

merged_data <- mobile_merge %>% 
  left_join(landline_merge) %>% 
  na.omit() %>% 
  mutate_if(is.character, as.factor)

# Plot below shows mobile and landline data sets list different populations
ggplot(merged_data, 
       aes(year, mobile_pop - landline_pop))+
geom_point()

# Split data for model
set.seed(123)
data_split <- initial_split(merged_data, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# Build initial model
rf_mod <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

# Train model
set.seed(234)
rf_fit <- 
  rf_mod %>% 
  fit(country ~ ., data = train_data)

# Predict and evaluate model
rf_training_pred <- 
  predict(rf_fit, train_data) %>% 
  bind_cols(predict(rf_fit, train_data, type = "prob")) %>% 
  bind_cols(train_data%>% 
              select(c("country","continent")))

rf_training_pred %>%                
  accuracy(truth = country, .pred_class)

# Testing set predictions
rf_testing_pred <- 
  predict(rf_fit, test_data) %>% 
  bind_cols(predict(rf_fit, test_data, type = "prob")) %>% 
  bind_cols(test_data %>% 
              select(c("country","continent")))

rf_testing_pred %>%                   
  accuracy(truth = country, .pred_class)

ggplot(rf_testing_pred %>% filter(continent == "Europe"), 
       aes(.pred_class, country))+
  geom_hex(color = "white")+
  scale_fill_viridis_c(option = "plasma")


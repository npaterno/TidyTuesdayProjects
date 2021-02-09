#Load Packages
library(tidyverse)
library(ggthemes)
library(ggdark)

#Load Data
student_debt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv')

#Tile Plot Example for Student

ggplot(student_debt, 
       aes(year, race, fill = loan_debt))+
  geom_tile()+
  scale_fill_viridis_c(option = "plasma")+
  labs(
    title = "Student Load Debt",
    subtitle = "Average family debt",
    x = "Year",
    y = "Race",
    fill = "Loan Debt (in 2016 $)",
    caption = "Source: Urban Institute | Graph: @mathl3t3"
  )+
  dark_mode(theme_foundation())
  

library(tidyverse)
library(ggthemes)
library(zoo)

loans <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-26/loans.csv")

plot_data <- loans %>% 
  mutate(quarter_year = paste("Q", loans$quarter,"/", loans$year, sep = "")) %>% 
  mutate(quarter_year_date=as.Date(as.yearqtr(quarter_year, format = "Q%q/%y"))) %>%
  group_by(quarter_year_date) %>% 
  summarize("Voluntary Payments" = sum(voluntary_payments),
         "Wage Garnishments" = sum(wage_garnishments),
         "Outstanding Debts" = sum(starting)) %>% 
  pivot_longer(cols = c("Voluntary Payments", "Wage Garnishments", "Outstanding Debts"),
               names_to = "dollar_amount_type", 
               values_to = "amount") %>% 
  select(c(quarter_year_date, dollar_amount_type, amount))

plot_a <- ggplot(plot_data)+
  geom_smooth(aes(x = quarter_year_date,
                  y = amount/10^6,
                  group = dollar_amount_type,
                  color = dollar_amount_type),
              se = FALSE,
              na.rm = TRUE)+
  theme_economist()+
  labs(title = "Total Student Debts and Payments",
       subtitle = "by quarter",
       x = "Date",
       y = "Dollars (millions)",
       color = "Type",
       caption = "Source: Dept of Education | Graph: @Mathl3t3")

plot_b <- ggplot(plot_data %>% filter(dollar_amount_type!="Outstanding Debts"))+
  geom_smooth(aes(x = quarter_year_date,
                  y = amount/10^6,
              group = dollar_amount_type,
              color = dollar_amount_type),
            se = FALSE,
            na.rm = TRUE)+
  theme_economist()+
  labs(title = "Total Payments",
       subtitle = "by type",
       x = "Date",
       y = "Dollars (millions)",
       color = "Type",
       caption = "Source: Dept of Education | Graph: @Mathl3t3")

ggsave("PaymentsAndDebts.png",
       plot = plot_a,
       height = 5,
       width = 7)

ggsave("Payments.png",
       plot = plot_b,
       height = 5,
       width = 7)

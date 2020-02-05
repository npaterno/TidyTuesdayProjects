library(tidyverse)
library(ggdark)
library(ggthemes)
library(ggrepel)
library(ggalt)

attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')

sb_winners <- standings %>% 
  filter(sb_winner == "Won Superbowl")

mean_attendance <- attendance %>% 
  filter(team_name %in% sb_winners$team_name) %>% 
  group_by(team_name, year) %>% 
  na.omit() %>% 
  summarize(mean_attend = mean(weekly_attendance))

mean_attend_sb_winners <- full_join(mean_attendance,sb_winners) %>% 
  na.omit()

ggplot()+
  geom_line(mean_attendance, 
              mapping = aes(year, mean_attend/10^3, color = team_name),
              na.rm = TRUE)+
  geom_point(mean_attend_sb_winners,
             mapping = aes(year, mean_attend/10^3, color = team_name),
             size = 2)+
  annotate(geom="text", 
           x=2003, 
           y=57, 
           label="Dots represent Superbowl wins",
           color="white")+
  labs(title = "Average weekly attendance for teams that have won the Superbowl",
       subtitle = "2000-2019",
       x = "Year",
       y = "Average Attendance (in thousands)",
       caption = "Source: Pro Football Reference | Graph: @Mathl3t3",
       color = "Team Name")+
  dark_mode(theme_fivethirtyeight())

###########################################################################

seahawks_2013 <- games %>% 
  filter(year == 2013) %>% 
  filter(home_team == "Seattle Seahawks" | away_team == "Seattle Seahawks") %>% 
  mutate(opponent = case_when(
    home_team_name == "Seahawks" ~ away_team_name,
    TRUE ~ home_team_name
  ),
  seattle_pts = case_when(
    winner == "Seattle Seahawks" ~ pts_win,
    TRUE ~ pts_loss
  ),
  opp_pts = case_when(
    winner != "Seattle Seahawks" ~ pts_win,
    TRUE ~ pts_loss
  ))

ggplot(seahawks_2013)+
  geom_dumbbell(aes(x = seattle_pts, xend = opp_pts, y = date), 
                size = 3,
                color = "#A5ACAF",
                colour_x = "#69BE28", 
                colour_xend = "#D50A0A")+
  geom_text(data = filter(seahawks_2013, date == "September 8"),
            aes(x = seattle_pts, y = date),
            label = "Seahawks", fontface = "bold",
            color = "#69BE28",
            hjust = -0.5)+ 
  geom_text(data = filter(seahawks_2013, date == "September 8"),
            aes(x = opp_pts, y = date),
            label = "Opponent", fontface = "bold",
            color = "#D50A0A",
            hjust = 1.5)+ 
  theme(panel.background = element_rect(fill = "#002145"),
        panel.grid = element_blank())+
  labs(title = "Seattle Seahawks",
       subtitle = "Superbowl XLVII",
       x = "Points",
       y = "",
       caption = "Source: Pro Football Reference | Graph: @Mathl3t3")

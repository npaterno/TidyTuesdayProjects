library(tidyverse)
library(ggdark)
library(ggthemes)
library(ggrepel)
library(ggalt)
library(lubridate)
library(gridExtra)

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
       caption = "Source: Pro Football Reference | Graph: @Mathl3t3",
       color = "Team Name")+
  dark_mode(theme_fivethirtyeight())

ggsave("SuperbowlAttendance.png",
       width = 9,
       height = 4)

###########################################################################

seahawks_2013 <- games %>% 
  filter(year == 2013) %>% 
  filter(home_team == "Seattle Seahawks" | away_team == "Seattle Seahawks") %>% 
  rename(season = year) %>% 
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
  ),
  year = case_when(
    str_detect(week, "o") ~ 2014,
    TRUE ~ 2013
  ),
  date = mdy(paste(date,",",year))) 

ggplot(seahawks_2013)+
  geom_dumbbell(aes(x = seattle_pts, xend = opp_pts, y = date), 
                size = 3,
                color = "#A5ACAF",
                colour_x = "#69BE28", 
                colour_xend = "#D50A0A")+
  geom_text(data = seahawks_2013, 
            aes(x = seattle_pts, y = date),
            label = "Seahawks", 
            fontface = "bold",
            color = "#69BE28",
            vjust = 1.4)+ 
  geom_text(data = seahawks_2013, 
            aes(x = opp_pts, y = date),
            label = seahawks_2013$opponent, 
            fontface = "bold",
            color = "#D50A0A",
            vjust = -0.6)+ 
  theme(panel.background = element_rect(fill = "#002145"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(color = "#D50A0A"),
        plot.background = element_rect(fill = "#002145"),
        text = element_text(color = "#69BE28",
                            face = "italic",
                            family = "sans"),
        plot.title = element_text(size = 20),
        axis.text = element_text(color = "#69BE28", 
                                 size = 12))+
  labs(title = "Seattle Seahawks",
       subtitle = "Superbowl XLVII",
       x = "Points",
       y = "",
       caption = "Source: Pro Football Reference | Graph: @Mathl3t3")

ggsave("seahawks.png",
       width = 9,
       height = 8)

###############################################################

stat_totals <- games %>% 
  mutate(pts_total = pts_win + pts_loss,
         yds_total = yds_win + yds_loss,
         turnovers_total = turnovers_win + turnovers_loss,
         date = mdy(paste(date,",",year)))%>% 
  select(c(pts_total,yds_total,turnovers_total,date,winner))

pt_percentages <- games %>% 
  mutate(percent_win = pts_win/(pts_win+pts_loss),
         percent_loss = pts_loss/(pts_win+pts_loss)) %>% 
  select(c(week, percent_win, percent_loss)) %>% 
  group_by(week) %>% 
  summarize(Winner = mean(percent_win),
            Loser = mean(percent_loss)) %>% 
  pivot_longer(cols = c(Winner, Loser),
               names_to = "means",
               values_to = "percent")

p1 <- ggplot()+
  geom_histogram(stat_totals, 
                 mapping = aes(pts_total), 
                 color = "white",
                 fill = "blue", 
                 binwidth = 5)+
  labs(title = "Distribution of Total Points Scored",
       subtitle = "per game",
       x = "Points",
       y = "Number of Games")+
  theme(plot.background = element_rect(fill = "grey30",
                                       color = "grey30"),
        panel.background = element_rect(fill = "grey30"),
        panel.grid = element_blank(),
        axis.text = element_text(color = "white"),
        text = element_text(color = "white"))

p2 <- ggplot()+
  geom_bar(pt_percentages, 
           mapping = aes("", percent, fill = means),
           width = 1,
           stat = "identity")+
  coord_polar("y", start = 0)+
  scale_fill_viridis_d(option = "magma")+
  theme(axis.text = element_blank(),
        panel.background = element_rect(fill = "grey30"),
        plot.background = element_rect(fill = "grey30",
                                       color = "grey30"),
        text = element_text(color = "white"),
        legend.background = element_rect(fill = "grey30"),
        panel.grid = element_blank())+
  labs(title = "Average percent of total points scored",
       fill = "Team",
       x = "",
       y = "")

grid <- grid.arrange(p1,p2, 
                     nrow = 1,
                     top = grid::textGrob("Points Analysis", 
                                          grid::gp = gpar(col = "white")),
                     bottom = grid::textGrob("Source: Pro Football Reference | Graph: @Mathl3t3",
                                             gp = grid::gpar(col = "white"))
                     )
  
  
cowplot::ggdraw(grid)+
  theme(plot.background = element_rect(fill = "grey30"))

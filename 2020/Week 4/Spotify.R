library(tidyverse)
library(lubridate)

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

spotify_songs_2 <- spotify_songs %>% 
  mutate(duration_sec = duration_ms/1000,
         duration_time = hms::as_hms(duration_sec))


ggplot(spotify_songs_2, aes(duration_time, 
                            group = playlist_genre, 
                            color = playlist_genre))+
  geom_density()+
  labs(title = "Density of Song Length",
       x = "Song Length",
       y = "Density",
       color = "Genre")+
  theme(panel.background = element_rect(fill = "grey20"),
        panel.grid.major.x  = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "white") )

        
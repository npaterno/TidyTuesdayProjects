library(tidyverse)
library(lubridate)

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

spotify_songs_2 <- spotify_songs %>% 
  mutate(duration_sec = duration_ms/1000,
         duration_time = hms::as_hms(duration_sec),
         release_date = as.Date(track_album_release_date))


ggplot(spotify_songs_2, aes(duration_time, 
                            color = playlist_genre))+
  geom_density()+
  labs(title = "Density of Song Length",
       x = "Song Length",
       y = "Density",
       color = "Genre", 
       caption = "Source: spotifyR | Graph: @Mathl3t3")+
  theme(panel.background = element_rect(fill = "grey10"),
        panel.grid.major.x  = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "white"),
        plot.background = element_rect(fill = "grey10"),
        axis.text = element_text(color = "white"),
        text = element_text(color = "white"),
        legend.background = element_rect(fill = "grey10"),
        legend.key = element_rect(fill = "grey10"))

spotify_songs_3 <- spotify_songs_2 %>% 
  mutate(year = year(release_date),
         month = month(release_date),
         wkday = weekdays(as.POSIXct(release_date))) %>% 
  na.omit() %>% 
  group_by(playlist_subgenre, wkday) %>% 
  summarize(count = n())

spotify_songs_3$wkday <- ordered(
  spotify_songs_3$wkday, 
  levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(spotify_songs_3, aes(wkday, 
                            playlist_subgenre,
                            fill = count))+
  geom_tile(na.rm = TRUE)+
  scale_fill_viridis_c()

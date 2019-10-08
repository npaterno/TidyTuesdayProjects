# Load Libraries
library(tidyverse)
library(ggthemes)

# Load data
ipf_lifts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")

# Filter Men's Data
men<-ipf_lifts%>%
  filter(sex == "M")

# Calculate average squat, bench and deadlift for each lifter
mavg<-men%>%
  group_by(name)%>%
  mutate(mavg_squat = mean(best3squat_kg),
         mavg_bench = mean(best3bench_kg),
         mavg_dead = mean(best3deadlift_kg))%>%
  select(-c("event","federation","age_class","division","equipment"))%>%
  separate("date", into = c("year","month","day"), sep = "-", convert = TRUE)%>%
  mutate(decade = floor(year/10)*10)

# Isolate top 10 bench per year
mbench<-mavg%>%
  arrange(desc(best3bench_kg))%>%
  group_by(year)%>%
  top_n(10,best3bench_kg)

# Graph top 10 bench per year
ggplot(mbench, aes(year,best3bench_kg, color = age))+
  geom_point(na.rm=TRUE, size = 3)+
  scale_color_continuous_tableau(palette = "Green")+
  theme_economist()+
  labs(title = "Top 10 Male Bench Press",
       subtitle = "by year",
       x = "Year",
       y = "Weight (kg)", 
       color = "Age",
       caption = "Source: openpowerlifting.org | Graph: @Mathl3t3")+
  coord_flip()


# Repeat Bench Press Analysis for Women

# Filter Women's data
women<-ipf_lifts%>%
  filter(sex == "F")

# Calculate average squat, bench and deadlift for each lifter
wavg<-women%>%
  group_by(name)%>%
  mutate(wavg_squat = mean(best3squat_kg),
         wavg_bench = mean(best3bench_kg),
         wavg_dead = mean(best3deadlift_kg))%>%
  select(-c("event","federation","age_class","division","equipment"))%>%
  separate("date", into = c("year","month","day"), sep = "-", convert = TRUE)%>%
  mutate(decade = floor(year/10)*10)

# Isolate top 10 women's bench per year
wbench<-wavg%>%
  arrange(desc(best3bench_kg))%>%
  group_by(year)%>%
  top_n(10,best3bench_kg)

# Graph top 10 women's bench per year
ggplot(wbench, aes(year,best3bench_kg, color = age))+
  geom_point(na.rm=TRUE, size = 3)+
  scale_color_continuous_tableau(palette = "Red")+
  theme_economist()+
  labs(title = "Top 10 Female Bench Press",
       subtitle = "by year",
       x = "Year",
       y = "Weight (kg)", 
       color = "Age",
       caption = "Source: openpowerlifting.org | Graph: @Mathl3t3")+
  coord_flip()

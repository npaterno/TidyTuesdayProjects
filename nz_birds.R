library(tidyverse)
library(ggthemes)

nz_bird <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv")

vote_totals<-function(data, rank){
  place<-c("vote_1","vote_2","vote_3","vote_4","vote_5")
  
  total_vote_place<-data%>%
    filter(vote_rank == place[rank])%>%
    group_by(date, bird_breed)%>%
    summarize(count = n())%>%
    ungroup()
}  #Function to return data set of total num votes for a specific place

first_place<-vote_totals(nz_bird,1) #Data set of first place votes
second_place<-vote_totals(nz_bird,2) #Data set of second place votes
third_place<-vote_totals(nz_bird,3) #Data set of third place votes

#Join first and second place data sets
top2<-full_join(first_place, second_place, 
                by = c("date","bird_breed"))

#Join first, second and third place data sets, rename variables and tidydata for plotting
top3<-full_join(top2, third_place, 
                by = c("date","bird_breed"))%>%
  rename("First Place" = count.x,
         "Second Place" = count.y,
         "Third Place" = count)%>%
  pivot_longer(cols = c("First Place","Second Place","Third Place"),
               names_to = "place",
               values_to = "num_votes")

#Calculates the top 10 overall vote getters
top_10_overall<-top3%>%
  group_by(bird_breed)%>%
  summarize(total = sum(num_votes))%>%
  arrange(desc(total))%>%
  top_n(n = 10, wt = total)

#Subsets top 10 vote getters from top3 data set
plot_data<-top3%>%
  filter(bird_breed %in% top_10_overall$bird_breed)

ggplot(plot_data)+
  geom_line(mapping = aes(x = date, 
                            y = num_votes, 
                            color = bird_breed),
              na.rm = TRUE)+
  facet_wrap(~place)+
  theme_economist()+
  labs(title = "Top 10 Birds in New Zealand",
       subtitle = "Number of first, second and third place votes of the top 10 overall bird breeds",
       x = "Date of Vote",
       y = "Number of Votes",
       color = "Breed",
       caption = "Source: New Zealand Forest and Bird Orginization | Graph: @Mathl3t3")

#Load Libraries
library(tidyverse)
library(extrafont)

#Set up fonts
loadfonts(device = "win")

#Import data
nyc_regents <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-10/nyc_regents.csv")

#Tidy data for plot
longer_nyc_regents <- nyc_regents %>% 
  pivot_longer(cols = c("integrated_algebra","global_history","living_environment","english","us_history"),
               names_to = "category",
               values_to = "num_exams")

#Set up colors
jet.colors <- colorRampPalette(c("#F0FFFF", "cyan", "#007FFF", "yellow", "#FFBF00", "orange", "red", "#7F0000"), bias = 2.25)

#Plot regents exam scores based on subject
ggplot(longer_nyc_regents, 
       aes(score, num_exams, fill = category))+
  geom_col(na.rm = TRUE)+
  annotate("text", 
           x = 66, 
           y = 28000, 
           label = "Minimum\nRegents Diploma\nscore is 65", 
           hjust = 0,
           size = 3,
           color = "white") +
  scale_fill_manual(values = jet.colors(5), 
                    labels = c("English","Global History","Algebra","Biology","US History"))+
  labs(title = "NYC Regents Exam Scores: 2010",
       subtitle = "Were grades being 'adjusted' in order to improve graduation rates?",
       x = "Exam Score",
       y = "Number of Exams", 
       fill = "Subject", 
       caption = "Source: New York City Department of Education | Graph: @Mathl3t3")+
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "grey10"),
        legend.text = element_text(color = "White"),
        plot.background = element_rect(fill = "grey10"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "green3",
                                          linetype = 3),
        panel.background = element_rect(fill = "grey10"),
        text = element_text(family = "Bell MT",
                            color = "White"), 
        plot.subtitle = element_text(face = "italic"),
        axis.text = element_text(color = "White"))

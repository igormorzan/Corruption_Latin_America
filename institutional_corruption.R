library(tidyverse)
library(ggplot2)
library(ggthemes)

institution_corruption <- read.csv("perception-of-corruption-by-institution-global.csv")

bar_institution <- institution_corruption %>%
  mutate(percentage = Global.corruption.perception.by.institutions....) %>%
  ggplot(aes(x = Entity, y = as.numeric(Global.corruption.perception.by.institutions....))) +
  labs(title = "Perception of Corruption by Institution Across Latin America", 
       subtitle = "Percentage of survey respondents that think that each corresponding institution is corrupt",
       caption = "Source: Transparency International, GCB 2013") +
  geom_bar(stat="identity") +
  theme_economist() + 
  coord_flip() +
  xlab(NULL) + 
  ylab(NULL) +
  theme(legend.position = "none") +
  geom_text(aes(label = percentage), size = 4, colour = "white", position = position_stack(vjust = 0.5))

write_rds(bar_institution, "~/Desktop/Corruption_Latin_America/App/bar_institution.rds")

point_institution <- institution_corruption %>%
  mutate(percentage = Global.corruption.perception.by.institutions....) %>%
  ggplot(aes(x = Entity, y = as.numeric(Global.corruption.perception.by.institutions....), fill = Entity)) +
  labs(title = "Perception of Corruption by Institution Across Latin America", 
       subtitle = "Percentage of survey respondents that think that each corresponding institution is corrupt",
       caption = "Source: Transparency International, GCB 2013") +
  geom_point() +
  theme_economist() + 
  coord_flip() +
  xlab(NULL) + 
  ylab(NULL) +
  theme(legend.position = "none") +
  geom_text(aes(label = percentage), size = 4, colour = "black", position = position_stack(vjust = 1.1))

write_rds(point_institution, "~/Desktop/Corruption_Latin_America/App/point_institution.rds")
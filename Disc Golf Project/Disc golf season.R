library(readxl)
library(tidyverse)
library(ggplot2)

Disc_Golf_2023_Season <- read_excel("2023 Disc Golf Season.xlsx", 
                                       sheet = "Worlds Championship (Major)")

Disc_Golf_Data <- Disc_Golf_2023_Season %>%
  mutate(Winner = ifelse(Position == 1, 1, 0))


First_Place <- Disc_Golf_Data%>%
  filter(Winner == 1)

Loser <- Disc_Golf_Data %>%
  group_by(Name) %>%
  filter(all(Position != 1)) %>%
  ungroup() %>%
  select(Name, Position, Events, Ratings, Money_earned)%>%
  distinct()

Disc_Golf_Money <- Disc_Golf_Data %>%
  select(Name, Money_earned, Events, Position, Average_Tmax, Percipitation, Average_Wind) %>%
  group_by(Name) %>%
  summarise(Money_earned = sum(Money_earned),
            Position = sum(Position),
            Events = n()) %>%
  ungroup() %>%
  select(Name, Money_earned, Events, Position)


Cashed <- Disc_Golf_Money %>%
  filter(Money_earned != 0) %>%
  mutate(Average_Cash = Money_earned/Events,
         Average_Position = Position/Events,
         Percipitation = percipitation )


ggplot <- ggplot(data = Cashed) +
  geom_point(mapping = aes(x = Money_earned, y = Events, text = paste0(Name))) 
  plotly::ggplotly(ggplot, tooltip = "text")
  
Average_Cash <- ggplot(data = Cashed) + 
  geom_point(mapping = aes(x=Average_Cash, y = Events, text = paste0(Name)))
plotly::ggplotly(Average_Cash,tooltip = "text")


Averages <- ggplot(data = Cashed) +
  geom_point(mapping = aes(x=Events, y = Average_Position, text = paste0(Name)))
plotly::ggplotly(Averages, tooltip = "text")


Wooded <- Disc_Golf_Data %>%
  filter(Type == "Wooded")

Hybrid <- Disc_Golf_Data %>%
  filter(Type == "Hybrid")

Open <- Disc_Golf_Data %>%
  filter(Type == "Open")


Disc_Golf_Data$Type <- factor(Disc_Golf_Data$Type, levels = c("Wooded", "Hybrid", "Open"))
Specific_Player_Filter <- Disc_Golf_Data %>%
  filter(Name == "Calvin Heimburg")

unique_levels <- unique(Disc_Golf_Data$Type)
print(unique_levels)
lm_model <- lm(Position~  Classification + Percipitation + Average_Wind + Average_Tmax , data = Specific_Player_Filter)
summary(lm_model)
caret::varImp(lm_model)

library(readxl)
library(tidyverse)
library(ggplot2)
### Data Imports ###
 Overall_Stats <- read_excel("dg.xlsx", sheet = "Overall Stats")
 FPO_Elite <- read_excel("dg.xlsx", sheet = "FPO Elite Stats")
 MPO_Elite <- read_excel("dg.xlsx", sheet = "MPO Elite Stats")
 MPO_Major <- read_excel("dg.xlsx", sheet = "MPO Major Stats")
 FPO_Major <- read_excel("dg.xlsx", sheet = "FPO Major Stats")
 ### ###
 
 ### Data Cleanup ###
 FPO_Elite <- FPO_Elite %>%
   rename(Elite_Average_Finish = Average_Finish, Elite_DNF = DNF, Elite_Podium_Percentage = Podium_Percentage, Elite_Top10 = Top_Ten_Percentage, Elite_Top20 = Top_Twenty_Percentagee)

  MPO_Elite <-  MPO_Elite %>%
   rename(Elite_Average_Finish = Average_Finish, Elite_DNF = DNF, Elite_Podium_Percentage = Podium_Percentage, Elite_Top10 = Top_Ten_Percentage, Elite_Top20 = Top_Twenty_Percentagee)
 
  FPO_Major <- FPO_Major %>%
    rename(Major_Average_Finish = Average_Finish, Major_DNF = DNF)
  
  MPO_Major <- MPO_Major %>%
    rename(Major_Average_Finish = Average_Finish, Major_DNF = DNF)

  
 Overall_stats_MPO <- Overall_Stats %>%
   mutate(MPO = ifelse(Division == "MPO", 1, 0)) %>%
   filter(MPO == 1) %>%
   select(-MPO)
 
 Overall_stats_FPO <- Overall_Stats %>%
   mutate(MPO = ifelse(Division == "MPO", 1, 0)) %>%
   filter(MPO == 0) %>%
   select(-MPO)
 ### ###  
 
 ### Joins ###
   Overall_stats_FPO <-  Overall_stats_FPO %>%
     left_join(FPO_Elite, by = "Name") %>%
     left_join(FPO_Major, by = "Name")
   
   Overall_stats_MPO <-  Overall_stats_MPO %>%
     left_join(MPO_Elite, by = "Name") %>%
     left_join(MPO_Major, by = "Name")
 
 ### ###
   
   
   
   ggplot(data = Overall_stats_MPO) +
     geom_point(mapping = aes(y = Events, x = Win_Percentage))
   
 plot <-   ggplot2:: ggplot(Overall_stats_MPO) +  
     geom_point(aes(y = Starts, x = Elite_Win_Percentage, text = paste0(Name))) 
   plotly::ggplotly(plot, tooltip = "text")
   
   options(scipen = 999)
   
  
   plot2 <-   ggplot2::ggplot(Overall_stats_MPO) +  
     geom_point(aes(y = Major_Starts, x = Major_Wins, text = paste0(Name))) 
   plotly::ggplotly(plot2, tooltip = "text")
   
   View(plot2)
   
   plot3 <-   ggplot2:: ggplot(Overall_stats_MPO) +  
     geom_point(aes(y = Events, x = Total_Cash , text = paste0(Name))) 
   plotly::ggplotly(plot3, tooltip = "text")
   seq(0, 100, by = 5)
   
   Elite_Top10_Graph <-   ggplot2:: ggplot(Overall_stats_MPO) +  
     geom_histogram(aes( x = Elite_Top10, text = paste0(Name)),color = "black", fill = "blue") +
     scale_x_continuous(breaks = seq(0, 100, by = 5))
   plotly::ggplotly(Elite_Top10_Graph, tooltip = "text")
   
   plot5 <-   ggplot2:: ggplot(Overall_stats_MPO) +  
     geom_point(aes(y = Major_Starts, x = Major_Money_Earned, text = paste0(Name))) 
   plotly::ggplotly(plot5, tooltip = "text")
   
   
   
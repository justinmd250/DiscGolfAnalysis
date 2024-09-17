library(readxl)
library(tidyverse)
library(ggplot2)
library(party)
library(rpart)
library(rpart.plot)
library(tree)
library(caret)  # For model training and evaluation
library(purrr)
library(randomForest)
Disc_Golf_Data <- read_excel("2023 Disc Golf Season.xlsx", 
                                       sheet = "2021-2023 Season")


Disc_Golf_Data <- Disc_Golf_Data %>%
  group_by(Events) %>%
  mutate(Avg_OB_Rate = mean(OB_Rate, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(OB_Rate_Gained = Avg_OB_Rate - OB_Rate) %>%
  rename(Start_Date = 'Start Date')%>%
  rename(Fairway_Hits = "Fairway Hits") %>%
  mutate(Winner = ifelse(Position == 1, 1, 0))

colSums(is.na(Disc_Golf_Data))
Disc_Golf_Data <- na.omit(Disc_Golf_Data)

str(Disc_Golf_Data)
First_Place <- Disc_Golf_Data%>%
  filter(Winner == 1)



# Load required libraries
library(dplyr)
library(lubridate)

# Function to calculate weighted average statistics by player across years
weighted_stats_by_player <- function(data) {

  
  # Ensure Start Date is in POSIXct format and extract the year
  data <- data %>%
    mutate(Year = year(Start_Date))  # Extract year from POSIXct Start_Date
  
  # Filter for the years 2021 to 2023
  data <- data %>% filter(Year >= 2021 & Year <= 2023)
  
  # Filter players who have data for all three years (2021, 2022, 2023)
  players_with_all_years <- data %>%
    group_by(Name) %>%
    filter(n_distinct(Year) == 3) %>%
    ungroup()
  
  # Define weights for each year
  year_weights <- data.frame(
    Year = c(2021, 2022, 2023),
    Weight = c(0.2, 0.6, 1.0)  # Assign weights (can be adjusted as needed)
  )
  
  # Merge the data with the year weights
  players_with_all_years <- players_with_all_years %>%
    left_join(year_weights, by = "Year")
  
  # Group by player and calculate the weighted average for numeric columns
  result <- players_with_all_years %>%
    group_by(Name) %>%
    summarise(across(where(is.numeric), 
                     ~ weighted.mean(.x, Weight, na.rm = TRUE)), 
              .groups = 'drop')
  
  return(result)
}

# Example of running the function
weighted_averaged_data <- weighted_stats_by_player(Disc_Golf_Data)

# Show the results
print(weighted_averaged_data)



























# Load required libraries
library(dplyr)
library(lubridate)

# Function to calculate average statistics by player and year
average_stats_by_player_year <- function(data) {
  
  # Ensure Start Date is in date format and extract the year
  data <- data %>%
    mutate(Year = year(Start_Date))  # Extract year from Start Date
  
  # Filter for the years 2021 to 2023
  data <- data %>% filter(Year >= 2021 & Year <= 2023)
  year_weights <- data.frame(
    Year = c(2021, 2022, 2023),
    Weight = c(0.5, 0.7, 1.0)  # Assign weights (can be adjusted as needed)
  )
  
  # Merge the data with the year weights
  data <- data %>%
    left_join(year_weights, by = "Year")
  
  # Group by player and calculate the weighted average for numeric columns
  result <- data %>%
    group_by(Name) %>%
    summarise(across(where(is.numeric), 
                     ~ weighted.mean(.x, Weight, na.rm = TRUE), .names = "weighted_{col}"), 
              .groups = 'drop')
  
  # Group by player name and year, then calculate the average of numeric columns
  result <- data %>%
    group_by(Name, Year) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = 'drop')  # Average for numeric columns
  
  return(result)
}

# Example of running the function
Averaged_Player_Data <- average_stats_by_player_year(Disc_Golf_Data)

# Show the results
print(Averaged_Player_Data)











Disc_Golf_Money <- Disc_Golf_Data %>%
  select(Name, Money_earned, Events, Position, Average_Tmax, Percipitation, Average_Wind) %>%
  group_by(Name) %>%
  summarise(Money_earned = sum(Money_earned),
            Position = sum(Position),
            Events = n()) %>%
  ungroup() %>%
  select(Name, Money_earned, Events, Position)


Cashed <- Disc_Golf_Data %>%
  select(Name, Money_earned, Events, Position, Average_Tmax, Percipitation, Average_Wind) %>%
  group_by(Name) %>%
  summarise(Money_earned = sum(Money_earned),
            Position = sum(Position),
            Events = n())%>%
  filter(Money_earned != 0) %>%
  mutate(Average_Cash = Money_earned/Events,
         Average_Position = Position/Events)
      


ggplot <- ggplot(data = Cashed) +
  geom_point(mapping = aes(y = Money_earned, x = Events, text = paste0(Name))) 
  plotly::ggplotly(ggplot, tooltip = "text")
  
Average_Cash <- ggplot(data = Cashed) + 
  geom_point(mapping = aes(x=Average_Cash, y = Events, text = paste0(Name)))
plotly::ggplotly(Average_Cash,tooltip = "text")


Averages <- ggplot(data = Cashed) +
  geom_point(mapping = aes(y=Events, x = Average_Position, text = paste0(Name, Average_Position)))
plotly::ggplotly(Averages, tooltip = "text")


Wooded <- Disc_Golf_Data %>%
  filter(Type == "Wooded")

Hybrid <- Disc_Golf_Data %>%
  filter(Type == "Hybrid")

Open <- Disc_Golf_Data %>%
  filter(Type == "Open")


Average_Position_By_Course_Type <- Disc_Golf_Data %>%
  filter(Events > 0) %>%
  group_by(Name, Type) %>%
  summarise(Money_earned = sum(Money_earned),
            Position = sum(Position),
            Events = n()) %>%
  filter(Money_earned != 0) %>%
  mutate(Average_Cash = Money_earned / Events,
         Average_Position = Position / Events) %>%
  group_by(Name) %>%
  filter(Events > 5)%>%
  summarize(Average_Position_Wooded = mean(Average_Position[Type == "Wooded"]),
            Average_Position_Open = mean(Average_Position[Type == "Open"]),
            Average_Position_Hybrid = mean(Average_Position[Type == "Hybrid"]),
            Average_Cash_Wooded = mean(Average_Cash[Type == "Wooded"]),
            Average_Cash_Hybrid = mean(Average_Cash[Type == "Hybrid"]),
            Average_Cash_Open = mean(Average_Cash[Type == "Open"]),
            Events = n())%>%
            filter(Events == 3)

wooded_data <- subset(Disc_Golf_Data, Type == "Wooded")
# Print the result

Disc_Golf_Data$Type <- factor(Disc_Golf_Data$Type, levels = c("Wooded", "Hybrid", "Open"))
Specific_Player_Filter <- Disc_Golf_Data %>%
  filter(Percipitation > 0.05, Average_Wind > 10)

unique_levels <- unique(Disc_Golf_Data$Type)
print(unique_levels)
lm_model_open <- lm(Position ~  Gaind_Tee_To_Green + Gained_c1x + Gained_C2 + OB_Rate_Gained + Scramble + Percipitation + Average_Wind, data = Open)
summary(lm_model_open)
caret::varImp(lm_model_open)
lm_model_hybrid <- lm(Position ~ Gaind_Tee_To_Green + Gained_c1x + Gained_C2 + OB_Rate_Gained + Scramble + Percipitation + Average_Wind, data = Hybrid)
summary(lm_model_hybrid)
caret::varImp(lm_model_hybrid)
lm_model_wooded <- lm(Position ~ Gaind_Tee_To_Green  + Gained_c1x + Gained_C2 + OB_Rate_Gained + Scramble + Percipitation + Average_Wind, data = Wooded)
summary(lm_model_wooded)
caret::varImp(lm_model_wooded)

str(Disc_Golf_Data)


tree_model_wooded <- rpart(Position ~ Gaind_Tee_To_Green + Gained_c1x + Gained_C2 + OB_Rate_Gained + Scramble + Percipitation + Average_Wind, 
                           data = Wooded, 
                           method = "anova") # Use "anova" for continuous outcomes

summary(tree_model_wooded)

# Plot the decision tree
rpart.plot(tree_model_wooded, 
           main = "Decision Tree for Position",
           type = 3, # Type 3 shows the fitted values and node numbers
           cex = 0.8)
tree_model_hybrid <- rpart(Position ~ Gaind_Tee_To_Green + Gained_c1x + Gained_C2 + OB_Rate_Gained + Scramble + Percipitation + Average_Wind, 
                           data = Hybrid, 
                           method = "anova") # Use "anova" for continuous outcomes

summary(tree_model_hybrid)

# Plot the decision tree
rpart.plot(tree_model_hybrid, 
           main = "Decision Tree for Position",
           type = 3, # Type 3 shows the fitted values and node numbers
           cex = 0.8)


tree_model_open <- rpart(Position ~ Gaind_Tee_To_Green + Gained_c1x + Gained_C2 + OB_Rate_Gained + Scramble + Percipitation + Average_Wind, 
                           data = Open, 
                           method = "anova") # Use "anova" for continuous outcomes

summary(tree_model_open)






# Plot the decision tree
rpart.plot(tree_model_open, 
           main = "Decision Tree for Position",
           type = 3, # Type 3 shows the fitted values and node numbers
           cex = 0.8)


features <- c("Gaind_Tee_To_Green", "Gained_c1x", "Gained_C2", "OB_Rate_Gained", "Scramble", "Percipitation", "Average_Wind")
target <- "Position"

model_data <- wooded_data %>%
  select(all_of(features), all_of(target))


set.seed(123)  # For reproducibility
train_index <- createDataPartition(model_data[[target]], p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]


model <- train(
  as.formula(paste(target, "~ .")),
  data = train_data,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)  # 10-fold cross-validation
)
predictions <- predict(model, test_data)

# Compare predictions with actual values
results <- data.frame(
  Actual = test_data[[target]],
  Predicted = predictions
)

# Calculate performance metrics
postResample(predictions, test_data[[target]])

# Plot actual vs. predicted values
ActualPredicted <- ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Actual vs. Predicted Values",
       x = "Actual Values",
       y = "Predicted Values")
plotly::ggplotly(ActualPredicted, tooltip = "text")






# Load required libraries
library(dplyr)
library(tidyr)



# Simulate round scores based on a combination of these parameters
# Higher strokes gained metrics mean lower scores (better performance)
simulate_score <- function(Gaind_Tee_To_Green, Gained_c1x, Gained_C2, OB_Rate_Gained, Scramble) {
  base_score <- 64  # Base score (e.g., par 70 course)
  adjustment <-  - (1 *Gaind_Tee_To_Green + 1 * Gained_c1x + 0.7 * Gained_C2 + 0.5 * OB_Rate_Gained  + 0.40 * Scramble)
  return(base_score + adjustment + rnorm(1, 0, 1)) } # Adding randomness to the score


# Simulate 4 rounds for each player (like a real tournament)
tournament_results <- weighted_averaged_data %>%
  rowwise() %>%
  mutate(
    round_1 = simulate_score(Gaind_Tee_To_Green, Gained_c1x, Gained_C2, OB_Rate_Gained, Scramble),
    round_2 = simulate_score(Gaind_Tee_To_Green, Gained_c1x, Gained_C2, OB_Rate_Gained, Scramble),
    round_3 = simulate_score(Gaind_Tee_To_Green, Gained_c1x, Gained_C2, OB_Rate_Gained, Scramble),
    round_4 = simulate_score(Gaind_Tee_To_Green, Gained_c1x, Gained_C2, OB_Rate_Gained, Scramble)
  ) %>%
  ungroup() %>%
  mutate(total_score = round_1 + round_2 + round_3 + round_4) %>%
  arrange(total_score)  # Sort by total score, lower score is better

# Rank the players
tournament_results <- tournament_results %>%
  mutate(rank = rank(total_score))

# Show the tournament results
print(tournament_results %>% select(Name, total_score, rank))
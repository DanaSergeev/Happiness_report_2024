#### Uploading Data and Packages
library(tidyverse) 
data <- read.csv("C:/Users/danas/OneDrive/Desktop/פסיכולוגיה- תואר שני/קורס R/Final_Assignment/World_Happiness_2024.csv")


#### Raw Data---- 
raw_data <- read.csv("World_Happiness_2024.csv")
write.csv(raw_data, "raw_happiness_data.csv", row.names = FALSE)

# Types of Variables
str(data) 
summary(data)
head(data) 
#Country.name, 
#Ladder.score, 
#upperwhisker, 
#lowerwhisker, 
#Explained.by..Log.GDP.per.capita,
#Explained.by..Social.support, 
#Explained.by..Healthy.life.expectancy, 
#Explained.by..Freedom.to.make.life.choices,
#Explained.by..Generosity, 
#Explained.by..Perceptions.of.corruption, 
#Dystopia...residual

# Summary Table for All Numeric Variables
summary_table <- data |>
  summarise(
    across(where(is.numeric), list(
      Mean = ~mean(.x, na.rm = TRUE),
      SD = ~sd(.x, na.rm = TRUE),
      Min = ~min(.x, na.rm = TRUE),
      Max = ~max(.x, na.rm = TRUE),
      N = ~sum(!is.na(.x))
    ), .names = "{.col}_{.fn}")
  ) |>
  pivot_longer(cols = everything(), names_to = c("Variable", "Statistic"), names_sep = "_") |>
  pivot_wider(names_from = Statistic, values_from = value)

# Descriptive Statistics
library(knitr)
kable(summary_table, caption = "Descriptive Statistics for All Variables")


# Distribution of the Happiness Index (Dependent Variable)
ggplot(data, aes(x = Ladder.score)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  geom_density(aes(y = ..count.. * 0.5), color = "red", size = 1) +
  labs(title = "Distribution of Happiness Scores", x = "Ladder Score", y = "Number of Countries")

library(ggplot2)
library(tidyr)

#Distribution Plots for All Variables
data |>
  pivot_longer(cols = where(is.numeric), names_to = "Variable", values_to = "Value") |>
  ggplot(aes(x = Value)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 20) +
  # הצגת את כל המשתנים בגרפים נפרדים
  facet_wrap(~ Variable, scales = "free") + 
  labs(title = "Distribution of All Numeric Variables", x = "Value", y = "Count")


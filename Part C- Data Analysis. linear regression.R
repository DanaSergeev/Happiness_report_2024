#### Uploading Data and Package

data <- read.csv("C:/Users/danas/OneDrive/Desktop/פסיכולוגיה- תואר שני/קורס R/Final_Assignment/clean_happiness_data.csv")

#### First Research Question 
###Does the relationship between freedom and perceived corruption differ between developed and developing countries?

## Creating a linear model to test both main effects and interaction
linear_model <- lm(corruption ~ freedom * developed, data = data)
summary(linear_model)

# Visualizing the effect of freedom on corruption perception in developed and developing countries
ggplot(data, aes(x = freedom, y = corruption, color = as.factor(developed))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Effect of Freedom on Perceived Corruption in Developed and Developing Countries",
       x = "Freedom Level",
       y = "Perceived Corruption",
       color = "Development Level") +
  theme_minimal()



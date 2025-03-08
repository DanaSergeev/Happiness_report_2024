#### Uploading Data and Package

data <- read.csv("C:/Users/danas/OneDrive/Desktop/פסיכולוגיה- תואר שני/קורס R/Final_Assignment/clean_happiness_data.csv")

#### Second Research Question ----
###How does perceived corruption affect national happiness, and does this effect vary depending on a country's level of freedom?

## Creating a logistic model to test both main effects and interaction
model1 <- glm(happy_country ~ 1, data = data, family = binomial)
model2 <- glm(happy_country ~  corruption*freedom + freedom + corruption, 
              data = data, family = binomial)

summary(model1)
summary(model2)

# odds ratio
exp(coef(model2))
exp(confint(model2))

# Calculating Odds
data$predicted_probs <- predict(model2, type = "response")  
data$predicted_odds <- data$predicted_probs / (1 - data$predicted_probs)  

# Odds of National Happiness as a Function of Perceived Corruption, with Freedom Level Indicator
ggplot(data, aes(x = corruption, y = predicted_odds, color = freedom)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  scale_y_log10() +  
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Effect of Perceived Corruption on National Happiness",
       x = "Perceived Corruption",
       y = "Odds of Being Happy",
       color = "Freedom Level") +
  theme_minimal()

#### ROC ----

library(pROC)

data$predict_model1 <- predict(model1, type = "response")
data$predict_model2 <- predict(model2, type = "response")

roc_model1 <- roc(data$happy_country, data$predict_model1)
roc_model2 <- roc(data$happy_country, data$predict_model2)

# Confusion matrix
threshold <- 0.5
table(Predicted = (data$predict_model2 > threshold)*1, Actual = data$happy_country)

# AUC
auc(roc_model1) #0.5
auc(roc_model2) #0.8413

# Plot ROC curves
plot(roc_model1, col = "blue", main = "ROC Curve Comparison")
plot(roc_model2, add = TRUE, col = "red")
legend("bottomright", legend = c("Model 1 (Baseline)", "Model 2 (Predictors + Interaction)"), 
       col = c("blue","red"), lwd = 2)







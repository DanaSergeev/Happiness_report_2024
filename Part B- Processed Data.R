#### Uploading Data and Packages

data <- read.csv("C:/Users/danas/OneDrive/Desktop/פסיכולוגיה- תואר שני/קורס R/Final_Assignment/World_Happiness_2024.csv")
library(tidyverse) 

#### Processed Data ----
processed_data <- data

# Renaming Variables
processed_data <- processed_data |>
  rename(
    country_name = Country.name,
    happiness = Ladder.score,
    GDP = Explained.by..Log.GDP.per.capita,
    social_support = Explained.by..Social.support,
    HLE = Explained.by..Healthy.life.expectancy,
    freedom = Explained.by..Freedom.to.make.life.choices,
    generosity = Explained.by..Generosity,
    corruption = Explained.by..Perceptions.of.corruption,
  )
processed_data <- processed_data |>
  rename(unexplained_happiness = Dystopia...residual)


### Adding a Dichotomous Variable "Happiness"

# Above average (5.5): Happy (1), Below average (5.5): Not happy (0)
processed_data$happy_country <- ifelse(processed_data$happiness > 5.5, 1, 0)

# Defining "Happiness" as a Factor
processed_data$happy_country <- as.factor(processed_data$happy_country)
is.factor(processed_data$happy_country)


### Adding Another Dichotomous Variable "Developed/Developing Country" Based on GDP
# Using the Percentile Where Developed Countries Are Found in World Bank Reports
# Looking for the Top 25% Percentile
threshold <- quantile(processed_data$GDP, 0.75, na.rm = TRUE) 
threshold

# Defining "developed" as a Factor
processed_data$developed <- ifelse(processed_data$GDP > threshold, 1, 0)
processed_data$developed <- as.factor(processed_data$developed)
is.factor(processed_data$developed)

table(processed_data$developed) 
# 35 Developed countries, 105 Developing


### Adding a "Continent" Variable

## Using countrycode
library(countrycode)

# Check if the package works for specific countries by testing continent assignment
test_countries <- c("France", "Brazil", "Japan", "India", "South Africa")
test_continents <- countrycode(test_countries, origin = "country.name", destination = "continent")
data.frame(Country = test_countries, Continent = test_continents)

processed_data$continent <- countrycode(processed_data$country_name,
                                    origin = "country.name",
                                    destination = "continent")


# Check for countries that were not identified by the function
unmatched_countries <- processed_data |>
  mutate(continent_check = countrycode(country_name, "country.name", "continent")) |>
  filter(is.na(continent_check)) |>
  select(country_name)

print(unique(unmatched_countries$country_name))

# Manual handling for the case of Kosovo
processed_data$continent[processed_data$country_name == "Kosovo"] <- "Europe"

# Check if there are countries without a continent assigned
sum(is.na(processed_data$continent))  


### Save
write.csv(processed_data, "processed_happiness_data.csv", row.names = FALSE)






#### Clean Data---- 

data <- read.csv("C:/Users/danas/OneDrive/Desktop/פסיכולוגיה- תואר שני/קורס R/Final_Assignment/processed_happiness_data_2024.csv")

## Delete NA
clean_data <- processed_data |>
  drop_na()

nrow(processed_data) - nrow(clean_data)

### Create a function
# calculate the average happiness by country status
calculate_avg_happiness <- function(data) {
  data |> 
    group_by(developed) |> 
    summarise(avg_happiness = mean(happiness, na.rm = TRUE))
}

# Calculate averages using the function
calculate_avg_happiness(clean_data)
# developing=5.11, developed=6.79

clean_data <- clean_data |>
  select(corruption, freedom, developed, happy_country)


### Save
write.csv(clean_data, "clean_happiness_data.csv", row.names = FALSE)


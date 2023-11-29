# Install packages
install.packages("yardstick")

# Import required libraries
library(readr)
library(dplyr)
library(glue)
library(yardstick)
library(ggplot2)

cars <- read.csv("car_insurance.csv")
head(cars)
View(cars)

# View datatype
str(cars)

# Missing values in data
colSums(is.na(cars))
# NA values in credit_score and annual_mileage

# Distribution of credit_score
summary(cars$credit_score)

# Distribution of annual_mileage
summary(cars$annual_mileage)

# Fill NA values with the mean
 cars <- cars %>%
   mutate(credit_score = 
            ifelse(is.na(credit_score), mean(credit_score, na.rm = TRUE), credit_score) ,
          annual_mileage = 
            ifelse(is.na(annual_mileage), mean(annual_mileage, na.rm = TRUE), annual_mileage))

features <- data.frame(features = names(subset(cars, select = -c(id, outcome))))

str(features)

accuracy <- c()

for (col in features_df$features) {
  # Create a model
  model <- glm(glue('outcome ~ {col}'), data = cars, family = 'binomial')
  # Get prediction values for the model
  predictions <- round(fitted(model))
  # Calculate accuracy
  accuracy <- length(which(predictions == cars$outcome)) / length(cars$outcome)
  # Add accuracy to features_df
  features[which(features_df$feature == col), "accuracy"] = accuracy
}
best_feature <- features$features[which.max(features$accuracy)]
best_accuracy <- max(features$accuracy)
best_feature_df <- data.frame(best_feature, best_accuracy)
best_feature_df



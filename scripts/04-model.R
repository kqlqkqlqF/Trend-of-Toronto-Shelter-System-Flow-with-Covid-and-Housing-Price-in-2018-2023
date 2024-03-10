---
#### Preamble ####
# Purpose: Clean the data download from OpendataToronto to make the 
#          datas into a uniform and cleaned style
# Author: Yiyi Feng, Linda Sun
# Date: 10 March 2024
# Contact: yiyi.feng@mail.utoronto.ca, lindayx.sun@mail.utoronto.ca
# License: MIT
# Pre-requisites: "data/cleaned_data.csv" file and "data/cleaned_data_fig3.csv" file. 
#Other information: Need to install packages "knitr", "janitor", "tidyverse", 
#                   and "lubridate"
---

####Workspace setup ####
library(tidyverse)
library(janitor)
library(knitr)
library(ggplot2)
library(ggpubr)
library(kableExtra)

#### Read data ####
cleaned_data_figtwo <-
  read_csv(
    file = "data/cleaned_data_fig2.csv",
    show_col_types = FALSE)

####modeling data####

# Calculate the sum of age groups for each row
cleaned_data_figtwo$age_sum <- rowSums(cleaned_data_figtwo[, c("ageunder16", "age16_24", "age25_44", "age45_64", "age65over")])


# Remove unnecessary columns
cleaned_data_figtwo <- cleaned_data_figtwo %>%
  select(-c(date_mmm_yy, population_group))

# Create an empty list to store linear models
models <- list()

# Iterate over each column and fit a linear model
for (col_name in colnames(cleaned_data_figtwo)) {
  # Fit linear model
  lm_model <- lm(cleaned_data_figtwo[[col_name]] ~ 1)
  
  # Store the model in the list
  models[[col_name]] <- lm_model
}






generate_predictions <- function(model_info, date) {
  predictions <- sapply(model_info, function(model) {
    print(str(model))  # Print the structure of the model object
    pred <- predict(model$model, newdata = data.frame(date_mmm_yy = date))
    return(pred)
  })
  return(predictions)
}

# Generate predictions for each age group for each date
prediction_data <- list()
for (age_group in names(models)) {
  print(age_group)  # Print the age group for debugging
  print(models[[age_group]])  # Print the model object for debugging
  prediction_data[[age_group]] <- generate_predictions(models[[age_group]], prediction_dates)
}

# Melt the data frame for plotting
library(reshape2)
prediction_data_melted <- melt(prediction_data, id.vars = "date_mmm_yy", variable.name = "age_group", value.name = "predicted_count")

# Plot the predicted counts over time for each age group
library(ggplot2)
ggplot(prediction_data_melted, aes(x = date_mmm_yy, y = predicted_count, color = age_group)) +
  geom_line() +
  labs(x = "Date", y = "Predicted Count", color = "Age Group") +
  theme_minimal()


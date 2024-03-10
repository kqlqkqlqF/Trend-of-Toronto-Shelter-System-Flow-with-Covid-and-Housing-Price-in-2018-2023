---
#### Preamble ####
# Purpose: Clean the data download from OpendataToronto to make the 
#          datas into a uniform and cleaned style
# Author: Yiyi Feng, Linda Sun
# Date: 3 March 2024
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





# Extract predictions from the models
predictions <- lapply(models, function(model) predict(model, newdata = cleaned_data_figtwo))

# Plotting actual vs predicted values for each column
par(mfrow=c(3, 3))  # Set up a multi-panel plot
for (i in 1:length(predictions)) {
  plot(cleaned_data_figtwo[[i]], predictions[[i]], main = names(predictions)[i],
       xlab = "Actual", ylab = "Predicted")
  abline(0, 1, col = "red")  # Add a diagonal line for reference
}

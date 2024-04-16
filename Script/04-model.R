---
#### Preamble ####
# Purpose: build the linear model for the relationship between the month passes and the total number of homeless
# Author: Yiyi Feng, Yingxvan Sun
# Date: 14 March 2024
# Contact: yiyi.feng@mail.utoronto.ca, lindayx.sun@mail.utoronto.ca
# License: MIT
# Pre-requisites: "data/cleaned_data_fig2.csv" file. 
#Other information: Need to install packages "tidyverse" and "here".
---

#### Workspace setup ####
library(tidyverse)
library(here)

#### Read data ####
cleaned_data_figtwo <-
  read_csv(
    file = "data/cleaned_data_fig2.csv",
    show_col_types = FALSE)

# Only take the rows representing all population
cleaned_data_model <- cleaned_data_figtwo[cleaned_data_figtwo$population_group == "All Population", ]

# Convert date_mmm_yy to Date format
cleaned_data_model$date_mmm_yy <- as.Date(cleaned_data_model$date_mmm_yy)

# Find the earliest year and month
earliest_year <- min(format(cleaned_data_model$date_mmm_yy, "%Y"))
earliest_month <- min(format(cleaned_data_model$date_mmm_yy, "%m"))

# Calculate the number of months that have passed since the earliest date
cleaned_data_model$month_number <- (as.numeric(format(cleaned_data_model$date_mmm_yy, "%Y")) - as.numeric(earliest_year)) * 12 +
  (as.numeric(format(cleaned_data_model$date_mmm_yy, "%m")) - as.numeric(earliest_month)) + 1

#sum the number of homeless for different age groups
cleaned_data_model$total_population <- rowSums(cleaned_data_model[, c("ageunder16", "age16_24", "age25_44", "age45_64", "age65over")])

#### Save data ####
write_csv(
  x = cleaned_data_model,
  file = "data/cleaned_data_model.csv"
)

# Fit a linear model
lm_model <- lm(total_population ~ month_number, data = cleaned_data_model)

# Print the summary of the model
summary(lm_model)

#### Save model ####
# Save the model as an .rds file to the 'models' folder
saveRDS(
  lm_model,
  file = here("model", "linear_model_month.rds")
)

---
#### Preamble ####
# Purpose: build the linear model for the relationship between the month passes and the total number of homeless
# Author: Yiyi Feng
# Date: 16th April 2024
# Contact: yiyi.feng@mail.utoronto.ca
# License: MIT
# Pre-requisites: "data/cleaned_data_fig2.csv" file. 
#Other information: Need to install packages "tidyverse" and "here".
---

#### Workspace setup ####
library(tidyverse)
library(here)
library(tidyr)
library(tibble)
library(dplyr)

#### Read data ####
cleaned_data_figtwo <-
  read_csv(
    file = "data/cleaned_data/cleaned_data_fig2.csv",
    show_col_types = FALSE)

cleaned_data_covid <-
  read_csv(
    file = "data/cleaned_data/cleaned_data_covid.csv",
    show_col_types = FALSE)


# Only take the rows representing all population
cleaned_data_model <- cleaned_data_figtwo[cleaned_data_figtwo$population_group == "All Population", ]

# Convert date_mmm_yy to Date format
cleaned_data_model$date_mmm_yy <- as.Date(cleaned_data_model$date_mmm_yy)

# Find the earliest year and month
earliest_year <- min(format(cleaned_data_model$date_mmm_yy, "%Y"))
earliest_month <- min(format(cleaned_data_model$date_mmm_yy, "%m"))

#sum the number of homeless for different age groups
cleaned_data_model$total_population <- rowSums(cleaned_data_model[, c("ageunder16", "age16_24", "age25_44", "age45_64", "age65over")])


# Filter homeless data to include only the years 2020 to 2024
cleaned_data_model_filtered <- cleaned_data_model[cleaned_data_model$date_mmm_yy >= "2020/1/1" & cleaned_data_model$date_mmm_yy <= "2024/2/1", ]

# Reshape the COVID-19 data to long format
cleaned_data_covid_long <- pivot_longer(cleaned_data_covid, -Year, names_to = "Month", values_to = "Cases")

# Remove rows with NA values
cleaned_data_covid_long <- na.omit(cleaned_data_covid_long)

# Generate a sequence of dates from January 2020 to February 2024
dates <- seq(as.Date("2020-01-01"), as.Date("2024-02-01"), by = "month")

cleaned_data_covid_long$date_mmm_yy <- dates

# Add all columns from cleaned_data_model_filtered to cleaned_data_covid_long based on the Date_mm_dd column
cleaned_data_covid_long_merged <- merge(cleaned_data_covid_long, cleaned_data_model_filtered, by = "date_mmm_yy", all.x = TRUE)






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

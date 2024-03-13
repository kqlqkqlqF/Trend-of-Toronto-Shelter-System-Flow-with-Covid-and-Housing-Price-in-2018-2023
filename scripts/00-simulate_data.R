---
#### Preamble ####
# Purpose: Simulate and generate a dataset for shelter dynamic information in
#          Toronto at the year of 2018
# Date: 12 March 2024
# Contact:lindayx.sun@mail.utoronto.ca, yiyi.feng@mail.utoronto.ca
# Author: Yingxvan Sun, Yiyi Feng
# Pre-requisites: no pre-requisites 
# License: MIT
#Other information: Need to install packages"tidyverse", and "lubridate".
---

#### Workspace setup ####
# Load necessary libraries for data manipulation and date handling
library(tidyverse)
library(lubridate)

# Set a seed to ensure reproducibility when simulating data
set.seed(1)

# Simulate dates for the year 2018, with monthly intervals
dates <- seq(ymd("2018-01-01"), ymd("2018-12-01"), by = "month")

# Define population groups for simulated data
population_groups <- c("All Population", "Chronic", "Refugees", "Families", "Youth", 
                       "Single Adult", "Non-refugees", "Indigenous")

# Generate combinations of dates and population groups using map_df function
date_population_combinations <- map_df(dates, ~ tibble(
  date_mmm_yy = .x,
  population_group = sample(population_groups, size = length(population_groups), replace = FALSE)
))

# Simulate data for various parameters based on population groups and dates
simulated_data <- date_population_combinations %>%
  mutate(
    returned_from_housing = sample(0:500, n(), replace = TRUE),  # Simulate individuals returning from housing
    returned_to_shelter = sample(0:1000, n(), replace = TRUE),   # Simulate individuals returning to shelter
    newly_identified = sample(0:2000, n(), replace = TRUE),       # Simulate newly identified homeless individuals
    moved_to_housing = sample(0:1500, n(), replace = TRUE),       # Simulate individuals moving to housing
    became_inactive = sample(0:2000, n(), replace = TRUE),        # Simulate individuals becoming inactive
    ageunder16 = sample(0:5000, n(), replace = TRUE),             # Simulate age groups
    age16_24 = sample(0:5000, n(), replace = TRUE),
    age25_44 = sample(0:5000, n(), replace = TRUE),
    age45_64 = sample(0:5000, n(), replace = TRUE),
    age65over = sample(0:5000, n(), replace = TRUE),
    gender_male = sample(0:10000, n(), replace = TRUE),           # Simulate gender distribution
    gender_female = sample(0:10000, n(), replace = TRUE),
    gender_transgender_non_binary_or_two_spirit = sample(0:500, n(), replace = TRUE)
  ) %>%
  group_by(date_mmm_yy) %>%
  mutate(
    actively_homeless = sample(1:15000, n(), replace = TRUE)  # Simulate actively homeless individuals
  ) %>%
  ungroup() %>%
  arrange(date_mmm_yy, population_group) %>%
  mutate(id = row_number()) %>%
  select(id, everything())  # Ensure id is the first column

# Move actively_homeless column to the 9th position
simulated_data <- simulated_data %>%
  select(id, date_mmm_yy, population_group, everything(), actively_homeless) %>%
  relocate(actively_homeless, .after = became_inactive)

# Correct calculation for population_group_percentage
simulated_data <- simulated_data %>%
  mutate(
    population_group_percentage = if_else(
      population_group == "All Population",
      "100%",
      sprintf("%.2f%%", actively_homeless / max(actively_homeless) * 100)
    )
  )

# Write simulated data to a CSV file for further analysis
write_csv(simulated_data, "data/simulated_data.csv")



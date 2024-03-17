#### Preamble ####
# Purpose: Test the cleaned dataset and the simulated dataset to ensure the data were cleaned and there's no unexpected errors
# Date: 12 March 2024
# Contact:lindayx.sun@mail.utoronto.ca, yiyi.feng@mail.utoronto.ca
# Author: Yingxvan Sun, Yiyi Feng
# Pre-requisites: "data/cleaned_data.csv" file, "data/simulated_data.csv". 
# License: MIT
#Other information: Need to install packages "testthat", "lubridate", "tidyverse", "dplyr", and "stringr".

#### Workspace setup ####
library(tidyverse)
library(testthat)
library(lubridate)
library(stringr)


#### Test data ####
#Read the dataset
file_paths <- c("data/simulated_data.csv", "data/cleaned_data.csv")
results <- lapply(file_paths, function(file_path) {
  # Read the CSV file
  data <- read_csv(file_path, col_types = cols())
  
  # Check if the N/A data has been completely cleared from the table
  na_free <- all(!is.na(data))
  
  # Check if the first line is all strings
  first_row_is_string <- all(sapply(names(data), is.character))
  
  # Check if the second column is all dates and the dates are all on the 1st
  dates_valid_and_first_of_month <- all(!is.na(ymd(data[[2]][-1]))
                                        & day(ymd(data[[2]][-1])) == 1)
  
  # Check whether the third column belongs to one of the specified strings
  population_group_valid <- all(data[[3]][-1] %in% c("All Population", "Chronic", "Refugees", "Families", "Youth", "Single Adult", "Non-refugees", "Indigenous"))
  
  # Check whether the fourth to seventeenth columns are all integers greater 
  #than or equal to 0
  cols_4_to_17_are_integers <-
    all(sapply(data[4:17], 
               function(col) all(col[-1] >= 0 
                                 & col[-1] == floor(col[-1]))))
  
  # Check whether the eighteenth column is all percentages between 0 and 100
  percentages_valid <- all(sapply(data[[18]][-1], function(x) {
    percentage <- as.numeric(sub("%", "", x))
    return(!is.na(percentage) && percentage >= 0 && percentage <= 100)
  }))
  
  # Check for All Population rows to have 100% population_group_percentage
  data$population_group_percentage_numeric <-
    as.numeric(str_remove(data$population_group_percentage, "%"))
  all_population_percentage_check <- 
    all(data[data$population_group == "All Population", ]
        $population_group_percentage_numeric == 100)
  
  
  # Return result
  list(
    na_free = na_free,
    first_row_is_string = first_row_is_string,
    ddates_valid_and_first_of_month = dates_valid_and_first_of_month,
    population_group_valid = population_group_valid,
    cols_4_to_17_are_integers = cols_4_to_17_are_integers,
    percentages_valid = percentages_valid,
    all_population_percentage_check = all_population_percentage_check
  )
})

print(results)


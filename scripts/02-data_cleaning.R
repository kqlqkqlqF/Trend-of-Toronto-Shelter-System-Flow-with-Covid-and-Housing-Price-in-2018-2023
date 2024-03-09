---
#### Preamble ####
# Purpose: Clean the data download from OpendataToronto to make the 
#          datas into a uniform and cleaned style
# Author: Yiyi Feng
# Date: 3 March 2024
# Contact: yiyi.feng@mail.utoronto.ca, lindayx.sun@mail.utoronto.ca
# License: MIT
# Pre-requisites: "data/raw_data.csv" file, which is the raw data. 
#Other information: Need to install packages "knitr", "janitor", "tidyverse", 
#                   and "lubridate"
---

#### Workspace setup ####
library(knitr)
library(janitor)
library(lubridate)
library(tidyverse)

#### Basic cleaning ####
#import raw data
raw_data <-
  read_csv(
    file = "data/raw_data.csv",
    show_col_types = FALSE
  )

#switch setting of local system time for converting date in data cleaning, 
#please mute this sentence if not needed on your computer
loc <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C") 

#switch the character time to date and select information needed
cleaned_data <-
  clean_names(raw_data) |>
  mutate(date_mmm_yy = as.factor(date_mmm_yy)) |>
  mutate(date_mmm_yy = paste("01",date_mmm_yy,sep="-")) |>
  mutate(date_mmm_yy = strptime(date_mmm_yy,format="%d-%b-%y")) |>
  mutate(date_mmm_yy = as.Date(date_mmm_yy, format = "%Y-%m-%d"))
cleaned_data#take a look at the cleaned data for checking

#set local time back to beggining, please mute this sentence if not needed on your computer
Sys.setlocale("LC_TIME", loc) 


#### Save data ####
write_csv(
  x = cleaned_data,
  file = "data/cleaned_data.csv"
)

####summerize data for figure one####
figone_data <- 
  cleaned_data |>
  select(date_mmm_yy, population_group, returned_from_housing, returned_to_shelter, newly_identified, moved_to_housing, became_inactive, actively_homeless)


figone_data_clean <- subset(figone_data, figone_data$population_group == "All Population")
head(figone_data_clean)

figone_data_clean <- figone_data_clean %>% 
  select(-population_group)
head(figone_data_clean)

# Extract the year from the date_mmm_yy column
figone_data_clean$year <- format(figone_data_clean$date_mmm_yy, "%Y")

# Summarize the data by year
figone_data_clean <- figone_data_clean %>%
  group_by(year) %>%
  summarize(
    returned_from_housing = sum(returned_from_housing),
    returned_to_shelter = sum(returned_to_shelter),
    newly_identified = sum(newly_identified),
    moved_to_housing = sum(moved_to_housing),
    became_inactive = sum(became_inactive),
    actively_homeless = sum(actively_homeless)
  )

# Remove the last row
figone_data_clean <- figone_data_clean %>% 
  slice(-n())
figone_data_clean


#### Save data ####
write_csv(
  x = figone_data_clean,
  file = "data/cleaned_data_fig1.csv"
)


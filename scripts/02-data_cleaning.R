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




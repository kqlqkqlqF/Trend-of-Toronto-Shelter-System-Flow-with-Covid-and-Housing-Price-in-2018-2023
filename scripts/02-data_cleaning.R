---
#### Preamble ####
# Purpose: Clean the data up so that only the columes we need remained
# Author: Yiyi Feng
# Date: 20 January 2024
# Contact: yiyi.feng@mail.utoronto.ca
# License: MIT
# Pre-requisites: [inputs/data/raw_data.csv]
# Any other information needed? nope
---

```{r}
#### Workspace setup ####
library(knitr)
library(janitor)
library(lubridate)
library(tidyverse)


#### Basic cleaning ####
raw_data <-
  read_csv(
    file = "../inputs/data/raw_data.csv",
    show_col_types = FALSE
  )


```

```{r}
####!st round of data cleaning and data processing (for data section1)####

#switch setting of local system time for converting date in data cleaning, please mute this sentence if not needed on your computer
loc <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C") 

#switch the character time to date and select information needed
Fst_cleaned_data <-
  clean_names(raw_data) |>
  mutate(date_mmm_yy = as.factor(date_mmm_yy)) |>
  mutate(date_mmm_yy = paste("01",date_mmm_yy,sep="-")) |>
  mutate(date_mmm_yy = strptime(date_mmm_yy,format="%d-%b-%y")) |>
  mutate(date_mmm_yy = as.Date(date_mmm_yy, format = "%Y-%m-%d")) |>
  select(date_mmm_yy, ageunder16, age16_24, age25_44, age45_64, age65over)
Fst_cleaned_data#take a look at the cleaned data for checking

#set local time back to beggining, please mute this sentence if not needed on your computer
Sys.setlocale("LC_TIME", loc) 


#### Save data ####
write_csv(
  x = Fst_cleaned_data,
  file = "../outputs/data/analysis_data_1.csv"
)

#get another dataset with mean of people move to shelter
Snd_cleaned_data <-
  Fst_cleaned_data |>
  mutate(occupied_month = month(
    date_mmm_yy,
    label = TRUE,
    abbr = FALSE
  )) |>
  arrange(month(date_mmm_yy)) |> 
  summarise(num_under16 = mean(ageunder16), num_age16_24 = mean(age16_24), num_age25_44 = mean(age25_44), num_age45_64 = mean(age45_64), num_over65 = mean(age65over), 
            .by = occupied_month) |>
  select(occupied_month, num_under16, num_age16_24, num_age25_44, num_age45_64, num_over65)
Snd_cleaned_data#show the data for checking

#### Save data ####

write_csv(
  x = Snd_cleaned_data,
  file = "../outputs/data/analysis_data_2.csv"
)

```

```{r}
####2nd round of data cleaning and data processing (for data section 2)####

#switch setting of local system time for converting date in data cleaning, please mute this sentence if not needed on your computer
loc <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C") 

#get another dataset with mean of people move to shelter
Trd_cleaned_data <-
  clean_names(raw_data) |>
  mutate(date_mmm_yy = as.factor(date_mmm_yy)) |>
  mutate(date_mmm_yy = paste("01",date_mmm_yy,sep="-")) |>
  mutate(date_mmm_yy = strptime(date_mmm_yy,format="%d-%b-%y")) |>
  mutate(date_mmm_yy = as.Date(date_mmm_yy, format = "%Y-%m-%d")) |>
  select(date_mmm_yy, population_group, returned_from_housing, returned_to_shelter, moved_to_housing)
Trd_cleaned_data#return the data for checking

#### Save data ####

write_csv(
  x = Trd_cleaned_data,
  file = "../outputs/data/analysis_data_3.csv"
)

Fth_cleaned_data <-
  Trd_cleaned_data |>
  mutate(occupied_year = year(
    date_mmm_yy)) |>
  arrange(month(date_mmm_yy)) |> 
  summarise(num_returned_from_housing = mean(returned_from_housing), num_returned_to_shelter = mean(returned_to_shelter), num_moved_to_housing = mean(moved_to_housing), 
            .by = occupied_year) |>
  select(occupied_year, num_returned_from_housing, num_returned_to_shelter, num_moved_to_housing)
Fth_cleaned_data#show the data for checking

#set local time back to beggining, please mute this sentence if not needed on your computer
Sys.setlocale("LC_TIME", loc) 

#### Save data ####

write_csv(
  x = Fth_cleaned_data,
  file = "../outputs/data/analysis_data_4.csv"
)

```

```{r}
####3rd round of data cleaning (for data section 3)####
Fth_cleaned_data <-
  clean_names(raw_data) |>
  mutate(date_mmm_yy = as.factor(date_mmm_yy)) |>
  mutate(date_mmm_yy = paste("01",date_mmm_yy,sep="-")) |>
  mutate(date_mmm_yy = strptime(date_mmm_yy,format="%d-%b-%y")) |>
  mutate(date_mmm_yy = as.Date(date_mmm_yy, format = "%Y-%m-%d"))
Fth_cleaned_data#return the data for checking

#### Save data ####
write_csv(
  x = Fth_cleaned_data,
  file = "../outputs/data/analysis_data_5.csv"
)
```



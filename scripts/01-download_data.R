---
#### Preamble ####
# Purpose: Download and save the shelter flow data from Toronto Opendata
# Author: Yiyi Feng, Yingxvan Sun
# Date: 10 March 2024
# Contact: yiyi.feng@mail.utoronto.ca, lindayx.sun@mail.utoronto.ca
# License: MIT
# Pre-requisites: none. 
#Other information: Need to install packages "opendatatoronto", "readr", and "dplyr"
---

#### Workspace setup ####
library(opendatatoronto)
library(dplyr)
library(readr)

#### Download data ####

# get package from Toronto Opendata
package <- show_package("ac77f532-f18b-427c-905c-4ae87ce69c93")
head(package)

# get all resources for this package
resources <- list_package_resources("ac77f532-f18b-427c-905c-4ae87ce69c93")
head(resources)#show the first six lines of package for checking

# load the first datastore resource as a sample
the_raw_data <- filter(resources, row_number()==1) %>% get_resource()
the_raw_data#show the whole raw data for checking


#### Save data ####
write_csv(
  x = the_raw_data,
  file = "data/raw_data.csv"
)

         

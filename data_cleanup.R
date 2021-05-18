library(data.table)
library(dplyr)

#Find valid users ----
data <- fread("Litterati-Partners.csv")

data_clean <- data %>%
    mutate(timestamp = as.POSIXct(gsub(" GMT", "", Date), format = "%d/%m/%Y %H:%M:%OS")) %>%
    mutate(lat = as.numeric(gsub("/.{1,}", "", `Location (Lat / Long)`))) %>%
    mutate(lon = as.numeric(gsub(".{1,}/", "", `Location (Lat / Long)`))) %>%
    mutate(week = strftime(timestamp, format = "%W")) %>%
    mutate(day = strftime(timestamp, format = "%D"))
    
## >= 1 survey per week
data_clean_week <- data_clean %>%
    distinct(Username, week) %>%
    group_by(Username) %>%
    summarise(count = n()) %>%
    filter(count > 3) %>%
    select(-count) %>%
    inner_join(data_clean)

## Reported Cleaned Up Data


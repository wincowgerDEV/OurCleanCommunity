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
data_reconciled <- fread("StudyAreas/User_Cleaned_Data/Total-Merge-Data-reconciled.csv")
brands <- fread("StudyAreas/User_Cleaned_Data/TrashTaxonomy/Brand_Manufacturer_Relation.csv")
site_data <- fread("StudyAreas/Demographic_Site_Data/Demographic_Data.csv")

data_reconciled_cleaned <- data_reconciled %>%
    left_join(brands %>% dplyr::select(-Brand), by = "ID") %>%
    mutate_all(na_if, "") %>%
    mutate_all(na_if, " ") %>%
    filter(!id %in% c(data_reconciled %>%
               filter(Name == "Hannah Hapich" & Day %in% c("10/24/2018", "10/25/2018")) %>%
               pull(id))) %>%
    filter(!id %in% c(data_reconciled %>%
                          filter(Name == "Win Cowger" & Day %in% c("11/27/2018")) %>%
                          pull(id))) %>%
    mutate(Manufacturer = ifelse(is.na(Manufacturer), "other", Manufacturer)) %>%
    left_join(site_data) %>%
    mutate(Name = case_when(
        Name == "Hannah Hapich" ~ "Site 1",
        Name == "Hina Nogi" ~ "Site 2",
        Name == "Jacqueline Santiago" ~ "Site 3",
        Name == "LokTrevor" ~ "Site 4",
        Name == "Melissa" ~ "Site 5",
        Name == "Stanley" ~ "Site 6",
        Name == "Win Cowger" & Day %in% c("10/1/2018",  "10/3/2018",  "10/6/2018",  "10/8/2018",  "10/11/2018", "10/12/2018", "9/16/2018",  "9/18/2018",  "9/21/2018",  "9/25/2018", "9/26/2018",  "11/27/2018", "9/28/2018") ~ "Site 7A",
        Name == "Win Cowger" & Day %in% c( "4/2/2020", "4/5/2020", "3/23/2020",  "3/26/2020",  "3/30/2020",  "4/3/2020") ~ "Site 7B",
    ))

fwrite(data_reconciled_cleaned, "StudyAreas/User_Cleaned_Data/reconciled_cleaned.csv")

data_site_weekend <- data_reconciled_cleaned %>%
    distinct(Name, Day, `Street Sweeping Schedule`)

fwrite(data_site_weekend, "StudyAreas/User_Cleaned_Data/weekend_sweep.csv")

data_reconciled %>%
    filter(Name == "Win Cowger") %>%
    pull(Day) %>%
    unique()



data_reconciled_cleaned %>%
    pull(Name) %>%
    unique()



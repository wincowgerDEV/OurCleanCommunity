library(dplyr)
library(data.table)

setwd("G:/My Drive/GrayLab/Projects/Plastics/ActiveProjects/TrackingTrashReceipts/Code/Github/StudyAreas/User_Cleaned_Data/edited_files")

files_list <- list.files(pattern = ".csv")

files <- lapply(files_list, fread)

merged <- rbindlist(files, fill = T)

full_litterati <- fread("G:/My Drive/GrayLab/Projects/Plastics/ActiveProjects/TrackingTrashReceipts/Code/Github/Litterati-Partners.csv")

people <- c("Win Cowger", 
            "Hina Nogi", 
            "Melissa", 
            "Stanley", 
            "Jacqueline Santiago", 
            "Hannah Hapich",
            "LokTrevor") 

full_litterati_merged <- full_litterati %>%
    filter(Name %in% people) %>%
    rename(id = Litter) %>%
    mutate(id = as.character(id)) %>%
    right_join(merged)

unique(full_litterati_merged$Brand)

write.csv(full_litterati_merged,"full_litterati_merged.csv")

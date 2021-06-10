#RCLEANCOMMUNITY Quality Check

setwd("G:/My Drive/GrayLab/Plastics/Articles Publish/Active/RCleanCommunity")

library(dplyr)

OurClean <- read.csv("Our Clean Community2.csv")

DuplicateTimestamps <- OurClean %>%
  group_by(litterTimestamp, litterDatestamp) %>%
  summarise(n = n()) %>%
  filter(n > 1)%>%
  arrange(desc(n))

DuplicateLatLong <- OurClean %>%
  group_by(lat, lon) %>%
  summarise(n = n()) %>%
  filter(n > 1)%>%
  arrange(desc(n))

DuplicateLatLongTimePerson <- OurClean %>%
  group_by(lat, lon, litterTimestamp, litterDatestamp, username) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  arrange(desc(n))

write.csv(DuplicateTimestamps, "DupTime.csv")
write.csv(DuplicateLatLong, "DupLatLon.csv")
write.csv(DuplicateLatLongTimePerson, "DupLatLonTimePerson.csv")

setwd("G:/My Drive/GrayLab/Projects/Plastics/ActiveProjects/RCleanCommunity/Data/Processed Data")
library(data.table)
library(dplyr)
library(RCurl)
library(tmap)
library(ggplot2)
library(tidyr)
library(scales)
library(viridis)
library(readxl)

OurCleanCommunity <- fread("Our Clean Community.csv")

LitteratiTrash <- fread("MyData2.csv")
Cleaned2018Data <- read_xlsx("CleanedData2018.xlsx")

CleanedData <- fread("StudyAreas/User_Cleaned_Data/merged_site_data.csv")


#Download Litterati Photos
Folder <- "Photos/"
for(photo in 6300:nrow(OurCleanCommunity)){
  download.file(url = paste(OurCleanCommunity[photo,"url"], sep = ""), destfile = paste(Folder, OurCleanCommunity[photo,"litterId"], ".jpg", sep = ""), mode = "wb", quiet = T)
  Sys.sleep(0.5)
}


#Download and name Photos
Folder <- "StudyAreas/User_Cleaned_Data/Photos/"
for(photo in 1:nrow(CleanedData)){
  download.file(url = paste(CleanedData[photo,"Image URL"], sep = ""), destfile = paste(Folder, CleanedData[photo,"id"], CleanedData[photo,"Material"], CleanedData[photo,"Item"], CleanedData[photo,"Brand"], ".jpg", sep = "_"), mode = "wb", quiet = T)
  Sys.sleep(0.5)
}


#download.file(url = "https://litterati-images-prod.s3-us-west-2.amazonaws.com/2018/09/28/cfa2c012-7756-3afe-9835-31047ebc71e0-92684.jpg", destfile = paste(Folder, photo, ".jpg", sep = ""), mode = "wb" )



###Make file for Lets Do It World

LDIW <- LitteratiTrash %>%
  mutate_all(funs(toupper)) %>%
  mutate_all(funs(gsub(" ", "", ., fixed = TRUE))) %>%
  filter(Materials %in% c("PLASTIC", "PAPER", "GLASS", "METAL")) %>%
  filter(Brands == "") 

table(LitteratiTrash$Materials)

write.csv(LDIW, "LDIW.csv")

#Make Maps
tmap_mode('view')
# Example data("NLD_prov")
# Example tm_shape(NLD_prov) + tm_polygons('population') + tm_layout(basemaps = c('OpenStreetMap')) # Or basemap = 'https://mt1.google.com/vt/lyrs=m&x={x}&y={y}&z={z}'

pts <- cbind(LitteratiTrash$f)
tm_shape(Litterati) + tm_layout(basemaps = c('OpenStreetMap'))




#Analyze Litter Types
LitterTypes <- unlist(strsplit(LitteratiTrash$tags, ","))
LitterTypes <- as.data.frame(LitterTypes)
LitterTypes$Types <- LitterTypes$LitterTypes
LitterMaterial <- LitterTypes %>%
  group_by(LitterTypes) %>%
  summarise(n = n()) %>%
  #ungroup()%>%
  filter(LitterTypes == "plastic"| LitterTypes == "paper"| LitterTypes == "metal"| LitterTypes == "glass"| LitterTypes == "food") %>%
  mutate(n = n/sum(n))
  #ungroup()%>%
  #group_by(n) %>%
  #top_n(7,n)
LitterItem <- LitterTypes %>%
  group_by(LitterTypes) %>%
  summarise(n= n()) %>%
  filter(LitterTypes == "cigarette"| LitterTypes == "wrapper"| LitterTypes == "napkin"| LitterTypes == "cup"| LitterTypes == "receipt") %>%
  mutate(n = n/sum(n))
#ungroup()%>%

LitterBrand <- LitterTypes %>%
  group_by(LitterTypes) %>%
  summarise(n= n()) %>%
  filter(LitterTypes == "marlboro"| LitterTypes == "jackinthebox"| LitterTypes == "camel"| LitterTypes == "starbucks"| LitterTypes == "mcdonalds") %>%
  mutate(n = n/sum(n))

#ungroup()%>%
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
LitterTypes$n <- LitterTypes$n/sum(LitterTypes$n)

ggplot(LitterTypes, aes(x="", y=n, fill=as.factor(as.integer(n*100))))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_viridis_d(direction = -1) +  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = n/7 + c(0, cumsum(n)[-length(n)])), label = (LitterTypes$LitterTypes), size=5)

ggplot(LitterMaterial, aes(x="", y=n, fill=as.factor(as.integer(n*100))))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_viridis_d(direction = -1) +  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = n/5 + c(0, cumsum(n)[-length(n)])), label = (LitterMaterial$LitterTypes), size=5)

ggplot(LitterBrand, aes(x="", y=n, fill=as.factor(as.integer(n*100))))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_viridis_d(direction = -1) +  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = n/5 + c(0, cumsum(n)[-length(n)])), label = (LitterBrand$LitterTypes), size=5)

ggplot(LitterItem, aes(x="", y=n, fill=as.factor(as.integer(n*100))))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_viridis_d(direction = -1) +  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = n/5 + c(0, cumsum(n)[-length(n)])), label = (LitterItem$LitterTypes), size=5)


#Get Weather Data
#Scraped from Wunderground
Weather <- read.csv("G:/My Drive/GrayLab/Plastics/Articles Publish/RCleanCommunity/WeatherWunderground.csv")
Weather$Year <- 2018
Weather$Date <- as.Date(paste(Weather$Year, "-", Weather$Month, "-", Weather$Day, sep= ""), "%Y-%m-%d")
Weather$Temperature <- Weather$Temp/max(Weather$Temp)
Weather$Precipatation <- Weather$Precipitation..in./max(Weather$Precipitation..in.)
Weather$`Wind Speed` <- Weather$Max.Wind..mph./max(Weather$Max.Wind..mph.)
Weather <- Weather %>%
  select(Date, Temperature, Precipatation, `Wind Speed`) %>%
  gather(Event, Intensity, -Date )

ggplot(Weather) + geom_line(aes(x=Date, y=Intensity, color = Event), size = 1.5) + theme_gray() + ylab("Maximum Normalized Intensity") + xlab("Date")

#Litter Generation Rate

runningdifferencedays <- function(x) {
  for(value in 1:length(x)) {
      x[value] - x[value -1]
    }
}

for(value in 1:length(Win$Date)) {
  if(value == 1) {
    print(0)
  }
  else{
   print(as.numeric(Win$Date)[value] - as.numeric(Win$Date)[value -1]) 
  }
}

LitteratiTrash <- fread("MyData2.csv")

Win <- LitteratiTrash %>%
  #filter(user_id == 92684) %>%
  mutate(Date = substr(photo_timestamp,1,nchar(photo_timestamp)-3)) %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d %H:%M:%S" )) %>%
  group_by(Date) %>%
  summarise(Intensity = n()) %>%
  ungroup() %>%
  arrange(Date) %>%
  mutate(DateDiff = as.numeric(Date) - lag(as.numeric(Date))) %>%
  mutate(generationrate = Intensity/DateDiff)


ggplot(Win, aes(x=Date)) + geom_histogram(binwidth = 3, color = "black") + theme_gray() + ylab("Litter Count")

ggplot(Win, aes(x = Date, y = generationrate)) + geom_line() + theme_classic() + ylab("Litter Generation Rate (Count/Day)")

#Combined Plots
CombinedWetherTrash <- rbind(Weather, Win)
ggplot(CombinedWetherTrash) + geom_line(aes(x=Date, y=Intensity, color = Event), size = 1.5) + theme_gray() + scale_color_viridis_d()+ylab("Intensity") + xlab("Date")

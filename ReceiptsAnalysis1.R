

#Check out this wind data https://mrcc.illinois.edu/CLIMATE/Hourly/WindRose.jsp

setwd("G:/My Drive/GrayLab/Projects/Plastics/ActiveProjects/TrackingTrashReceipts/Code/Jackie")

library(readxl)
library(dplyr)
library(ggplot2)
library(ggmap)
library(cowplot)
library(readr)
library(lavaan)
library(smooth)
library(Hmisc)
library(googleway)
library(ggforce)
library(stormwindmodel)

#Function returns the difference between two bearings
angle_diff <- function(theta1, theta2){
  theta <- abs(theta1 - theta2) %% 360 
  return(ifelse(theta > 180, 360 - theta, theta))
}

angle_diff(18, 360)

Weather <- read.csv("G:/My Drive/GrayLab/Projects/Plastics/ActiveProjects/TrackingTrashReceipts/Data/Processed Data/RiversideMuniAirport_Cleaned.csv") %>%
           na_if("M") %>%
           mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
           mutate(across(where(is.character), as.numeric))
  
Weather[Weather == "M"] <- NA

Data2018 <- read.csv("Data_Receipts2018.csv")
summary(Data2018)
str(Data2018)

AllReceiptsData <- read.csv('All_Receipts_Draft.csv')

Data2018Complete <- left_join(Data2018, AllReceiptsData, by = c("ï..LitterID" = "LitterID")) #Data from only 2018

Data2018Cleaned <- Data2018Complete %>% #New table to generate coordinates of receipt place locations
  mutate(dateprintedcleaned = as.character(Date.Printed)) %>%
  mutate(timeprintedcleaned = as.character(Time.Printed)) %>%
  mutate(dateprintedcleaned = ifelse(dateprintedcleaned == "Unknown", NA, dateprintedcleaned)) %>%
  mutate(dateprintedcleaned = as.Date(dateprintedcleaned, format = c("%m/%d/%Y"))) %>%
  mutate(TimestampDatecleaned = as.character(Timestamp.Date)) %>%
  mutate(TimestampDatecleaned = as.Date(TimestampDatecleaned, format = c("%m/%d/%Y"))) %>%
  mutate(Timedifference = as.numeric(TimestampDatecleaned - dateprintedcleaned)) %>%
  mutate(Locationaddresscleaned = as.character(Location.address)) %>%
  mutate(Locationnamecleaned = as.character(Location.name))%>%
  mutate(Locationnamecleaned = ifelse(Locationnamecleaned == "Unknown", NA, Locationnamecleaned)) %>%
  mutate(paymentmethodcleaned = as.character(Payment.Method)) %>%
  mutate(Value = as.numeric(as.character(Total.Value..US.dollars.))) %>%
  mutate(litterID = as.character(ï..LitterID)) %>%
  select(litterID, username, user_id, dateprintedcleaned, TimestampDatecleaned, Timedifference, Locationaddresscleaned, Non.Specific.Location.Address, Manual.Lat, Manual.Lon, Locationnamecleaned, Itemized, paymentmethodcleaned, Value, lat, lon)
  #mutate_geocode(Locationaddresscleaned) %>%
  #mutate(lat1 = ifelse(lat1 == 35.2205194, NA, lat1)) %>%
  #mutate(lon1 = ifelse(lon1 == -80.8575444, NA, lon1)) %>%
  #mutate(CombinedLat = ifelse(Locationaddresscleaned == "Unknown", Manual.Lat, lat1)) %>%
  #mutate(CombinedLon = ifelse(Locationaddresscleaned == "Unknown" , Manual.Lon, lon1))

#Data2018LocLatLon <- data.frame(Data2018LocLatLon = Data2018Cleaned$CombinedLat, Data2018Cleaned$CombinedLon, Data2018Cleaned$litterID)
#write.csv(Data2018LocLatLon, "Data2018LocLatLon.csv") #New file for generated coordinates, to avoid re-running geocode function

  
Data20182019 <- read.csv("All_Receipts.csv")
summary(Data20182019)
str(Data20182019)

Data20182019cleaned <- Data20182019 %>% #Data from both 2018 & 2019
  mutate(dateprintedcleaned = as.character(Date.Printed)) %>%
  mutate(timeprintedcleaned1 = as.character(Time.Printed)) %>%
  mutate(dateprintedcleaned = ifelse(dateprintedcleaned == "Unknown", NA, dateprintedcleaned)) %>%
  mutate(dateprintedcleaned = as.Date(dateprintedcleaned, format = c("%m/%d/%Y"))) %>%
  mutate(TimestampDatecleaned = as.character(Timestamp.Date)) %>%
  mutate(TimestampDatecleaned = as.Date(TimestampDatecleaned, format = c("%m/%d/%Y"))) %>%
  mutate(Timedifference = as.numeric(TimestampDatecleaned - dateprintedcleaned)) %>%
  mutate(Locationaddresscleaned = as.character((Location.address))) %>%
  mutate(Locationnamecleaned = as.character(Location.name)) %>%
  mutate(Locationnamecleaned = ifelse(Locationnamecleaned == "Unknown", NA, Locationnamecleaned)) %>%
  mutate(paymentmethodcleaned = as.character(Payment.Method)) %>%
  mutate(Value = as.numeric(as.character(Total.Value..US.dollars.))) %>%
  mutate(litterID = as.character(LitterID)) %>%
  select(litterID, username, user_id, dateprintedcleaned, TimestampDatecleaned, Timedifference, Locationaddresscleaned, Non.Specific.Location.Address, Manual.Lat, Manual.Lon, Locationnamecleaned, Itemized, paymentmethodcleaned, Value, lat, lon)
  #mutate_geocode(Locationaddresscleaned) %>%
  #mutate(lat1 = ifelse(lat1 == 35.2205194, NA, lat1)) %>%
  #mutate(lon1 = ifelse(lon1 == -80.8575444, NA, lon1)) %>%
  #mutate(CombinedLat = ifelse(Locationaddresscleaned == "Unknown", Manual.Lat, lat1)) %>%
  #mutate(CombinedLon = ifelse(Locationaddresscleaned == "Unknown" , Manual.Lon, lon1))

#Data20182019LocLatLon <- data.frame(Data20182019LocLatLon = Data20182019cleaned$CombinedLat, Data20182019cleaned$CombinedLon, Data20182019cleaned$litterID)
#write.csv(Data20182019LocLatLon, "Data20182019LocLatLon.csv") #New file for generated coordinates, to avoid re-running geocode function


JacquelinesSiteData <- read.csv("JacquelinesSiteReceiptData.csv")

JacquelinesSiteDataCleaned <- JacquelinesSiteData %>% #2019 data not included in 2019 dataset
  mutate(dateprintedcleaned = as.character(Date.Printed)) %>%
  mutate(dateprintedcleaned = ifelse(dateprintedcleaned == "Unknown", NA, dateprintedcleaned)) %>%
  mutate(dateprintedcleaned = as.Date(dateprintedcleaned, format = c("%m/%d/%Y"))) %>%
  mutate(TimestampDatecleaned = as.character(Timestamp.Date)) %>%
  mutate(TimestampDatecleaned = as.Date(TimestampDatecleaned, format = c("%m/%d/%Y"))) %>%
  mutate(Timedifference = as.numeric(TimestampDatecleaned - dateprintedcleaned)) %>%
  mutate(Locationaddresscleaned = as.character(Location.address)) %>%
  mutate(Locationnamecleaned = as.character(Location.name)) %>%
  mutate(Locationnamecleaned = ifelse(Locationnamecleaned == "Unknown", NA, Locationnamecleaned)) %>%
  mutate(paymentmethodcleaned = as.character(Payment.Method)) %>%
  mutate(Value = as.numeric(as.character(Total.Value..US.dollars.))) %>%
  mutate(litterID = as.character(litterId)) %>%
  select(litterID, username, user_id, dateprintedcleaned, TimestampDatecleaned, Timedifference, Locationaddresscleaned, Non.Specific.Location.Address, Manual.Lat, Manual.Lon, Locationnamecleaned, Itemized, paymentmethodcleaned, Value, lat, lon)
  #mutate_geocode(Locationaddresscleaned) %>%
  #mutate(lat1 = ifelse(lat1 == 35.2205194, NA, lat1)) %>%
  #mutate(lon1 = ifelse(lon1 == -80.8575444, NA, lon1))

#JacquelinesSiteDataLocLatLon <- data.frame(JacquelinesSiteDataLocLatLon = JacquelinesSiteDataCleaned$lat1, JacquelinesSiteDataCleaned$lon1, JacquelinesSiteDataCleaned$litterID)
#write.csv(JacquelinesSiteDataLocLatLon, "JacquelinesSiteDataLocLatLon.csv") #New file for generated coordinates, to avoid re-running geocode function


CombinedData <- rbind(Data2018Cleaned, Data20182019cleaned, JacquelinesSiteDataCleaned) #all 3 data sets combined into 1

#Files of generated coordinates
Data2018LocLatLon <- read.csv("Data2018LocLatLon.csv")
Data20182019LocLatLon <- read.csv("Data20182019LocLatLon.csv")
JacquelinesSiteDataLocLatLon <- read.csv("JacquelinesSiteDataLocLatLon.csv")

CombinedLocLatLon <- rbind(Data2018LocLatLon, Data20182019LocLatLon, JacquelinesSiteDataLocLatLon)

CombinedDataLatLon<-cbind(CombinedData, CombinedLocLatLon) #Generated coordinates combined with the other attributes

#To find the distance between the location where the receipt was found and the location of the place it came from
ReceiptsData <- CombinedDataLatLon %>%
  mutate(latrad = (lat*pi)/180) %>%
  mutate(lonrad = (lon*pi)/180) %>%
  mutate(LocationLatrad = (LocLat*pi)/180) %>%
  mutate(LocationLonrad = (LocLon*pi)/180)

R <- 6371000 #meters
timestamplat <- ReceiptsData$latrad
locaddresslat <- ReceiptsData$LocationLatrad
timestamplon <- ReceiptsData$lonrad
locaddresslon <- ReceiptsData$LocationLonrad
changeinlat <- timestamplat - locaddresslat
changeinlon <- timestamplon - locaddresslon
a <- (sin(changeinlat/2) * sin(changeinlat/2)) + (cos(locaddresslat) * cos(timestamplat) * sin(changeinlon/2) * sin(changeinlon/2))
c <- 2 * atan2(sqrt(a), sqrt(1-a))
DistanceFromLocation <- R * c

ReceiptsDataandDistance <- cbind(ReceiptsData, DistanceFromLocation) #Calculated distance combined with rest of attributes

LocationNames <- read.csv("LocationNames.csv") #Location type categorization of places
CompleteData<-left_join(ReceiptsDataandDistance, LocationNames) #Final version: receipt dataset with all attributes

for (row in 1:nrow(CompleteData)) { #for loop output was made into a csv file
  combo1vec<-cbind(as.vector(as.numeric(CompleteData$lat[row])), as.vector(as.numeric(CompleteData$lon[row])))
  LocNames1<-CompleteData$Locationnamecleaned[row]
  if(is.na(LocNames1)) next
  Search1<-google_find_place(input = LocNames1, fields = c("name", "formatted_address", "geometry/location"), point = combo1vec)
  Search1df<-data.frame(Search1)
  Refined1<-slice(.data = Search1df, ... = 1, .preserve = FALSE)
  CompleteData[row, "googleformattedaddress"] <- Refined1$candidates.formatted_address
  CompleteData[row, "googleformattedlat"] <- Refined1$candidates.geometry$location$lat
  CompleteData[row, "googleformattedlon"] <- Refined1$candidates.geometry$location$lng
  CompleteData[row, "googleformattedname"] <- Refined1$candidates.name
  print(row)
}


CompleteDataWithGoogle <- CompleteData %>%
  mutate(googleLatrad = (googleformattedlat*pi)/180) %>%
  mutate(googleLonrad = (googleformattedlon*pi)/180) %>%
  mutate(changelat = latrad - googleLatrad) %>%
  mutate(changelon = lonrad - googleLonrad) %>%
  mutate(a = (sin(changelat/2) * sin(changelat/2)) + (cos(googleLatrad) * cos(latrad) * sin(changelon/2) * sin(changelon/2))) %>%
  mutate(c = 2 * atan2(sqrt(a), sqrt(1-a))) %>%
  mutate(googledistancemeters = R * c)



#Start HERE ----
CompleteDataWithGoogle <- read.csv("CompleteDataWithGoogle.csv") %>%
  mutate(startdate = as.Date(dateprintedcleaned, "%Y-%m-%d"), enddate =  as.Date(TimestampDatecleaned, "%Y-%m-%d"))


#This isn't working ----

library(swfscMisc)
#getAnywhere(bearing)[2]

for(row in 1:nrow(CompleteDataWithGoogle)){ #This is the inverse of what we would think because wind direction is the inverse. This will tell us what direction the trash came from.
  CompleteDataWithGoogle[row, "bearing"] <-bearing(lat2 = CompleteDataWithGoogle[row,"LocLat"]*pi/180, 
                                                   lon2 = CompleteDataWithGoogle[row,"LocLon"]*pi/180,
                                                   lat1 = CompleteDataWithGoogle[row,"lat"]*pi/180, 
                                                   lon1 = CompleteDataWithGoogle[row,"lon"]*pi/180)[1]
}

library(circular)
x.circ <- circular(CompleteDataWithGoogle$bearing, type="directions", units="degrees", template="geographics", rotation="clock")
rose.diag(x.circ, bins=10, col="gray", border=NA)


CompleteDataWithGoogle %>%
  filter(!is.na(Timedifference) & !is.na(bearing)) %>%
  as_tibble()

ggplot(data = CompleteDataWithGoogle) + stat_ecdf(aes(x = googledistancemeters)) + stat_ecdf(aes(x = DistanceFromLocation), color = "red") + scale_x_log10()

startdate <- CompleteDataWithGoogle[1, "dateprintedcleaned"]
enddate <- CompleteDataWithGoogle[1, "TimestampDatecleaned"]
bearing <- CompleteDataWithGoogle[1, "bearing"] %>% pull()

#Need to convert bearing to azimuth
startdate = CompleteDataWithGoogle[1,"startdate"] 
enddate = CompleteDataWithGoogle[1,"enddate"]
differenceofmean <- function(startdate, enddate){
  Weather %>%
    dplyr::filter(Date >= as.Date(startdate, "%Y-%m-%d") & Date <= as.Date(enddate, "%Y-%m-%d")) %>%
    select(Mean.Wind.Dir..deg.) %>%
    pull() %>%
    circular(type="directions", units="degrees", template="geographics", rotation="clock") %>%
    mean.circular(na.rm = T) %>%
    ifelse(.<0, .+360, .)
}

for(row in 1:nrow(CompleteDataWithGoogle)){
  CompleteDataWithGoogle[row, "meanbearing"] <- differenceofmean(startdate = CompleteDataWithGoogle[row,"startdate"], enddate = CompleteDataWithGoogle[row,"enddate"])
}

CompleteDataWithGoogle$differencebearings <- angle_diff(CompleteDataWithGoogle$bearing, CompleteDataWithGoogle$meanbearing)

cor.circular(CompleteDataWithGoogle$bearing, CompleteDataWithGoogle$meanbearing, test = T)

rainpresabs <- function(startdate, enddate){
  any(Weather %>%
        dplyr::filter(Date >= as.Date(startdate, "%Y-%m-%d") & Date <= as.Date(enddate, "%Y-%m-%d")) %>%
        select(Total.Precip..in.) %>%
        pull() > 0, na.rm = T)
}

for(row in 1:nrow(CompleteDataWithGoogle)){
  CompleteDataWithGoogle[row, "precip"] <-rainpresabs(startdate = CompleteDataWithGoogle[row,"dateprintedcleaned"], enddate = CompleteDataWithGoogle[row,"TimestampDatecleaned"])
}


x.circ <- circular(CompleteDataWithGoogle$bearing, type="directions", units="degrees", template="geographics", rotation="clock")
rose.diag(x.circ, bins=10, col="gray", border=NA)

y.circ <- circular(CompleteDataWithGoogle$meanbearing, type="directions", units="degrees", template="geographics", rotation="clock")
rose.diag(y.circ, bins=10, col="gray", border=NA)

cor.circular(x.circ, y.circ, test = T)
0

0.5*cos(rad(90))
0.5*sin(rad(90))

CompleteDataWithGoogle$ycoordbearing <- 0.5*sin(rad(CompleteDataWithGoogle$bearing))
CompleteDataWithGoogle$xcoordbearing <- 0.5*cos(rad(CompleteDataWithGoogle$bearing))
CompleteDataWithGoogle$ycoordmeanbearing <- 1*sin(rad(CompleteDataWithGoogle$meanbearing))
CompleteDataWithGoogle$xcoordmeanbearing <- 1*cos(rad(CompleteDataWithGoogle$meanbearing))
CompleteDataWithGoogle$row <- 1:nrow(CompleteDataWithGoogle)

CompleteDataCoordPlot <- CompleteDataWithGoogle %>%
  dplyr::select(xcoordmeanbearing, ycoordmeanbearing, row) %>%
  rename(x = xcoordmeanbearing, y = ycoordmeanbearing) %>%
  bind_rows(CompleteDataWithGoogle %>% 
              dplyr::select(ycoordbearing, xcoordbearing, row) %>%
              rename(x = ycoordbearing, y = xcoordbearing))
  

#360 (North) is on the right side.
ggplot() + 
  geom_point(data = CompleteDataCoordPlot, aes(x = x, y = y), alpha = 0.5, size = 2)+ 
  geom_line(data = CompleteDataCoordPlot, aes(x = x, y = y, group = row)) +
  geom_circle(aes(x0 = c(0,0), y0 = c(0,0), r = c(0.5,1))) + 
  theme_void() + 
  theme(aspect.ratio = 1)


plot(x.circ, stack = T)
plotCircular(area1 = CompleteDataWithGoogle$bearing, area2 = CompleteDataWithGoogle$meanbearing, spokes = )

ggplot(data = CompleteDataWithGoogle) +
  #geom_circle(aes(x))
  geom_point(aes(x = bearing)) +
  coord_polar()# +
  #scale_x_continuous(limits = c(0,360))


plot(x = y.circ)
lines.circular(x = x.circ, y = y.circ, nosort = T)

ggplot(CompleteDataWithGoogle, aes(x = "differnce", y = differencebearings)) + geom_boxplot(notch = T)

#how many of the receipts could have experienced precipitation?
CompleteDataWithGoogle %>%
  filter(!is.na(dateprintedcleaned) & !is.na(TimestampDatecleaned)) %>%
  group_by(precip) %>%
  summarise(count = n())

library(ggdark)
ggplot(CompleteDataWithGoogle, aes(x = differencebearings)) + geom_histogram(bins = 10) + scale_x_continuous(breaks = c(seq(0,180, by = 20)))+ dark_theme_classic(base_size = 20, base_line_size = 2) + labs(x = "Direction Angle Difference")
ggplot(CompleteDataWithGoogle, aes(x = DistanceFromLocation/Timedifference)) + geom_histogram(bins = 10) + scale_x_continuous()+ dark_theme_classic(base_size = 20, base_line_size = 2) + scale_x_log10() + labs(x = "Meters Traveled per Day")

#hist(CompleteDataWithGoogle$differencebearings)

write.csv(CompleteDataWithGoogle, "CompleteDataWithGoogle.csv")

source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

theme_rainplot<- function (base_size = 11, base_family = "Arial") 
{
  half_line <- base_size/2
  theme(
    line = element_line(colour = "black", size = rel(1.5), 
                        linetype = 1, lineend = "butt"), 
    rect = element_rect(fill = NA, colour = "black",
                        size = 0.5, linetype = 1),
    text = element_text(family = base_family, face = "plain",
                        colour = "black", size = base_size,
                        lineheight = 0.9,  hjust = 0.5,
                        vjust = 0.5, angle = 0, 
                        margin = margin(), debug = FALSE), 
    
    axis.line = element_line(size = 2, color = "black"), 
    axis.text = element_text(family= "Arial", size = rel(1.5), colour = "grey10"),
    axis.text.x = element_text(margin = margin(t = half_line/2), 
                               vjust = 1), 
    axis.text.y = element_blank(),
    axis.ticks = element_line(colour = "black", size=1), 
    axis.ticks.length = unit(half_line*0.75, "pt"), 
    axis.title = element_text(family="Arial",size = rel(1.5), colour = "black"),
    axis.title.x = element_text(margin = margin(t = half_line*5,
                                                b = half_line)),
    axis.title.y = element_text(angle = 90, 
                                margin = margin(r = half_line*5,
                                                l = half_line)),
    
    legend.background = element_rect(colour = NA), 
    legend.key = element_rect(colour = NA),
    legend.key.size = unit(2, "lines"), 
    legend.key.height = NULL,
    legend.key.width = NULL, 
    legend.text = element_text(family = "Arial", size = rel(1)),
    legend.text.align = NULL,
    legend.title = element_text(family = "Arial", size = rel(1)), 
    legend.title.align = NULL, 
    legend.position = "right", 
    legend.direction = NULL,
    legend.justification = "center", 
    legend.box = NULL, 
    
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.spacing = unit(half_line, "pt"), panel.margin.x = NULL, 
    panel.spacing.y = NULL, panel.ontop = FALSE, 
    
    #Facet Labels
    strip.background = element_blank(),
    strip.text = element_text(family = "Arial",face="bold",colour = "black", size = rel(1.5)),
    strip.text.x = element_text(margin = margin(t = half_line,
                                                b = half_line)), 
    strip.text.y = element_text(angle = 0, 
                                margin = margin(l = half_line, 
                                                r = half_line)),
    strip.switch.pad.grid = unit(5, "lines"),
    strip.switch.pad.wrap = unit(5, "lines"), 
    
    
    plot.background = element_blank(), 
    plot.title = element_text(size = rel(1.5), 
                              margin = margin(b = half_line * 1.2)),
    plot.margin = margin(4*half_line, 4*half_line, 4*half_line, 4*half_line),
    complete = TRUE)
}


ggplot(data = CompleteData, 
       aes(x = 0, y = Timedifference)) +
  geom_flat_violin(aes(fill = ""), position = position_nudge(x = .3, y = 0),adjust =2) +
  geom_point(aes(y = Timedifference, fill = "", color = ""),position = position_jitter(width = .15), size = .75, alpha = 0.5)+
  geom_boxplot(aes(x = 0.22,fill = ""), notch= T, outlier.shape = NA,  width = .1, colour = "BLACK") +
  #geom_label_repel(arrow = arrow(length = unit(0.03, "npc"), type = "closed", ends = "first"),
  #                 force = 10, direction = "y", segment.size = 0.2,label.padding=.1, nudge_x = 1, hjust = 0) +
  #expand_limits(x = 5.25) +
  scale_x_continuous(breaks=NULL) +
  #ylim(0,1000) +
  scale_y_log10(breaks = c(1, 10, 100, 1000), limits = c(1,1000)) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  coord_flip() +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +
  theme_bw() +
  labs(x = "", y = "Time Difference (Days)")+
  theme_rainplot()

ggplot(data = CompleteData, 
       aes(x = 0, y = Value)) +
  geom_flat_violin(aes(fill = ""), position = position_nudge(x = .3, y = 0),adjust =2) +
  geom_point(aes(y = Value, fill = "", color = ""),position = position_jitter(width = .15), size = .75, alpha = 0.5)+
  geom_boxplot(aes(x = 0.22, fill = ""), notch= T, outlier.shape = NA,  width = .1, colour = "BLACK") +
  #geom_label_repel(arrow = arrow(length = unit(0.03, "npc"), type = "closed", ends = "first"),
  #                 force = 10, direction = "y", segment.size = 0.2,label.padding=.1, nudge_x = 1, hjust = 0) +
  #expand_limits(x = 5.25) +
  scale_x_continuous(breaks=NULL) +
  #ylim(0,1000) +
  scale_y_log10(breaks = c(0, 1, 10, 100, 1000), limits = c(0.5,1000)) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  coord_flip() +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +
  theme_bw() +
  labs(x = "", y = "Value (USD)")+
  theme_rainplot()


ggplot(CompleteData, aes(x=Value)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=median(Value, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) + scale_x_log10(limits = c(0.5,1000)) + theme_gray() + theme(aspect.ratio = 1)

ggplot(data = CompleteData, 
       aes(x = 0, y = Value)) +
  geom_flat_violin(aes(fill = ""), position = position_nudge(x = .3, y = 0),adjust =2) +
  geom_point(aes(y = Value, fill = "", color = ""),position = position_jitter(width = .15), size = .75, alpha = 0.5)+
  geom_boxplot(aes(x = 0.22, fill = ""), notch= T, outlier.shape = NA,  width = .1, colour = "BLACK") +
  #geom_label_repel(arrow = arrow(length = unit(0.03, "npc"), type = "closed", ends = "first"),
  #                 force = 10, direction = "y", segment.size = 0.2,label.padding=.1, nudge_x = 1, hjust = 0) +
  #expand_limits(x = 5.25) +
  scale_x_continuous(breaks=NULL) +
  #ylim(0,1000) +
  scale_y_log10(breaks = c(0, 1, 10, 100, 1000), limits = c(0.5,1000)) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  coord_flip() +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +
  theme_bw() +
  labs(x = "", y = "Value (USD)")+
  theme_rainplot()

#Rain plot: Time between when receipt was printed and when it was picked up from site
#ggplot(data = CompleteData, aes(x = "Time Difference (Days)", y = Timedifference))+scale_y_log10() + geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust =2, colour="blue",fill="blue")+
#  geom_boxplot(aes(y = Timedifference),outlier.shape = NA, alpha = 0.1, width = .1, colour = "gray", notch = TRUE)+geom_point(position = position_jitter(width = .15), size = .25) + 
#  ylab('Days')+xlab('')+coord_flip()+theme_cowplot()+guides(fill = FALSE, colour = FALSE) + 
#  ggtitle("Time Difference") + theme_gray()

#Rain plot: Total value of purchases on receipts
#ggplot(data = CompleteData, aes(x = "Value", y = Value)) + geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust =2, colour = "blue", fill = "blue") + 
#  geom_boxplot(aes(y = Value),outlier.shape = NA, alpha = 0.1, width = .1, colour = "gray", notch = TRUE)+geom_point(position = position_jitter(width = .15), size = .25) + 
#  ylab('Dollars')+xlab('')+coord_flip()+theme_cowplot()+guides(fill = FALSE, colour = FALSE) + 
#  ggtitle("Value") + theme_gray()

#Rain plot: Distance between where receipt was picked up and where it came from
ggplot(data = CompleteData, aes(x = "Distance", y = DistanceFromLocation)) + scale_y_log10()+  geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust =2, colour = "blue", fill = "blue") +
  geom_boxplot(aes(y = DistanceFromLocation),outlier.shape = NA, alpha = 0.1, width = .1, colour = "gray", notch = TRUE)+geom_point(position = position_jitter(width = .15), size = .25) + 
  ylab('Meters')+xlab('')+coord_flip()+theme_cowplot()+guides(fill = FALSE, colour = FALSE) + ggtitle("Distance From Location") + theme_gray()

#Bar plot: Frequency of type of location receipt is from 
LocationCounts <- read.csv("LocationTypeCounts.csv")
ggplot(data = LocationCounts, aes(x = reorder(?..Type.of.Location, Count), y = Count)) + geom_bar(stat = "identity") + xlab("Location Type") + geom_text(aes(label = Count), nudge_y = 2) + theme_gray() + coord_flip()

#paymentmethod <- data.frame(paymentmethod = unique(CompleteData$paymentmethodcleaned))
#write.csv(paymentmethod, "paymentmethod.csv")

#Bar plot: Frequency of type of payment used
PaymentCounts <- read.csv("paymentmethod.csv")
ggplot(data = PaymentCounts, aes(x = reorder(Payment.Method, Count), y = Count)) + geom_bar(stat = "identity") + xlab("Payment Method") +  geom_text(aes(label = Count), nudge_y = 3) + theme_gray() + coord_flip()


#LocationNames <- data.frame(Locationnamecleaned = unique(ReceiptsDataandDistance$Locationnamecleaned))
#write.csv(LocationNames, "LocationNames.csv") #Location type categorization of places

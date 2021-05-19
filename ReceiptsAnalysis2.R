setwd("G:/My Drive/GrayLab/Projects/Plastics/ActiveProjects/TrackingTrashReceipts/Code/Jackie")
library(dplyr)
library(ggplot2)
library(ggmap)
library(cowplot)
library(scales)
library(sfsmisc)
#install.packages("scales")
#install.packages("googleway")
library(googleway)
library(leaflet)

#register_google(key = ) #To run geocode function
#set_key(key = ) #To run google_find_place function

BootMeanNoNA <- function(data) {
  B <- 10000
  mean <- numeric(B)
  
  datanona <- data[!is.na(data)]
  n = length(datanona)
  
  set.seed(34345)
  for (i in 1:B) {
    boot <- sample(1:n, size=n, replace = TRUE)
    mean[i] <- mean(datanona[boot])
  }
  return(quantile(mean, c(0.025, 0.5, 0.975)))
}

BootMedianNoNA <- function(data) {
  B <- 10000
  mean <- numeric(B)
  
  datanona <- data[!is.na(data)]
  n = length(datanona)
  
  set.seed(34345)
  for (i in 1:B) {
    boot <- sample(1:n, size=n, replace = TRUE)
    mean[i] <- median(datanona[boot])
  }
  return(quantile(mean, c(0.025, 0.5, 0.975)))
}

Boot90percentNoNA <- function(data){
  B <- 10000
  mean <- numeric(B)
  
  datanona <- data[!is.na(data)]
  n = length(datanona)
  
  set.seed(34345)
  for (i in 1:B) {
    boot <- sample(1:n, size=n, replace = TRUE)
    mean[i] <- quantile(datanona[boot], 0.9)
  }
  return(quantile(mean, c(0.025, 0.5, 0.975)))
}


BrandLocations2018Data <- read.csv("BrandLocations_CleanedDateDTFFormatted2018.csv") %>%
  rename(lat=Latitude, lon=Longitude, material=Material, item= Item, brand=Brand, locationname=Location.Name, manualaddress=Manual.Address, manuallat = Manual.Lat, manuallon = Manual.Lon, datetime=DateAndTime, locationbrand=Location.Brand., photourl=ImageURL, litterid=ID, notes=Notes) %>%
  select(lat, lon, material, item, brand, locationname, manualaddress, manuallat, manuallon, datetime, locationbrand, photourl, litterid, notes)#2018 data
BrandLocationsMyData2 <- read.csv("BrandLocations_MyData2.csv") %>%
  rename(lat=lat, lon=lon, material=Materials, item= Items, brand=Brands, locationname=Location.Name, manualaddress=Manual.Address , manuallat = Manual.Lat, manuallon = Manual.Lon,  datetime=photo_timestamp, locationbrand=Location.Brand., photourl=photo_url, litterid=?..id, notes=Notes) %>%
  select(lat, lon, material, item, brand, locationname, manualaddress,  manuallat, manuallon, datetime, locationbrand, photourl, litterid, notes)#Win's site data
LocationBrandSiteItems <- read.csv("LocationBrandSiteItems.csv") %>%
  rename(lat=lat, lon=lon, material=Material, item= Item, brand=Brand, locationname=Location.Name, manualaddress=Manual.Address, manuallat = Manual.Lat, manuallon = Manual.Long, datetime=litterTimestamp, locationbrand=Location.Brand., photourl=url, litterid=litterId, notes=Notes) %>%
  select(lat, lon, material, item, brand, locationname, manualaddress, manuallat, manuallon, datetime, locationbrand, photourl, litterid, notes)#Jacqueline's site data
str(BrandLocations2018Data)
str(BrandLocationsMyData2)
str(LocationBrandSiteItems)

bind <- rbind(BrandLocations2018Data, BrandLocationsMyData2, LocationBrandSiteItems)

bind <- bind %>%
  mutate(manualaddress = ifelse(manualaddress == "", NA, manualaddress))
#25 doesn't work might need to incorporate the manual edits. Lots of hold ups, I wonder if the API is stalling? I have to manually go in and run the code to get it to move to the next one. Goes about 5 places and then stalls.
for (row in 26:nrow(bind)) { #for loop output was made into a csv file
  combo1vec<-cbind(as.vector(as.numeric(bind$lat[row])), as.vector(as.numeric(bind$lon[row])))
  LocNames1<- bind$locationname[row]
  Search1<-google_find_place(input = LocNames1, fields = c("name", "formatted_address", "geometry/location"), point = combo1vec)
  Search1df<-data.frame(Search1)
  Refined1<-slice(.data = Search1df, ... = 1, .preserve = FALSE)
  bind[row, "googleformattedaddress"] <- Refined1$candidates.formatted_address
  bind[row, "googleformattedlat"] <- Refined1$candidates.geometry$location$lat
  bind[row, "googleformattedlon"] <- Refined1$candidates.geometry$location$lng
  bind[row, "googleformattedname"] <- Refined1$candidates.name
  print(row)
}




#row = row+1
R <- 6371000 #meters

binddistanceformatted <- bind %>%
  mutate_geocode(manualaddress) #%>% 

  mutate(latrad = (lat*pi)/180) %>%
  mutate(lonrad = (lon*pi)/180) %>%
  mutate(googleLatrad = (googleformattedlat*pi)/180) %>%
  mutate(googleLonrad = (googleformattedlon*pi)/180) %>%
  mutate(changelat = latrad - googleLatRad) %>%
  mutate(changelon = lonrad - googleLonRad) %>%
  mutate(a = sin(changeinlat/2) * sin(changeinlat/2)) + (cos(locaddresslat) * cos(timestamplat) * sin(changeinlon/2) * sin(changeinlon/2)) %>%
  mutate(c = 2 * atan2(sqrt(a), sqrt(1-a))) %>%
  mutate(distancemeters = R * c)
          


#Test to see if receipt-place distances are comparable to item-nearestplace distances
LocationBrandSiteItems <- read.csv("LocationBrandSiteItems.csv") #%>% #Jacqueline's site data
  mutate(Locationaddressclean = as.character(Manual.Address)) %>%
  mutate_geocode(Locationaddressclean) #%>% 
  mutate(latrad = (lat*pi)/180) %>%
  mutate(lonrad = (lon*pi)/180) %>%
  mutate(LocationLatrad = (lat1*pi)/180) %>%
  mutate(LocationLonrad = (lon1*pi)/180)

R <- 6371000 #meters
timestamplat <- LocationBrandSiteItems$latrad
locaddresslat <- LocationBrandSiteItems$LocationLatrad
timestamplon <- LocationBrandSiteItems$lonrad
locaddresslon <- LocationBrandSiteItems$LocationLonrad
changeinlat <- timestamplat - locaddresslat
changeinlon <- timestamplon - locaddresslon
a <- (sin(changeinlat/2) * sin(changeinlat/2)) + (cos(locaddresslat) * cos(timestamplat) * sin(changeinlon/2) * sin(changeinlon/2))
c <- 2 * atan2(sqrt(a), sqrt(1-a))
DistanceFromLocation <- R * c
DistanceFromLocation

SiteItemsDistances <- cbind(LocationBrandSiteItems, DistanceFromLocation)

#Test rain plot: distance b/n branded items & nearest place
ggplot(data = SiteItemsDistances, aes(x = "Distance", y = DistanceFromLocation)) + scale_y_log10(breaks = c(100,1000,10000)) + geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust =2, colour = "blue", fill = "blue") +
  geom_boxplot(aes(y = DistanceFromLocation),outlier.shape = NA, alpha = 0.1, width = .1, colour = "gray", notch = TRUE)+geom_point(position = position_jitter(width = .15), size = .25) + 
  ylab('Meters')+xlab('')+coord_flip()+theme_cowplot()+guides(fill = FALSE, colour = FALSE) + ggtitle("Distance From Nearest Location") + theme_gray()




#Create new table with generated nearest place coordinates using google_find_place
FinalResult1<-data.frame(LitterID = character(), ItemLat = character(), ItemLon = character(), ItemBrand = character(), BrandLocation = character(), GoogleAPI = character(), LocationName = character(), LocationAddress = character(), LocationLat = character(), LocationLon = character(), Notes = character(), SearchStatus = character())
for (row in 1:37) { #for loop output was made into a csv file
  lat1vec<-as.vector(as.numeric(LocationBrandSiteItems$lat[row]))
  lon1vec<-as.vector(as.numeric(LocationBrandSiteItems$lon[row]))
  combo1vec<-print(cbind(lat1vec, lon1vec))
  LocNames1<-LocationBrandSiteItems$Location.Name[row]
  Search1<-print(google_find_place(input = LocNames1, fields = c("name", "formatted_address", "geometry/location"), point = combo1vec))
  Search1df<-data.frame(Search1)
  Refined1<-print(slice(.data = Search1df, ... = 1, .preserve = FALSE))
  Refined1df<-data.frame(LitterID = LocationBrandSiteItems$litterId[row], ItemLat = LocationBrandSiteItems$lat[row], ItemLon = LocationBrandSiteItems$lon[row], ItemBrand = LocationBrandSiteItems$Brand[row], BrandLocation = LocationBrandSiteItems$Location.Name[row], GoogleAPI = LocationBrandSiteItems$Google.API.[row], LocationName = Refined1$candidates.name, LocationAddress = Refined1$candidates.formatted_address, LocationLat = Refined1$candidates.geometry$location$lat, LocationLon = Refined1$candidates.geometry$location$lng, Notes = LocationBrandSiteItems$Notes.2[row], SearchStatus = Refined1$status, stringsAsFactors = F)
  FinalResult1<-rbind(FinalResult1,Refined1df)
}
#write.csv(FinalResult1, "JacquelineSiteItemsPlaceLoc.csv")


#Create new table with generated nearest place coordinates using google_find_place
FinalResult2<-data.frame(LitterID = character(), ItemLat = character(), ItemLon = character(), ItemBrand = character(), BrandLocation = character(), GoogleAPI = character(), LocationName = character(), LocationAddress = character(), LocationLat = character(), LocationLon = character(), Notes = character(), SearchStatus = character())
for (row in 1:130) { #for loop output was made into a csv file
  lat2vec<-as.vector(as.numeric(BrandLocationsMyData2$lat[row]))
  lon2vec<-as.vector(as.numeric(BrandLocationsMyData2$lon[row]))
  combo2vec<-print(cbind(lat2vec, lon2vec))
  LocNames2<-BrandLocationsMyData2$Location.Name[row]
  Search2<-print(google_find_place(input = LocNames2, fields = c("name", "formatted_address", "geometry/location"), point = combo2vec))
  Search2df<-data.frame(Search2)
  Refined2<-print(slice(.data = Search2df, ... = 1, .preserve = FALSE))
  Refined2df<-data.frame(LitterID = BrandLocationsMyData2$ï..id[row], ItemLat = BrandLocationsMyData2$lat[row], ItemLon = BrandLocationsMyData2$lon[row], ItemBrand = BrandLocationsMyData2$Brands[row], BrandLocation = BrandLocationsMyData2$Location.Name[row], GoogleAPI = BrandLocationsMyData2$Google.API.[row], LocationName = Refined2$candidates.name, LocationAddress = Refined2$candidates.formatted_address, LocationLat = Refined2$candidates.geometry$location$lat, LocationLon = Refined2$candidates.geometry$location$lng, Notes = BrandLocationsMyData2$Notes[row], SearchStatus = Refined2$status, stringsAsFactors = F)
  FinalResult2<-rbind(FinalResult2, Refined2df)
}
#write.csv(FinalResult2, "MyData2SiteItemsPlaceLoc.csv")


#Create new table with generated nearest place coordinates using google_find_place
FinalResult3 <- data.frame(LitterID = character(), ItemLat = character(), ItemLon = character(), ItemBrand = character(), BrandLocation = character(), GoogleAPI = character(), LocationName = character(), LocationAddress = character(), LocationLat = character(), LocationLon = character(), Notes = character(), SearchStatus = character())
for (row in 1:207) { #for loop output was made into a csv file
  lat3vec<-as.vector(as.numeric(BrandLocations2018Data$Latitude[row]))
  lon3vec<-as.vector(as.numeric(BrandLocations2018Data$Longitude[row]))
  combo3vec<-print(cbind(lat3vec, lon3vec))
  LocNames3<-BrandLocations2018Data$Location.Name[row]
  Search3<-print(google_find_place(input = LocNames3, fields = c("name", "formatted_address", "geometry/location"), point = combo3vec))
  Search3df<-data.frame(Search3)
  Refined3<-print(slice(.data = Search3df, ... = 1, .preserve = FALSE))
  Refined3df<-data.frame(LitterID = BrandLocations2018Data$ID[row], ItemLat = BrandLocations2018Data$Latitude[row], ItemLon = BrandLocations2018Data$Longitude[row], ItemBrand = BrandLocations2018Data$Brand[row], BrandLocation = BrandLocations2018Data$Location.Name[row], GoogleAPI = BrandLocations2018Data$Google.API.[row], LocationName = Refined3$candidates.name, LocationAddress = Refined3$candidates.formatted_address, LocationLat = Refined3$candidates.geometry$location$lat, LocationLon = Refined3$candidates.geometry$location$lng, Notes = BrandLocations2018Data$Notes[row], SearchStatus = Refined3$status, stringsAsFactors = F)
  FinalResult3<-rbind(FinalResult3, Refined3df)
}
#write.csv(FinalResult3, "Data2018SiteItemsPlaceLoc.csv")


#######################################
###Skip ahead to hear for final result
#######################################


JacquelineSiteItemsPlaceLoc<-read.csv("JacquelineSiteItemsPlaceLoc.csv")
MyData2SiteItemsPlaceLoc<-read.csv("MyData2SiteItemsPlaceLoc.csv")
Data2018SiteItemsPlaceLoc<-read.csv("Data2018SiteItemsPlaceLoc.csv")
AllData <- rbind(JacquelineSiteItemsPlaceLoc, MyData2SiteItemsPlaceLoc, Data2018SiteItemsPlaceLoc) #all 3 for loop output tables combined
CompleteData <- read.csv("CompleteDataWithGoogle.csv")

CheckDuplicates<-AllData %>% #checked AllData for duplicates
  mutate(duplicated(AllData$LitterID))

#To find the distance between the location where the item was picked up and the location of the nearest place it could have came from
AllData2 <- AllData %>%
  mutate(itemLatrad = (ItemLat*pi)/180) %>%
  mutate(itemLonrad = (ItemLon*pi)/180) %>%
  mutate(placeLatrad = (LocationLat*pi)/180) %>%
  mutate(placeLonrad = (LocationLon*pi)/180)

R1 <- 6371000 #meters
itemLatrad <- AllData2$itemLatrad
placeLatrad <- AllData2$placeLatrad
itemLonrad <- AllData2$itemLonrad
placeLonrad <- AllData2$placeLonrad
changeinlat1 <- itemLatrad - placeLatrad
changeinlon1 <- itemLonrad - placeLonrad
a1 <- (sin(changeinlat1/2) * sin(changeinlat1/2)) + (cos(placeLatrad) * cos(itemLatrad) * sin(changeinlon1/2) * sin(changeinlon1/2))
c1 <- 2 * atan2(sqrt(a1), sqrt(1-a1))
ItemPlaceDistance <- R1 * c1

FinalItemBrandPlace <- cbind(AllData2, ItemPlaceDistance) #Final version: item-nearestplace distances and rest of attributes

#null hypothesis is that they are drawn from the same distribution. 
ks.test(FinalItemBrandPlace$ItemPlaceDistance, CompleteData$DistanceFromLocation, alternative = "two.sided")
ks.test(FinalItemBrandPlace$ItemPlaceDistance, CompleteData$googledistancemeters, alternative = "two.sided")

options(scipen = 999)
ggplot() + stat_ecdf(aes(FinalItemBrandPlace$ItemPlaceDistance), size = 3, color = "red") + stat_ecdf(aes(CompleteData$DistanceFromLocation), size = 3) + stat_ecdf(aes(CompleteData$googledistancemeters), size = 3, color = "gray") + scale_color_viridis_d() + scale_x_log10() + labs(x = "Distance From Origin (m)", y = "Percent Shorter")+ theme_gray() 


#Fit distribution to receipt data
library(fitdistrplus) #https://cran.r-project.org/web/packages/fitdistrplus/vignettes/paper2JSS.pdf

data <- CompleteData$DistanceFromLocation[!is.na(CompleteData$DistanceFromLocation)]/1000
#data <- FinalItemBrandPlace$ItemPlaceDistance[!is.na(FinalItemBrandPlace$ItemPlaceDistance)]/1000

summary(data)
BootMeanNoNA(data)
BootMedianNoNA(data)
length(data)
unique(CompleteData$username)
#unique(FinalItemBrandPlace$)


hist(log10(data))

plotdist(data, histo = T, demp = T)
descdist(data, boot = 1000)

fitw<- fitdist(data, "weibull")
summary(fitw)
plot(fitw)

fitgamma<- fitdist(data, "gamma")
summary(fitgamma)
plot(fitgamma)

fitln<- fitdist(data, "lnorm")
summary(fitln)
plot(fitln)

par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fitw, fitln, fitgamma), legendtext = plot.legend)
qqcomp(list(fitw, fitln, fitgamma), legendtext = plot.legend)
cdfcomp(list(fitw, fitln, fitgamma), legendtext = plot.legend)
ppcomp(list(fitw, fitln, fitgamma), legendtext = plot.legend)

library(sf)

Receipts <- CompleteData %>%
  st_as_sf(coords = c("lon", "lat")) %>% 
  st_sf(crs = 4326)

st_write(Receipts, "Receipts2.shp")
write.csv(CompleteData, "Receipts.csv")

Brands <- FinalItemBrandPlace %>%
  st_as_sf(coords = c("ItemLon", "ItemLat")) %>% 
  st_sf(crs = 4326) #

st_write(Brands, "Brands2.shp")
write.csv(FinalItemBrandPlace, "Brands.csv")

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = Receipts, radius = 5) %>%
  addCircleMarkers(data = Brands, color = "red", radius = 5)






cor.test(Data2018SiteItemsPlaceLoc$ItemLat, Data2018SiteItemsPlaceLoc$ItemLon, method = "spearman")


ggplot() + geom_density(aes(FinalItemBrandPlace$ItemPlaceDistance), size = 3, color = "red") + geom_density(aes(CompleteData$DistanceFromLocation), size = 3) + scale_color_viridis_d() + scale_x_log10(breaks = c(1, 10, 100, 1000), labels = c(1, 10, 100, 1000), limits = c(1,1000)) + theme_gray() 

BootMeanNoNA(FinalItemBrandPlace$ItemPlaceDistance)
BootMeanNoNA(CompleteData$DistanceFromLocation)

BootMedianNoNA(FinalItemBrandPlace$ItemPlaceDistance)
BootMedianNoNA(CompleteData$DistanceFromLocation)

Boot90percentNoNA(FinalItemBrandPlace$ItemPlaceDistance)
Boot90percentNoNA(CompleteData$DistanceFromLocation)

max(FinalItemBrandPlace$ItemPlaceDistance)
max(CompleteData$DistanceFromLocation, na.rm = T)

min(FinalItemBrandPlace$ItemPlaceDistance)
min(CompleteData$DistanceFromLocation, na.rm = T)

ecdf.ksCI(log10(FinalItemBrandPlace$ItemPlaceDistance))
ecdf.ksCI(log10(CompleteData$DistanceFromLocation))


#Rain plot: distance between item and nearest place
ggplot(data = FinalItemBrandPlace, aes(x = "Distance", y = ItemPlaceDistance)) + scale_y_log10()+  geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust =2, colour = "blue", fill = "blue") +
  geom_boxplot(aes(y = ItemPlaceDistance),outlier.shape = NA, alpha = 0.1, width = .1, colour = "gray", notch = TRUE)+geom_point(position = position_jitter(width = .15), size = .25) + 
  ylab('Meters')+xlab('')+coord_flip()+theme_cowplot()+guides(fill = FALSE, colour = FALSE) + ggtitle("Distance From Place") + theme_gray()

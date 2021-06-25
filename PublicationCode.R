#Notes ----
#Check out this wind data https://mrcc.illinois.edu/CLIMATE/Hourly/WindRose.jsp

#Libraries ----
library(readxl)
#library(dplyr)
library(ggplot2)
library(ggmap)
library(cowplot)
library(readr)
library(lavaan)
library(smooth)
library(Hmisc)
library(googleway)
library(ggforce)
#library(stormwindmodel)
library(RCurl)
library(httr)
library(hexView)
library(jsonlite)
library(data.table)
library(dplyr)
library(tidyr)
library(fitdistrplus) #https://cran.r-project.org/web/packages/fitdistrplus/vignettes/paper2JSS.pdf
library(swfscMisc)
library(circular)
library(geosphere)
#devtools::install_github(repo = "michaelmalick/r-malick")
library(malick)
library(collapsibleTree)
#library(dplyr)
#library(tidyr)
library(data.tree)
library(plotly)
library(ggdark)

#Functions ----
#Function returns the difference between two bearings
angle_diff <- function(theta1, theta2){
  theta <- abs(theta1 - theta2) %% 360 
  return(ifelse(theta > 180, 360 - theta, theta))
}

rainpresabs <- function(startdate, enddate){
  any(Weather %>%
        dplyr::filter(Date >= as.Date(startdate, "%Y-%m-%d") & Date <= as.Date(enddate, "%Y-%m-%d")) %>%
        dplyr::select(Total.Precip..in.) %>%
        pull() > 0, na.rm = T)
}

differenceofmean <- function(startdate, enddate){
  Weather %>%
    dplyr::filter(Date >= as.Date(startdate, "%Y-%m-%d") & Date <= as.Date(enddate, "%Y-%m-%d")) %>%
    dplyr::select(Mean.Wind.Dir..deg.) %>%
    pull() %>%
    circular(type="directions", units="degrees", template="geographics", rotation="clock") %>%
    mean.circular(na.rm = T) %>%
    ifelse(.<0, .+360, .)
}

#https://www.flutterbys.com.au/stats/tut/tut13.2.html
calc_shannon <- function(community) {
  p <- table(community)/length(community) # Find proportions
  p <- p[p > 0] # Get rid of zero proportions (log zero is undefined)
  -sum(p * log(p)) # Calculate index
}

calc_simpson <- function(community) {
  p <- table(community)/length(community) # Find proportions
  1 / sum(p^2) # Calculate index
}

calc_menshinicks <- function(community) {
  p <- length(unique(community))/sqrt(length(community)) # Find proportions
  p # Calculate index
}

calc_numgroups <- function(community) {
  p <- length(unique(community)) # Find proportions
  p # Calculate index
}

cleantext <- function(x) {
  tolower(gsub("[[:space:]]", "", x))
}

not_all_na <- function(x) any(!is.na(x))

#requires alias to have Alias, RowID and Key columns, requires DF to have a count column and some column to match
jointohierarchy <- function(DF, Hierarchy, Alias, ColNum) {
  DF <- mutate_all(DF, cleantext) %>%
    mutate(Count = as.numeric(Count))
  Hierarchy <- mutate_all(Hierarchy, cleantext)
  Alias <- mutate_all(Alias, cleantext) %>%
    mutate(RowID = as.integer(RowID))
  
  DF$RowID <- unlist(apply(DF, 1, function(x) which(Alias == as.character(x[ColNum]), arr.ind = TRUE)[1]))
  
  DF <- DF %>%
    left_join(dplyr::select(Alias, RowID, Key)) %>%
    dplyr::select(Key, Count) %>%
    mutate(Key = ifelse(is.na(Key), "other", Key))
  
  list <- apply(DF, 1, function(x) which(Hierarchy == as.character(x[1]), arr.ind = TRUE))
  
  for(n in 1:length(list)){
    if(length(list[[n]]) == 0) next
    DF[n, "Row"] <- unname(list[[n]][1,1])
    DF[n, "Column"] <- unname(list[[n]][1,2])
  }
  
  RowColSum <- DF %>%
    group_by(Row, Column) %>%
    summarise(sum = sum(Count)) %>%
    filter(!is.na(Row)) #This one of the culprits, the rows aren't being matched so they are being dropped and we need some backup mechanism. 
  
  HierarcyFinalForm <- Hierarchy[0,]
  HierarcyFinalForm$sum <- numeric()
  
  row = 25
  for(row in 1:nrow(RowColSum)){
    filter1 <- Hierarchy %>%
      filter(.[[1]] == Hierarchy[unlist(RowColSum[row, "Row"]),1])
    filter2 <- filter1 %>%
      filter_all(any_vars(. == Hierarchy[unlist(RowColSum[row, "Row"]),unlist(RowColSum[row, "Column"])])) %>%
      dplyr::select(where(not_all_na)) %>%
      mutate(sum = unlist(RowColSum[row, "sum"]))
    
    if(unlist(RowColSum[row, "Column"]) < ncol(filter2)-1) {
      filter2[,(unlist(RowColSum[row, "Column"])+1):(ncol(filter2)-1)] <- NA
    }
    filter2 <- filter2 %>%
      distinct()
    
    #print(sum(filter2$sum) == sum(RowColSum[row, "sum"]))
    
    HierarcyFinalForm <- bind_rows(HierarcyFinalForm, filter2)
  }
  
  test <-  HierarcyFinalForm %>%
    mutate_if(is.character, as.factor) %>%
    mutate(sum = as.numeric(unname(sum))) %>%
    dplyr::group_by(across(c(-sum))) %>%
    summarise(sum = sum(sum)) %>%
    ungroup() %>%
    mutate_if(is.factor, as.character) 
  
}

removeslash <- function(x){
  gsub("/", " OR ", x)
}

converttotree <- function(x){
  #x[is.na(x)] <- ""
  x <- mutate_all(x, removeslash) %>%
    mutate(key = "trash") %>%
    mutate(sum = as.numeric(sum)) %>%
    dplyr::relocate(key) %>%
    dplyr::group_by(across(c(-sum))) %>%
    dplyr::summarise(sum = sum(sum)) %>%
    unite(pathString, sep = "/", na.rm = T, -sum) ##Seems like we may be losing some of the sums here, would expect original values to be equal to the summed.
  FromDataFrameTable(x)
}

#check out this: https://stackoverflow.com/questions/45225671/aggregating-values-on-a-data-tree-with-r
myApply <- function(node) {
  node$totalsum <- 
    sum(c(node$sum, purrr::map_dbl(node$children, myApply)), na.rm = TRUE)
}

#DF2 <- SMC
#Hierarchy <- ItemsHierarchy
#Alias <- ItemsAlias
#ColNum <- 2


AggregateTrees <- function(DF1, Alias, Hierarchy, ColNum){
  DFA <- jointohierarchy(DF = DF1, Hierarchy = Hierarchy, Alias = Alias, ColNum = ColNum)%>%
    add_row(X1 = "missing", sum = sum(DF1$Count) - sum(.$sum)) #May need to be changed to X1 or Level.1 depending on the hierarchy dataset. Should clean them up.
  
  bindedtree <- converttotree(DFA)  
  
  myApply(bindedtree)
  
  bindedtree
  
}

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


#Datasets ----

source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")


CompleteDataWithGoogle <- read.csv("CompleteDataWithGoogle.csv") %>%
  mutate(startdate = as.Date(dateprintedcleaned, "%Y-%m-%d"), enddate =  as.Date(TimestampDatecleaned, "%Y-%m-%d"))

Trips <- fread("Trips_by_Distance.csv")

Weather <- read.csv("RiversideMuniAirport_Cleaned.csv") %>%
  na_if("M") %>%
  mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
  mutate(across(where(is.character), as.numeric))

full_data <- fread("Litterati-Partners.csv") %>%
  mutate(timestamp = as.POSIXct(gsub(" GMT", "", Date), format = "%d/%m/%Y %H:%M:%OS")) %>%
  mutate(lat = as.numeric(gsub("/.{1,}", "", `Location (Lat / Long)`))) %>%
  mutate(lon = as.numeric(gsub(".{1,}/", "", `Location (Lat / Long)`))) %>%
  mutate(week = strftime(timestamp, format = "%W")) %>%
  mutate(day = strftime(timestamp, format = "%D"))

#vechaversine <- Vectorize(haversine)

#data_2018 <- fread("CleanedData2018.csv") %>%
#  dplyr::select(ID:Brand) %>%
#  rename(Litter = ID, date_unformatted = Date) %>%
#  inner_join(full_data) %>%
#  dplyr::select(Litter, Longitude, Latitude, timestamp, Username, Name, Material, Item, Brand) %>%
#  group_by(Username) %>%
#  mutate(minlat = min(Latitude), maxlat = max(Latitude), minlon = min(Longitude), maxlon = max(Longitude)) %>%
#  ungroup()%>%
#  mutate(horizdist = vechaversine(lon1 = minlon, lat1 = maxlat, lon2 = maxlon, lat2 = maxlat), vertdist = vechaversine(lon1 = minlon, lat1 = minlat, lon2 = minlon, lat2 = maxlat)) %>%
#  mutate(area_m2 = horizdist * vertdist)

site_data_cleaned <- fread("StudyAreas/User_Cleaned_Data/reconciled_cleaned.csv") %>%
  left_join(fread("StudyAreas/User_Cleaned_Data/weekend_sweep.csv")) %>%
  mutate(Date = as.Date(Day, format = "%m/%d/%Y"))
  

#Dataset source
#https://data.bts.gov/Research-and-Statistics/Trips-by-Distance/w96p-f2qv

#Basic Stats 2018 ----

#Input Rate Changes
input_rate <- site_data_cleaned %>%
  #filter(user_id == 92684) %>%
  #mutate(Date = substr(photo_timestamp,1,nchar(photo_timestamp)-3)) %>%
  group_by(Date, Name, Site_Length_m, Weekend, Sweeping) %>%
  summarise(Intensity = n()) %>%
  arrange(Date) %>%
  group_by(Name) %>%
  mutate(DateDiff = as.numeric(Date) - dplyr::lag(as.numeric(Date), order_by = Name)) %>%
  mutate(generationrate = Intensity/DateDiff/Site_Length_m) %>%
  filter(Date != as.Date("3/23/2020", format = "%m/%d/%Y")) %>% #Bring back in for 2020 analysis.
  ungroup() 

ggplot(input_rate) + geom_boxplot(aes(x = Name, y = generationrate, color = Weekend), notch = T) + scale_y_log10()
ggplot(input_rate) + geom_boxplot(aes(x = Name, y = generationrate, color = Sweeping), notch = T) + scale_y_log10()


ggplot(input_rate) + geom_boxplot(aes(y = generationrate, color = Weekend), notch = T) + scale_y_log10()
ggplot(input_rate) + geom_boxplot(aes(y = generationrate, color = Sweeping), notch = T) + scale_y_log10()

#ggplot(input_rate) + geom_point(aes(x = Date, y = generationrate), notch = T) + scale_y_log10()
# get all the start and end points

ggplot(input_rate, aes(x = Name, y = generationrate)) + 
  geom_boxplot(notch = T) + 
  #geom_smooth(method = "lm") +
  #geom_tile(aes(y=weekends$weekend*max(generationrate)), fill="yellow") +
  #facet_wrap(Name ~., scales = "free_x") + 
  theme_bw() + 
  labs(y = "Generation Rate #/Day/m") + 
  scale_y_log10() 

ggplot(input_rate, aes(x = Date, y = generationrate)) + 
  geom_point() + 
  #geom_smooth(method = "lm") +
  #geom_tile(aes(y=weekends$weekend*max(generationrate)), fill="yellow") +
  facet_wrap(Name ~., scales = "free_x") + 
  theme_bw() + 
  labs(y = "Generation Rate #/Day/m") + 
  scale_y_log10() + 
  scale_x_date(date_minor_breaks = "1 week")

ggplot(input_rate, aes(x = Date, y = Name)) + 
  geom_point(alpha = 0.5, size = 4) + 
  #geom_smooth(method = "lm") +
  #facet_wrap(Name ~., scales = "free") + 
  theme_bw() + 
  labs(y = "Site") + 
  scale_x_date(date_minor_breaks = "1 month")

#Input Rate Correlation
mean_input_rate <- site_data_cleaned %>%
  #filter(user_id == 92684) %>%
  #mutate(Date = substr(photo_timestamp,1,nchar(photo_timestamp)-3)) %>%
  mutate(Date = as.Date(Day, format = "%m/%d/%Y")) %>%
  group_by(Date, Name, Site_Length_m, `Mean Income_households_2019_5year_census`, `Median Income_households`, Cal_Enviro_Screen, Road_Width_m, Landuse_Type) %>%
  summarise(Intensity = n()) %>%
  arrange(Date) %>%
  group_by(Name) %>%
  mutate(DateDiff = as.numeric(Date) - dplyr::lag(as.numeric(Date), order_by = Name)) %>%
  mutate(generationrate = Intensity/DateDiff/Site_Length_m) %>%
  filter(Date != as.Date("3/23/2020", format = "%m/%d/%Y")) %>% #Bring back in for 2020 analysis.
  ungroup() %>%
  group_by(Name, `Mean Income_households_2019_5year_census`, `Median Income_households`, Cal_Enviro_Screen, Road_Width_m, Landuse_Type) %>%
  summarise(mean = mean(generationrate, na.rm = T)) %>%
  ungroup() %>%
  dplyr::select(-Name) %>%
  mutate(Cal_Enviro_Screen = case_when(
    Cal_Enviro_Screen == "40-45" ~ 42.5,
    Cal_Enviro_Screen == "65-70" ~ 67.5,
    Cal_Enviro_Screen == "80-85" ~ 82.5,
    Cal_Enviro_Screen == "85-90" ~ 87.5,
    Cal_Enviro_Screen == "90-95" ~ 92.5,
    Cal_Enviro_Screen == "95-100" ~ 97.5,
  )) %>%
  mutate(Landuse_Type = case_when(
    Landuse_Type == "Residential" ~ 1,
    Landuse_Type == "Mixed" ~0
  )) %>%
  gather(key, value, -mean) %>%
  mutate(value = as.numeric(value))

ggplot(mean_input_rate) + 
  geom_point(aes(x = value, y = mean)) + 
  facet_wrap(key~., scales = "free") + 
  labs(y = "Mean Trash Generation (#/m/day") + 
  theme_bw()

#Material Types By Site and Date
material_composition <- site_data_cleaned %>%
  #filter(user_id == 92684) %>%
  #mutate(Date = substr(photo_timestamp,1,nchar(photo_timestamp)-3)) %>%
  #mutate(Date = as.Date(Day, format = "%m/%d/%Y")) %>%
  #mutate(Date = as.Date(timestamp)) %>%
  group_by(Date, Name, Material_TT) %>%
  summarise(Intensity = n()) %>%
  ungroup()

ggplot(arrange(material_composition, Date, Name, Intensity), aes(fill=Material_TT, y=Intensity, x=Date)) + 
  geom_bar(position="fill", stat="identity")+
  #scale_fill_viridis_d() +
  facet_wrap(Name ~. , scales = "free") + 
  theme_bw()


#Trash Diversity
material_diversity <- site_data_cleaned %>%
  #filter(user_id == 92684) %>%
  #mutate(Date = substr(photo_timestamp,1,nchar(photo_timestamp)-3)) %>%
  mutate(Date = as.Date(Day, format = "%m/%d/%Y")) %>%
  group_by(Date, Name) %>%
  dplyr::summarise(shannon = calc_shannon(Material_TT),
                   simpson = calc_simpson(Material_TT),
                   menhincks = calc_menshinicks(Material_TT),
                   numgroups = calc_numgroups(Material_TT)) %>%
  mutate(evenness = shannon/log(numgroups))

ggplot(material_diversity, aes(x = evenness, y = numgroups, color = Name)) +
  geom_point() + 
  scale_color_viridis_d(option = "B") + 
  theme_bw()

ggplot(material_diversity) + 
  geom_point(aes(x = Date, y = shannon)) + 
  geom_point(aes(x = Date, y = simpson), color = "red") + 
  geom_point(aes(x = Date, y = menhincks), color = "blue") + 
  geom_point(aes(x = Date, y = numgroups), color = "green") +
  
  #geom_smooth(method = "lm") +
  #geom_tile(aes(y=weekends$weekend*max(generationrate)), fill="yellow") +
  facet_wrap(Name ~., scales = "free") + 
  theme_bw() + 
  labs(y = "Diversity") + 
  #scale_y_log10() + 
  scale_x_date(date_minor_breaks = "1 week")


#Data sets ----
ItemsHierarchy <- read.csv("Taxonomy/ITEMSHierarchyLower.csv")

MaterialsHierarchy <- read.csv("Taxonomy/MaterialsHierarchyLower.csv") 

ItemsAlias <- read.csv("Taxonomy/PrimeItems.csv")%>%
  mutate(RowID = 1:nrow(.)) %>%
  rename(Key = Item)

MaterialsAlias <- read.csv("Taxonomy/PrimeMaterials.csv") %>%
  mutate(RowID = 1:nrow(.)) %>%
  rename(Key = Material)

ItemsHierarchy <- ItemsHierarchy %>%
  rename(X1 = Level.1)
#Data Processing ----

DF <- site_data_cleaned %>%
  group_by(Material_TT) %>%
  summarise(Count = n())

DF2 <- site_data_cleaned %>%
  group_by(Item_TT) %>%
  summarise(Count = n())

#Output for lumping analysis ----
DFA <- jointohierarchy(DF2, Hierarchy = ItemsHierarchy, Alias = ItemsAlias, ColNum = 1)%>%
  add_row(X1 = "missing", sum = sum(DF2$Count) - sum(.$sum)) #May need to be changed to X1 or Level.1 depending on the hierarchy dataset. Should clean them up.


ItemTree <- AggregateTrees(DF2,  Alias = ItemsAlias, Hierarchy = ItemsHierarchy, ColNum = 1)
ItemTreedf <- ToDataFrameNetwork(ItemTree, "totalsum")
#Next add plotly figure with this df. 

MaterialTree <- AggregateTrees(DF,  Alias = MaterialsAlias, Hierarchy = MaterialsHierarchy, ColNum = 1)
MaterialTreedf <- ToDataFrameNetwork(MaterialTree, "totalsum")

#Figures of the hierarchy trees ----
collapsibleTree(
  MaterialsHierarchy, hierarchy = c(names(MaterialsHierarchy)), collapsed = F, fontSize = 30, zoomable = T
)

collapsibleTree(
  ItemsHierarchy, hierarchy = c(names(ItemsHierarchy)), fontSize = 20, zoomable = T, width = 3000, height = 1000
)

#Trip Distances ----
#Was thinking that we should limit this to work trips but I don't think so any more. 
IETrips <- Trips %>%
  filter(`State Postal Code` == "CA") %>%
  mutate(DateFormatted = as.Date(Date)) %>%
  filter(DateFormatted < "2019-12-30") %>%
  filter(`County Name` == "Riverside County" | `County Name` == "San Bernardino County")
#How do we treat the population staying at home? Are those zeros?
#We got ourselves a double sided censorship here. End is greater than, and beginning is less than. 

IEDistribution <- IETrips %>%
  dplyr::select(`Number of Trips`:`Number of Trips >=500`) %>%
  summarise(across(everything(), ~sum(.x, na.rm = T)))

pdf_ie_dist <- IEDistribution %>%
  pivot_longer(everything()) %>%
  filter(name != "Number of Trips") %>%
  mutate(name = factor(name, levels = c("Number of Trips <1", 
                                        "Number of Trips 1-3", 
                                        "Number of Trips 3-5", 
                                        "Number of Trips 5-10",
                                        "Number of Trips 10-25",
                                        "Number of Trips 25-50",
                                        "Number of Trips 50-100",
                                        "Number of Trips 100-250",
                                        "Number of Trips 250-500",
                                        "Number of Trips >=500"
                                        ))) %>%
  mutate(max_dist = case_when(
    name == "Number of Trips <1" ~ 1,
    name == "Number of Trips 1-3" ~ 3,
    name == "Number of Trips 3-5" ~ 5,
    name == "Number of Trips 5-10" ~ 10,
    name == "Number of Trips 10-25" ~ 25,
    name == "Number of Trips 25-50" ~ 50,
    name == "Number of Trips 50-100" ~ 100,
    name == "Number of Trips 100-250" ~ 250,
    name == "Number of Trips 250-500" ~ 500,
    name == "Number of Trips >=500" ~ 3000,
  )) %>%
  mutate(min_dist = case_when(
    name == "Number of Trips <1" ~ 0,
    name == "Number of Trips 1-3" ~ 1,
    name == "Number of Trips 3-5" ~ 3,
    name == "Number of Trips 5-10" ~ 5,
    name == "Number of Trips 10-25" ~ 10,
    name == "Number of Trips 25-50" ~ 25,
    name == "Number of Trips 50-100" ~ 50,
    name == "Number of Trips 100-250" ~ 100,
    name == "Number of Trips 250-500" ~ 250,
    name == "Number of Trips >=500" ~ 500,
  )) %>%
  mutate(mean_dist = (max_dist + min_dist) / 2) %>%
  mutate(percent = value/sum(value)) %>%
  mutate(count = round(percent * 10000))

#Going with lognormal
montecarlo_vector <- c()

#Or Uniform
for(row in 1:nrow(pdf_ie_dist)){
  montecarlo_vector <- c(montecarlo_vector, runif(n = unlist(pdf_ie_dist[row, "count"]), min = unlist(pdf_ie_dist[row, "min_dist"]), max = unlist(pdf_ie_dist[row, "max_dist"]))) 
}

#Result doesn't seem to depend much on which of the above we choose. Could also try log uniform, might be a nice way to get to the middle ground.

#In meters
ggplot() + 
  stat_ecdf(data = CompleteDataWithGoogle, aes(x = DistanceFromLocation), color = "red") + 
  stat_ecdf(aes(x = montecarlo_vector * 1.60934 * 1000), color = "blue") + 
  scale_x_log10()

montecarlo_meters = montecarlo_vector * 1.60934 * 1000

receipt_distance_quantiles = quantile(CompleteDataWithGoogle$DistanceFromLocation, probs = seq(0.01, 0.99, by = 0.01), na.rm = T)
montecarlo_distance_quantiles = quantile(montecarlo_meters, probs = seq(0.01, 0.99, by = 0.01), na.rm = T)

# Plot Quantiles against one another. 
ggplot() + 
geom_point(aes(y = receipt_distance_quantiles, x = montecarlo_distance_quantiles)) + 
  geom_smooth(aes(y = receipt_distance_quantiles, x = montecarlo_distance_quantiles),
              method = "lm") + 
  scale_x_log10() + 
  scale_y_log10()

linear_quantile_regression = lm(log10(receipt_distance_quantiles) ~ log10(montecarlo_distance_quantiles))

#Might not be the best way to do this because the data set is synthesized.
#ks.test(x = montecarlo_vector * 1.60934 * 1000, y =  CompleteDataWithGoogle$DistanceFromLocation)

#Wind and Rain ----

#CompleteDataWithGoogle_Weather <- 
#Receipt Distances ----

for(row in 1:nrow(CompleteDataWithGoogle)){ #This is the inverse of what we would think because wind direction is the inverse. This will tell us what direction the trash came from.
  CompleteDataWithGoogle[row, "bearing"] <-bearing(lat2 = CompleteDataWithGoogle[row,"LocLat"]*pi/180, 
                                                   lon2 = CompleteDataWithGoogle[row,"LocLon"]*pi/180,
                                                   lat1 = CompleteDataWithGoogle[row,"lat"]*pi/180, 
                                                   lon1 = CompleteDataWithGoogle[row,"lon"]*pi/180)[1]
}


x.circ <- circular(CompleteDataWithGoogle$bearing, type="directions", units="degrees", template="geographics", rotation="clock")
rose.diag(x.circ, bins=10, col="gray", border=NA)

CompleteDataWithGoogle %>%
  filter(!is.na(Timedifference) & !is.na(bearing)) %>%
  as_tibble()

startdate <- CompleteDataWithGoogle[1, "dateprintedcleaned"]
enddate <- CompleteDataWithGoogle[1, "TimestampDatecleaned"]
bearing <- CompleteDataWithGoogle[1, "bearing"] 

#Need to convert bearing to azimuth
startdate = CompleteDataWithGoogle[1,"startdate"] 
enddate = CompleteDataWithGoogle[1,"enddate"]

#difference between bearings.
for(row in 1:nrow(CompleteDataWithGoogle)){
  CompleteDataWithGoogle[row, "meanbearing"] <- differenceofmean(startdate = CompleteDataWithGoogle[row,"startdate"], enddate = CompleteDataWithGoogle[row,"enddate"])
}

CompleteDataWithGoogle$differencebearings <- angle_diff(CompleteDataWithGoogle$bearing, CompleteDataWithGoogle$meanbearing)

cor.circular(CompleteDataWithGoogle$bearing, CompleteDataWithGoogle$meanbearing, test = T)


for(row in 1:nrow(CompleteDataWithGoogle)){
  CompleteDataWithGoogle[row, "precip"] <-rainpresabs(startdate = CompleteDataWithGoogle[row,"dateprintedcleaned"], enddate = CompleteDataWithGoogle[row,"TimestampDatecleaned"])
}

#Wind velocity transport velocity correlation ----

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
  summarise(count = n()) %>%
  ggplot() + geom_col(aes(x = precip, y = count))

ggplot(CompleteDataWithGoogle, aes(x = differencebearings)) + geom_histogram(bins = 10) + scale_x_continuous(breaks = c(seq(0,180, by = 20)))+ dark_theme_classic(base_size = 20, base_line_size = 2) + labs(x = "Direction Angle Difference")
ggplot(CompleteDataWithGoogle, aes(x = DistanceFromLocation/Timedifference)) + geom_histogram(bins = 10) + scale_x_continuous()+ dark_theme_classic(base_size = 20, base_line_size = 2) + scale_x_log10() + labs(x = "Meters Traveled per Day")

#hist(CompleteDataWithGoogle$differencebearings)

write.csv(CompleteDataWithGoogle, "CompleteDataWithGoogle.csv")

ggplot(data = CompleteData, 
       aes(x = 0, y = Timedifference)) +
  geom_flat_violin(aes(fill = ""), position = position_nudge(x = .3, y = 0),adjust =2) +
  geom_point(aes(y = Timedifference, fill = "", color = ""),position = position_jitter(width = .15), size = .75, alpha = 0.5)+
  geom_boxplot(aes(x = 0.22,fill = ""), notch= T, outlier.shape = NA,  width = .1, colour = "BLACK") +
  scale_x_continuous(breaks=NULL) +
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
  scale_x_continuous(breaks=NULL) +
  scale_y_log10(breaks = c(0, 1, 10, 100, 1000), limits = c(0.5,1000)) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  coord_flip() +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +
  theme_bw() +
  labs(x = "", y = "Value (USD)")+
  theme_rainplot()

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


#Cleanup Receipt Data ----
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
  dplyr::select(litterID, username, user_id, dateprintedcleaned, TimestampDatecleaned, Timedifference, Locationaddresscleaned, Non.Specific.Location.Address, Manual.Lat, Manual.Lon, Locationnamecleaned, Itemized, paymentmethodcleaned, Value, lat, lon)


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
  dplyr::select(litterID, username, user_id, dateprintedcleaned, TimestampDatecleaned, Timedifference, Locationaddresscleaned, Non.Specific.Location.Address, Manual.Lat, Manual.Lon, Locationnamecleaned, Itemized, paymentmethodcleaned, Value, lat, lon)

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
  dplyr::select(litterID, username, user_id, dateprintedcleaned, TimestampDatecleaned, Timedifference, Locationaddresscleaned, Non.Specific.Location.Address, Manual.Lat, Manual.Lon, Locationnamecleaned, Itemized, paymentmethodcleaned, Value, lat, lon)

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




#Notes ----
#Run the code below in sequence to reproduce study analysis. 

#Libraries ----
library(gridExtra)
library(ggplot2)
library(readr)
library(Hmisc)
library(ggrepel)
library(data.table)
library(dplyr)
library(tidyr)
library(swfscMisc)
library(circular)
library(data.tree)
library(plotly)
library(ggforce)

#Functions ----
vector_wind_average <- function(speed, direction){
  complete_values = !is.na(direction) & !is.na(speed)
  
  NS = speed[complete_values] * cos(direction[complete_values]*pi/180)
  EW = speed[complete_values] * sin(direction[complete_values]*pi/180)
  
  TotalNS = sum(NS)
  TotalEW = sum(EW)
  
  atan2(TotalEW, TotalNS)*180/pi
}

aggregate_vector_wind_average <- function(startdate, enddate){
date_range <-  Weather %>%
    dplyr::filter(Date >= as.Date(startdate, "%Y-%m-%d") & Date <= as.Date(enddate, "%Y-%m-%d")) %>%
    dplyr::select(Mean.Wind.Dir..deg., Mean.Wind.Speed..mph.) #%>%
    
    vector_wind_average(date_range$Mean.Wind.Speed..mph., date_range$Mean.Wind.Dir..deg.) %>%
    ifelse(.<0, .+360, .)
}

rainpresabs <- function(startdate, enddate){
  any(Weather %>%
        dplyr::filter(Date >= as.Date(startdate, "%Y-%m-%d") & Date <= as.Date(enddate, "%Y-%m-%d")) %>%
        dplyr::select(Total.Precip..in.) %>%
        pull() > 0, na.rm = T)
}


cleantext <- function(x) {
  tolower(gsub("[[:space:]]", "", x))
}


BootMean <- function(data) {
  B <- 10000
  mean <- numeric(B)
  n = length(data)
  
  set.seed(34347)
  for (i in 1:B) {
    boot <- sample(1:n, size=n, replace = TRUE)
    mean[i] <- mean(data[boot], na.rm = T)
  }
  return(quantile(mean, c(0.025, 0.5, 0.975), na.rm = T))
}

not_all_na <- function(x) any(!is.na(x))

removeslash <- function(x){
  gsub("/", " OR ", x)
}

AggregateTrees <- function(DF, Alias, Hierarchy){
  
  DF <- mutate_all(DF, cleantext) %>%
    mutate(Count = as.numeric(Count))
  
  colnames(DF) <- c("Alias", "Count")
  
  Hierarchy <- mutate_all(Hierarchy, cleantext)
  
  colnames(Hierarchy) <- c("from", "Key")
  
  Alias <- mutate_all(Alias, cleantext) 
  
  DF_v2 <- DF %>%
    left_join(Alias) %>%
    dplyr::select(Key, Count) %>%
    mutate(Key = ifelse(is.na(Key), "other", Key)) %>%
    right_join(Hierarchy) %>%
    select(from, Key, Count) %>%
    add_row(from = "trash", Key = "missing", Count = sum(DF$Count, na.rm = T) - sum(.$Count, na.rm = T))%>%
    mutate(Count = Count/sum(Count, na.rm = T))
  
  
  DF_network <- FromDataFrameNetwork(DF_v2)
  
  DF_network$Do(function(x) x$totalsum <- ifelse(is.null(x$Count), 0, x$Count) + sum(Get(x$children, "totalsum")), traversal = "post-order")
  
  Treedf <- ToDataFrameNetwork(DF_network, "totalsum") 
  
  Treedf %>%
    add_row(from = "trash", to = "", totalsum = sum(Treedf %>%
                                                      filter(from == "trash") %>% 
                                                      pull(totalsum)))
  
  
}

grouped_uncertainty <- function(DF_group, Group_Alias, Group_Hierarchy, type){
  
  groups <- DF_group %>%
    distinct(Name, Day)
  
  df_join = data.frame(from = character(), 
                       to = character(), 
                       totalsum = numeric(), 
                       Name = character(),
                       Day = character())
  
  for(row in 1:nrow(groups)){
    df_subset <- DF_group %>%
      inner_join(groups[row,]) %>%
      select(Class, Count)
    
    df_join <- AggregateTrees(DF = df_subset, Alias = Group_Alias, Hierarchy = Group_Hierarchy) %>%
      mutate(from = ifelse(from == "trash", type, from)) %>%
      mutate(Name = unname(unlist(groups[row,"Name"])), Day = unname(unlist(groups[row,"Day"]))) %>%
      bind_rows(df_join)
  }
  
  df_join_boot <- df_join %>%
    #mutate(node_num = rep(1:27, times = nrow(groups))) %>%
    group_by(from, to) %>%
    summarise(mean_prop = mean(totalsum, na.rm = T), 
              min_prop = BootMean(totalsum)[1], 
              max_prop = BootMean(totalsum)[3])
  

}

sunburstplot <-function(df_join_boot){
  
  values <- paste(df_join_boot$to, 
                  "<br>", 
                  round(df_join_boot$mean_prop, 2) * 100, 
                  " (", 
                  round(df_join_boot$min_prop, 2) * 100, 
                  "-", 
                  round(df_join_boot$max_prop, 2) * 100, 
                  ")%", 
                  sep = "")
  
  values[df_join_boot$mean_prop < 0.1] <- NA
  
  plot_ly() %>%
    add_trace(
      labels = df_join_boot$to,
      parents = df_join_boot$from,
      type = "sunburst",
      maxdepth = 6,
      domain = list(column = 1), 
      branchvalues = 'total',
      texttemplate = values,
      values = df_join_boot$mean_prop) 
}

#Datasets and Cleanup ----

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


mass_of_items <- fread("material_item_site_join_mass.csv")

site_data_cleaned <- fread("StudyAreas/User_Cleaned_Data/reconciled_cleaned.csv") %>%
  left_join(fread("StudyAreas/User_Cleaned_Data/weekend_sweep_manual.csv")) %>%
  mutate(Date = as.Date(Day, format = "%m/%d/%Y")) %>%
  left_join(mass_of_items)
  
#total_distance of roads surveyed
sum(unique(site_data_cleaned$Site_Length_m))


#Check number of lost data points
vals <- site_data_cleaned %>%
  filter(is.na(Lon)) %>%
  mutate(id = as.numeric(id)) %>%
  left_join(full_data, by = c("id" = "Litter"))

nrow(vals)

#Bring in census data for each site. 
census_data <- fread("StudyAreas/Demographic_Site_Data/PDB_2015_Tract.csv")

#check that all sites have a tract id
unique(site_data_cleaned$TractID) %in% unique(census_data$GIDTR)

#cal enviroscreen data. 
enviroscreen <- fread("Enviroscreen/enviroscreen_with_cols.csv")

#filter to inland empire counties and join enviroscreen data. Also calculate population density.
IE_Enviroscreen_Data <- census_data %>%
  filter(County_name == "Riverside County" | County_name == "San Bernardino County") %>%
  mutate(GIDTR = as.numeric(GIDTR)) %>%
  dplyr::select(County_name, LAND_AREA, Tot_Population_CEN_2010, GIDTR) %>%
  mutate(Pop_Density= Tot_Population_CEN_2010/LAND_AREA) %>%  
  inner_join(enviroscreen, by = c("GIDTR" = "tract"))

dataset_enviroscreen <- census_data %>%
  filter(County_name %in% c("Riverside County", "San Bernardino County", "Los Angeles County")) %>%
  mutate(GIDTR = as.numeric(GIDTR)) %>%
  dplyr::select(County_name, LAND_AREA, Tot_Population_CEN_2010, GIDTR) %>%
  mutate(Pop_Density= Tot_Population_CEN_2010/LAND_AREA) %>% 
  inner_join(enviroscreen, by = c("GIDTR" = "tract")) %>%
  filter(GIDTR %in% as.numeric(site_data_cleaned$TractID))

#compare population density for the inland empire to adding los angeles too.
sum(IE_Enviroscreen_Data$Tot_Population_CEN_2010)/sum(IE_Enviroscreen_Data$LAND_AREA)
sum(dataset_enviroscreen$Tot_Population_CEN_2010)/sum(dataset_enviroscreen$LAND_AREA)

#skim mean values for enviroscreen
IE_enviro_skim <- skimr::skim(IE_Enviroscreen_Data)
data_enviro_skim <- skimr::skim(dataset_enviroscreen)

#Phone location uncertainty analysis ----
intersected <- fread("StudyAreas/User_Cleaned_Data/uncertainty/points_inside_areas.csv") %>%
  dplyr::select(id) %>%
  mutate(inside_area = TRUE, id = as.character(id)) %>%
  distinct()

all_distances <- fread("StudyAreas/User_Cleaned_Data/uncertainty/all_points_distance_to_densified_vertices_of_area_meters.csv") %>%
  left_join(intersected) %>%
  mutate(inside_area = ifelse(is.na(inside_area), FALSE, inside_area)) %>%
  mutate(distance_m = ifelse(inside_area, 0, HubDist))

#calculate the location uncertainty value.
BootMean(all_distances$distance_m[!all_distances$inside_area])
mean(all_distances$distance_m[!all_distances$inside_area], na.rm = T)

#Human trip distances, Wind, and Rain ----
##Trip Distances ----
#Dataset source
#https://data.bts.gov/Research-and-Statistics/Trips-by-Distance/w96p-f2qv

#Check summary stats of distances
CompleteDataWithGoogle %>%
  filter(!is.na(DistanceFromLocation)) %>%
  pull(enddate) %>%
  summary()

CompleteDataWithGoogle %>%
  filter(!is.na(DistanceFromLocation))

CompleteDataWithGoogle %>%
  filter(!is.na(startdate))

CompleteDataWithGoogle %>%
  filter(!is.na(DistanceFromLocation) & !is.na(startdate)) %>%
  pull(startdate) 

#Just look at IE trips
IETrips <- Trips %>%
  filter(`State Postal Code` == "CA") %>%
  mutate(DateFormatted = as.Date(Date)) %>%
  filter(DateFormatted < "2019-12-30") %>%
  filter(`County Name` == "Riverside County" | `County Name` == "San Bernardino County")

IEDistribution <- IETrips %>%
  dplyr::select(`Number of Trips`:`Number of Trips >=500`) %>%
  summarise(across(everything(), ~sum(.x, na.rm = T)))

#Reformat names and set distribution estimation values.
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

#Estimate with uniform distribution
montecarlo_vector <- c()
for(row in 1:nrow(pdf_ie_dist)){
  montecarlo_vector <- c(montecarlo_vector, runif(n = unlist(pdf_ie_dist[row, "count"]), min = unlist(pdf_ie_dist[row, "min_dist"]), max = unlist(pdf_ie_dist[row, "max_dist"]))) 
}

#Result doesn't seem to depend much on which of the above we choose. Could also try log uniform, might be a nice way to get to the middle ground.

#compare receipt and human trip distances in meters
p1 <- ggplot() + 
  stat_ecdf(data = CompleteDataWithGoogle, aes(x = DistanceFromLocation), color = "red") + 
  stat_ecdf(aes(x = montecarlo_vector * 1.60934 * 1000), color = "blue") + 
  scale_x_log10(limits = c(1, 10000000), breaks = 10^c(0:7), labels = 10^c(0:7)) +
  geom_text(aes(x = 100, y = 0.50, label = "Receipt Distances"), color = "red", size = 3) +
  geom_text(aes(x = 100000, y = 0.50, label = "Human Trip Distances"), color = "blue", size = 3) +
  theme_bw(base_size = 20) + 
  labs(x = "Distance From Location (m)", y = "Proportion Shorter")
#annotation_custom(ggplotGrob(p2), xmin = 500, xmax = 2000, ymin = 0.5, ymax = 1)

montecarlo_meters = montecarlo_vector * 1.60934 * 1000

receipt_distance_quantiles = quantile(CompleteDataWithGoogle$DistanceFromLocation, probs = seq(0.01, 0.99, by = 0.01), na.rm = T)
montecarlo_distance_quantiles = quantile(montecarlo_meters, probs = seq(0.01, 0.99, by = 0.01), na.rm = T)

# Plot Quantiles against one another. 
p2 <- ggplot() + 
  geom_point(aes(y = receipt_distance_quantiles, x = montecarlo_distance_quantiles)) + 
  geom_smooth(aes(y = receipt_distance_quantiles, x = montecarlo_distance_quantiles),
              method = "lm",
              se = F) + 
  scale_x_log10(breaks= 10^c(1:5)) + 
  scale_y_log10() + 
  theme_classic(base_size = 20) + 
  labs(x = "Human Trip Quantile Distance (m)", y = "Receipt Quantile Distance (m)") + 
  coord_equal() +
  geom_abline(intercept = 0, slope = 1)

#Get summary stats for goodness of fit between the two PDFs
linear_quantile_regression = lm(log10(receipt_distance_quantiles) ~ log10(montecarlo_distance_quantiles))
summary(linear_quantile_regression)

##Difference between nearest location and actual location ----
ggplot(CompleteDataWithGoogle) + stat_ecdf(aes(x = DistanceFromLocation)) + stat_ecdf(aes(x = googledistancemeters), color = "red") + scale_x_continuous()+ theme_classic(base_size = 20, base_line_size = 2) + scale_x_log10() + labs(x = "Meters Traveled per Day")


##Travel Velocity ----
ggplot(CompleteDataWithGoogle, aes(x = DistanceFromLocation/Timedifference)) + geom_histogram(bins = 10) + scale_x_continuous()+ theme_classic(base_size = 20, base_line_size = 2) + scale_x_log10() + labs(x = "Meters Traveled per Day")

###Summary stats----
max(CompleteDataWithGoogle$DistanceFromLocation, na.rm = T)
min(CompleteDataWithGoogle$DistanceFromLocation, na.rm = T)

mean(CompleteDataWithGoogle$DistanceFromLocation/CompleteDataWithGoogle$Timedifference, na.rm =T)
min(CompleteDataWithGoogle$DistanceFromLocation/CompleteDataWithGoogle$Timedifference, na.rm =T)
max(CompleteDataWithGoogle$DistanceFromLocation/CompleteDataWithGoogle$Timedifference, na.rm =T)

quantile(CompleteDataWithGoogle$DistanceFromLocation/CompleteDataWithGoogle$Timedifference, na.rm =T)

transport_speed <- CompleteDataWithGoogle$DistanceFromLocation/CompleteDataWithGoogle$Timedifference

length(transport_speed[!is.na(transport_speed)])
length(CompleteDataWithGoogle$DistanceFromLocation[!is.na(CompleteDataWithGoogle$DistanceFromLocation)])
length(CompleteDataWithGoogle$Timedifference[!is.na(CompleteDataWithGoogle$Timedifference)])

##Rain ----
for(row in 1:nrow(CompleteDataWithGoogle)){
  CompleteDataWithGoogle[row, "precip"] <-rainpresabs(startdate = CompleteDataWithGoogle[row,"dateprintedcleaned"], enddate = CompleteDataWithGoogle[row,"TimestampDatecleaned"])
}

#rain plot how many of the receipts could have experienced precipitation?
p4 <- CompleteDataWithGoogle %>%
  filter(!is.na(dateprintedcleaned) & !is.na(TimestampDatecleaned)) %>%
  group_by(precip) %>%
  summarise(count = n()) %>%
  ggplot() + 
  geom_col(aes(x = precip, y = count)) +
  theme_bw(base_size = 20) +
  labs(x = "Precipitation Occured?", y = "Count")

##Wind ----
for(row in 1:nrow(CompleteDataWithGoogle)){ #This is the inverse of what we would think because wind direction is the inverse. This will tell us what direction the trash came from.
  CompleteDataWithGoogle[row, "trashbearing"] <- swfscMisc::bearing(lat2 = CompleteDataWithGoogle[row,"LocLat"]*pi/180, 
                                                                    lon2 = CompleteDataWithGoogle[row,"LocLon"]*pi/180,
                                                                    lat1 = CompleteDataWithGoogle[row,"lat"]*pi/180, 
                                                                    lon1 = CompleteDataWithGoogle[row,"lon"]*pi/180)[1]
}


#difference between bearings.
for(row in 1:nrow(CompleteDataWithGoogle)){
  CompleteDataWithGoogle[row, "windmeanbearing"] <- aggregate_vector_wind_average(startdate = CompleteDataWithGoogle[row,"startdate"], enddate = CompleteDataWithGoogle[row,"enddate"])
}

###Wind velocity correlation ----

x.circ <- circular(CompleteDataWithGoogle$trashbearing, type="directions", units="degrees", template="geographics", rotation="clock")
rose.diag(x.circ, bins=10, col="gray", border=NA)

y.circ <- circular(CompleteDataWithGoogle$windmeanbearing, type="directions", units="degrees", template="geographics", rotation="clock")
rose.diag(y.circ, bins=10, col="gray", border=NA)

cor.circular(x.circ, y.circ, test = T)

#Calculate the trash and wind bearing x and y coordinates.
CompleteDataWithGoogle$trashycoordbearing <- 0.5*sin(rad(CompleteDataWithGoogle$trashbearing))
CompleteDataWithGoogle$trashxcoordbearing <- 0.5*cos(rad(CompleteDataWithGoogle$trashbearing))
CompleteDataWithGoogle$windycoordmeanbearing <- 1*sin(rad(CompleteDataWithGoogle$windmeanbearing))
CompleteDataWithGoogle$windxcoordmeanbearing <- 1*cos(rad(CompleteDataWithGoogle$windmeanbearing))
CompleteDataWithGoogle$row <- 1:nrow(CompleteDataWithGoogle)

#join the trash bearing data and wind data into single frame for plotting. 
CompleteDataCoordPlot <- CompleteDataWithGoogle %>%
  filter(!is.na(windxcoordmeanbearing) & !is.na(trashxcoordbearing)) %>% #Removes data where we don't have wind or dont have receipt direction from the analysis.
  dplyr::select(windxcoordmeanbearing, windycoordmeanbearing, row) %>%
  rename(x = windxcoordmeanbearing, y = windycoordmeanbearing) %>%
  bind_rows(CompleteDataWithGoogle %>%
              filter(!is.na(windxcoordmeanbearing) & !is.na(trashxcoordbearing)) %>% 
              dplyr::select(trashycoordbearing, trashxcoordbearing, row) %>%
              rename(x = trashycoordbearing, y = trashxcoordbearing)) 


#wind plot 360 (North) is on the right side.
p3 <- ggplot() + 
  geom_circle(aes(x0 = c(0,0), y0 = c(0,0), r = c(0.5,1))) + 
  geom_point(data = CompleteDataCoordPlot, aes(x = x, y = y), alpha = 0.5, size = 2) + 
  geom_line(data = CompleteDataCoordPlot, aes(x = x, y = y, group = row)) +
  theme_void() + 
  theme(aspect.ratio = 1, legend.position = NULL) + 
  geom_text(aes(x = c(0,1.1,0,-1.1), y = c(1.1, 0, -1.1, 0) , label = c("W", "N", "E", "S")), size = 10) 

grid.arrange(p4,p3,p1,p2, ncol = 4)

#Litter Accumulation Rate Changes ----

##Data cleaning ----
input_rate <- site_data_cleaned %>%
  group_by(Date, Name, Site_Length_m, Weekend, Sweeping) %>%
  summarise(Intensity = n()) %>%
  arrange(Date) %>%
  group_by(Name) %>%
  mutate(DateDiff = as.numeric(Date) - dplyr::lag(as.numeric(Date), order_by = Name)) %>%
  mutate(Days_Since_First = as.numeric(Date) - min(as.numeric(Date))) %>%
  mutate(generationrate = Intensity/DateDiff/Site_Length_m) %>%
  mutate(max_norm_generation = scale(generationrate)) %>%
  ungroup() 

input_rate_mass <- site_data_cleaned %>%
  group_by(Date, Name, Site_Length_m, Weekend, Sweeping) %>%
  summarise(Intensity = sum(weigth_estimate_g, na.rm = T)) %>%
  arrange(Date) %>%
  group_by(Name) %>%
  mutate(DateDiff = as.numeric(Date) - dplyr::lag(as.numeric(Date), order_by = Name)) %>%
  mutate(generationrate = Intensity/DateDiff/Site_Length_m) %>%
  ungroup()

#Input Rate Correlation
mean_input_rate <- site_data_cleaned %>%
  mutate(Date = as.Date(Day, format = "%m/%d/%Y")) %>%
  group_by(Date, Name, Site_Length_m, Cal_Enviro_Screen, Road_Width_m, Landuse_Type, TractID) %>%
  summarise(Intensity = n()) %>%
  arrange(Date) %>%
  group_by(Name) %>%
  mutate(DateDiff = as.numeric(Date) - dplyr::lag(as.numeric(Date), order_by = Name)) %>%
  mutate(generationrate = Intensity/DateDiff/Site_Length_m) %>%
  ungroup() %>%
  bind_rows(mutate(., Name = "All")) %>%
  group_by(Name) %>%
  summarise(count = n(), sd = sd(generationrate, na.rm = T), mean = mean(generationrate, na.rm = T), cv = sd(generationrate, na.rm = T)/mean(generationrate, na.rm = T), minmean = BootMean(generationrate)[1], maxmean = BootMean(generationrate)[3]) %>%
  ungroup() %>%
  mutate(count = ifelse(Name == "All", count-8, count-1))

  
#Input Rate Correlation Mass
mean_input_rate_mass <- site_data_cleaned %>%
  mutate(Date = as.Date(Day, format = "%m/%d/%Y")) %>%
  group_by(Date, Name, Site_Length_m, Cal_Enviro_Screen, Road_Width_m, Landuse_Type, TractID) %>%
  summarise(Intensity = sum(weigth_estimate_g, na.rm = T)) %>%
  arrange(Date) %>%
  group_by(Name) %>%
  mutate(DateDiff = as.numeric(Date) - dplyr::lag(as.numeric(Date), order_by = Name)) %>%
  mutate(generationrate = Intensity/DateDiff/Site_Length_m) %>%
  ungroup() %>%
  bind_rows(mutate(., Name = "All")) %>%
  group_by(Name) %>%
  summarise(count = n(), mean = mean(generationrate, na.rm = T), cv = sd(generationrate, na.rm = T)/mean(generationrate, na.rm = T), minmean = BootMean(generationrate)[1], maxmean = BootMean(generationrate)[3]) %>%
  ungroup() 

#Quanitles for all generation rate 
site_data_cleaned %>%
  mutate(Date = as.Date(Day, format = "%m/%d/%Y")) %>%
  group_by(Date, Name, Site_Length_m, Cal_Enviro_Screen, Road_Width_m, Landuse_Type, TractID) %>%
  summarise(Intensity = n()) %>%
  arrange(Date) %>%
  group_by(Name) %>%
  mutate(DateDiff = as.numeric(Date) - dplyr::lag(as.numeric(Date), order_by = Name)) %>%
  mutate(generationrate = Intensity/DateDiff/Site_Length_m) %>%
  ungroup() %>%
  pull(generationrate) %>%
  quantile(na.rm = T)


##plots ----

ggplot() + 
  geom_boxplot(data = input_rate %>%
                 bind_rows(input_rate %>%
                             mutate(Name = "All")), aes(x = Name, y = generationrate)) + 
  geom_errorbar(data = mean_input_rate, aes(x = Name, y = mean, ymin = minmean, ymax = maxmean),color = "red", width = 0.25)+
  geom_point(data = mean_input_rate, aes(x = Name, y = mean), color = "red")+
  geom_text(data = mean_input_rate, aes(x = Name, y = 0.75, label = paste("n=", count, "", sep = "")), size = 5) +
  theme_classic(base_size = 20) + 
  labs(y = "Accumulation Rate (#/Day/m)") + 
  scale_y_log10(limits = c(0.001, 1)) 


ggplot(input_rate, aes(x = Date, y = generationrate)) + 
  geom_smooth(method = "lm", color = "transparent") +
  geom_point(alpha = 0.5) + 
  geom_line(alpha = 0.5) + 
  facet_grid(cols = vars(Name), scales = "free_x", space = "free_x") + 
  #geom_text(aes(x = Date, y = 0.75, label = Intensity), size = 3) +
  theme_classic(base_size = 18) + 
  labs(y = "Accumulation Rate (#/Day/m)", x = "Week of Year") + 
  scale_y_log10(limits = c(0.001, 1)) + 
  scale_x_date(date_breaks = "1 week", date_labels = "%W") 

#Testing if the days since first date impacts the mean trend of the data. 
ggplot(input_rate, aes(x = Days_Since_First, y = max_norm_generation)) + 
  geom_smooth() +
  geom_point(alpha = 0.5) + 
  geom_line(alpha = 0.5) + 
  #facet_grid(cols = vars(Name), scales = "free_x", space = "free_x") + 
  #geom_text(aes(x = Date, y = 0.75, label = Intensity), size = 3) +
  theme_classic(base_size = 18) + 
  labs(y = "Accumulation Rate (#/Day/m)", x = "Days Since First") #+ 
  #scale_y_log10(limits = c(0.001, 1))  

input_rate_2 <- input_rate %>%
  filter(Days_Since_First > 7)

#Testing if the days since first date impacts the mean trend of the data. 
ggplot(input_rate_2, aes(x = Days_Since_First, y = max_norm_generation)) + 
  geom_smooth() +
  geom_point(alpha = 0.5) + 
  geom_line(alpha = 0.5) + 
  #facet_grid(cols = vars(Name), scales = "free_x", space = "free_x") + 
  #geom_text(aes(x = Date, y = 0.75, label = Intensity), size = 3) +
  theme_classic(base_size = 18) + 
  labs(y = "Accumulation Rate (#/Day/m)", x = "Days Since First") #+ 
#scale_y_log10(limits = c(0.001, 1))  

#This is significant for total count but is heavily impacted by the first week which adds leverage. Not significant for normalized. 
model <- lm(input_rate$max_norm_generation ~ input_rate$Days_Since_First)

summary(model)

#This is insignificant but has a similar estimate value. 
model2 <- lm(input_rate_2$max_norm_generation ~ input_rate_2$Days_Since_First)

summary(model2)

ggplot(input_rate, aes(x = Date, y = Name)) + 
  geom_point(alpha = 0.5, size = 4) + 
  geom_text_repel(aes(label = Intensity),
                  size = 4,
                  force_pull   = 0, # do not pull toward data points
                  nudge_y      = 0.5,
                  direction    = "x",
                  angle        = 90,
                  hjust        = 0,
                  segment.size = 0.2,
                  max.iter = 1e4, max.time = 1) +
  theme_classic(base_size = 20) + 
  labs(y = "") + 
  scale_x_date(date_breaks = "2 month")


#Taxonomy Data sets ----

#Trash mass conversion matching. 
material_item_list <- site_data_cleaned %>%
  distinct(Material_TT, Item_TT)

write.csv(material_item_list, "material_item_site.csv")

ItemsHierarchy <- read.csv("Taxonomy/Website/Items_Hierarchy_V2.csv")


BrandHierarchy <- read.csv("Taxonomy/Website/BrandManufacturer.csv") %>%
  rename(to = Brand, from = Manufacturer) 
  
MaterialsHierarchy <- read.csv("Taxonomy/Website/Materials_Hierarchy_V2.csv") 

ItemsAlias <- read.csv("Taxonomy/Website/Items_Alias_V2.csv")%>%
  #mutate(RowID = 1:nrow(.)) %>%
  rename(Key = Item)

MaterialsAlias <- read.csv("Taxonomy/Website/Materials_Alias_V2.csv") %>%
  #mutate(RowID = 1:nrow(.)) %>%
  rename(Key = Material)

##Data Processing ----

Material_DF <- site_data_cleaned %>%
  mutate_all(removeslash) %>%
  group_by(Material_TT) %>%
  summarise(Count = n()) %>%
  ungroup()

Material_DF_group <- site_data_cleaned %>%
  mutate_all(removeslash) %>%
  group_by(Material_TT, Name, Day) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  rename(Class = Material_TT)


Item_DF <- site_data_cleaned %>%
  mutate_all(removeslash) %>%
  group_by(Item_TT) %>%
  summarise(Count = n()) %>%
  ungroup() 


Item_DF_group <- site_data_cleaned %>%
  mutate_all(removeslash) %>%
  group_by(Item_TT, Name, Day) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  rename(Class = Item_TT)

Brand_DF_group <- site_data_cleaned %>%
  mutate_all(removeslash) %>%
  group_by(Brand_TT, Name, Day) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  rename(Class = Brand_TT)

#want this to be zero unmatches. 
unmatched <- Item_DF %>%
  mutate_all(cleantext) %>%
  anti_join(ItemsAlias %>%
              mutate_all(cleantext), by = c("Item_TT" = "Alias"))

##Material taxonomy ----
MaterialTreeDF <- AggregateTrees(DF = Material_DF, Alias = MaterialsAlias, Hierarchy = MaterialsHierarchy) %>%
  mutate(from = ifelse(from == "trash", "Materials", from))

material_grouped <- grouped_uncertainty(DF_group = Material_DF_group, Group_Alias = MaterialsAlias, Group_Hierarchy = MaterialsHierarchy, type = "Materials")

material_grouped_test <- material_grouped %>%
  filter(mean_prop > 0.1)

MaterialsPlot <- sunburstplot(df_join_boot = material_grouped)

orca(MaterialsPlot, "material-plot.svg")

##Item taxonomy ----
ItemTreeDF <- AggregateTrees(DF = Item_DF, Alias = ItemsAlias, Hierarchy = ItemsHierarchy) %>%
  mutate(from = ifelse(from == "trash", "Items", from))

#Item prop uncertainty
item_grouped <- grouped_uncertainty(DF_group = Item_DF_group, Group_Alias = ItemsAlias, Group_Hierarchy = ItemsHierarchy, type = "Items")

item_plot <- sunburstplot(df_join_boot = item_grouped)
orca(item_plot, "item-plot.svg")

##Brand tree df ----

days_sites_exist <- site_data_cleaned %>%
  distinct(Name, Day)

zero_values_brands <- expand.grid(to = iconv(unique(site_data_cleaned$Brand_TT),  from = 'UTF-8', to = 'ASCII//TRANSLIT'), from = iconv(unique(site_data_cleaned$Manufacturer), from = 'UTF-8', to = 'ASCII//TRANSLIT'), Name = unique(site_data_cleaned$Name), Day = unique(site_data_cleaned$Day)) %>%
  inner_join(days_sites_exist)

Brand_DF_group <- site_data_cleaned %>%
  mutate(Manufacturer = ifelse(Manufacturer == "other" & Brand_TT != "", "Unmerged", ifelse(Manufacturer == "other" & Brand_TT == "", "Unbranded", Manufacturer))) %>%
  group_by(Manufacturer, Brand_TT, Name, Day) %>%
  summarize(totalsum = n()) %>%
  ungroup() %>%
  group_by(Name, Day) %>%
  mutate(totalsum_value = sum(totalsum)) %>%
  ungroup() %>%
  mutate(totalsum = totalsum/totalsum_value) %>%
  rename(to = Brand_TT, from = Manufacturer) %>%
  mutate(from = iconv(from, from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>%
  mutate(to = iconv(to, from = 'UTF-8', to = 'ASCII//TRANSLIT'))%>%
  full_join(zero_values_brands) %>%
  mutate(totalsum = ifelse(is.na(totalsum), 0, totalsum))

#From this it looks like zeros are being propogated to days where they don't belong since there is really high uncertainty even for unbranded 
#something like this probably works to summarize the from column only. 
brand_grouped <- Brand_DF_group %>%
    group_by(from, Name, Day) %>%
    summarise(totalsum = sum(totalsum)) %>%
    ungroup() %>%
    group_by(from) %>%
    summarise(mean_prop = mean(totalsum, na.rm = T), 
            min_prop = BootMean(totalsum)[1], 
            max_prop = BootMean(totalsum)[3]) %>%
    ungroup() %>%
    rename(to = from) %>% 
    mutate(from = "Brands") %>%
    add_row(from = "Brands", 
            to = "", 
            mean_prop = sum(filter(., from == "Brands") %>% pull(mean_prop)))
  
brand_plot <- sunburstplot(brand_grouped)
orca(brand_plot, "brand-plot.svg")
#Unbranded things show us the importance of creating forensic labels, Unmerged things show us the importance of data science research in this field to build relational databases. Brands show us the minimum level of responsibility that these companies have for their environmental impact. Propose a fine on companies which is concomittant with their observed environmental impact and subsequent cleanup. Right now we are giving these companies subsidies on their pollution by paying for cleanup. 3% for philip moris and so on. We are incentivising their pollution by reinforcing the idea that cigarettes will just disappear when they enter the environment. This is the largest pollution we have ever seen since the BP oil spill and their should be a similar reponse from governments to end the pollution of our envioronment and hold the companies responsible.  

#Power analysis ----
#Power Analysis for detecting % shift in future studies

#How many samples do we need to detect a X% shift?
for(n in 1:length(mean_input_rate$mean)){
  power_count_10 <- power.t.test(delta = mean_input_rate$mean[n] - 0.1*mean_input_rate$mean[n], sd = mean_input_rate$sd[n], sig.level = 0.05, power = 0.8)
  print(mean_input_rate$Name[n])
  print(power_count_10$n)
}

#How much power did we have to detect a X% reduction
for(n in 1:length(mean_input_rate$mean)){
  power_count_10 <- power.t.test(n = mean_input_rate$count[n], delta = mean_input_rate$mean[n] - 0.6*mean_input_rate$mean[n], sd = mean_input_rate$sd[n], sig.level = 0.05)
  print(mean_input_rate$Name[n])
  print(power_count_10$power)
}

#What size of effect (% change in mean) could we have detected with the power we have?
for(n in 1:length(mean_input_rate$mean)){
  power_count_10 <- power.t.test(n = mean_input_rate$count[n], sd = mean_input_rate$sd[n], sig.level = 0.05, power = 0.8)
  print(mean_input_rate$Name[n])
  print(power_count_10$delta / mean_input_rate$mean[n])
}

#What size of effect cohens D could we have detected with the power we have? 0.5 is medium, 0.2 is small, 0.8 is large
for(n in 1:length(mean_input_rate$mean)){
  power_count_10 <- power.t.test(n = mean_input_rate$count[n], sd = mean_input_rate$sd[n], sig.level = 0.05, power = 0.8)
  print(mean_input_rate$Name[n])
  print(power_count_10$delta / mean_input_rate$sd[n])
}

#sessioninfo ----
sessioninfo <- sessionInfo()
saveRDS(sessioninfo, "sessioninfo.rds")
readRDS("sessioninfo.rds")

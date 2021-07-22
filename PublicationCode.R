#Notes ----
#Check out this wind data https://mrcc.illinois.edu/CLIMATE/Hourly/WindRose.jsp

#Libraries ----
library(readxl)
library(gridExtra)
#library(dplyr)
library(ggplot2)
library(readr)
library(lavaan)
library(smooth)
library(Hmisc)
library(ggrepel)
#library(stormwindmodel)
library(data.table)
library(dplyr)
library(tidyr)
#library(fitdistrplus) #https://cran.r-project.org/web/packages/fitdistrplus/vignettes/paper2JSS.pdf
library(swfscMisc)
library(circular)
library(geosphere)
#devtools::install_github(repo = "michaelmalick/r-malick")
#library(malick)
library(data.tree)
library(plotly)


#Functions ----
#Function returns the difference between two bearings
vector_wind_average <- function(speed, direction){
  complete_values = !is.na(direction) & !is.na(speed)
  
  NS = speed[complete_values] * cos(direction[complete_values]*pi/180)
  EW = speed[complete_values] * sin(direction[complete_values]*pi/180)
  
  TotalNS = sum(NS)
  TotalEW = sum(EW)
  
  atan2(TotalEW, TotalNS)*180/pi
}

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
date_range <-  Weather %>%
    dplyr::filter(Date >= as.Date(startdate, "%Y-%m-%d") & Date <= as.Date(enddate, "%Y-%m-%d")) %>%
    dplyr::select(Mean.Wind.Dir..deg., Mean.Wind.Speed..mph.) #%>%
    
    vector_wind_average(date_range$Mean.Wind.Speed..mph., date_range$Mean.Wind.Dir..deg.) %>%
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

#requires alias to have Alias, RowID and Key columns, requires DF to have a count column and some column to match
jointohierarchy <- function(DF, Hierarchy, Alias, ColNum) {
  DF <- mutate_all(DF, cleantext) %>%
    mutate(Count = as.numeric(Count))
  Hierarchy <- mutate_all(Hierarchy, cleantext)
  Alias <- mutate_all(Alias, cleantext) 
  
  DF$RowID <- unlist(apply(DF, 1, function(x) which(Alias == as.character(x[ColNum]), arr.ind = TRUE)[1]))
  
  DF <- DF %>%
    left_join(dplyr::select(Alias, Key)) %>%
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



#check out this: https://stackoverflow.com/questions/45225671/aggregating-values-on-a-data-tree-with-r
myApply <- function(node) {
  node$totalsum <- 
    sum(c(node$Count, purrr::map_dbl(node$children, myApply)), na.rm = TRUE)
}



removeslash <- function(x){
  gsub("/", " OR ", x)
}
#DF2 <- SMC
#Hierarchy <- ItemsHierarchy
#Alias <- ItemsAlias
#ColNum <- 2


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
      #text = ~paste('</br> Mean: ', round(df_join_boot$mean_prop, 2),
      #              '</br> Min: ', round(df_join_boot$min_prop, 2),
      #              '</br> Max: ', round(df_join_boot$max_prop, 2)),
      #textinfo = "label+percent entry",
      texttemplate = values,
      values = df_join_boot$mean_prop) #%>%
   # layout(uniformtext=list(minsize=18, mode = "show"))
}
  

#Datasets ----

#source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")


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

mass_of_items <- fread("material_item_site_join_mass.csv")

site_data_cleaned <- fread("StudyAreas/User_Cleaned_Data/reconciled_cleaned.csv") %>%
  left_join(fread("StudyAreas/User_Cleaned_Data/weekend_sweep_manual.csv")) %>%
  mutate(Date = as.Date(Day, format = "%m/%d/%Y")) %>%
  left_join(mass_of_items)
  
census_data <- fread("StudyAreas/Demographic_Site_Data/PDB_2015_Tract.csv")

unique(site_data_cleaned$TractID) %in% unique(census_data$GIDTR)

enviroscreen <- fread("Enviroscreen/enviroscreen_with_cols.csv")

IE_Enviroscreen_Data <- census_data %>%
  filter(County_name == "Riverside County" | County_name == "San Bernardino County") %>%
  mutate(GIDTR = as.numeric(GIDTR)) %>%
  dplyr::select(County_name, LAND_AREA, Tot_Population_CEN_2010, GIDTR) %>%
  mutate(Pop_Density= Tot_Population_CEN_2010/LAND_AREA) %>%  #mutate(GIDTR = as.numeric(GIDTR)) %>%
  inner_join(enviroscreen, by = c("GIDTR" = "tract"))

dataset_enviroscreen <- census_data %>%
  filter(County_name %in% c("Riverside County", "San Bernardino County", "Los Angeles County")) %>%
  #dplyr::select(County_name, GIDTR) %>%
  mutate(GIDTR = as.numeric(GIDTR)) %>%
  dplyr::select(County_name, LAND_AREA, Tot_Population_CEN_2010, GIDTR) %>%
  mutate(Pop_Density= Tot_Population_CEN_2010/LAND_AREA) %>% 
  inner_join(enviroscreen, by = c("GIDTR" = "tract")) %>%
  filter(GIDTR %in% as.numeric(site_data_cleaned$TractID))

sum(IE_Enviroscreen_Data$Tot_Population_CEN_2010)/sum(IE_Enviroscreen_Data$LAND_AREA)
sum(dataset_enviroscreen$Tot_Population_CEN_2010)/sum(dataset_enviroscreen$LAND_AREA)

#skim mean values for enviroscreen
IE_enviro_skim <- skimr::skim(IE_Enviroscreen_Data)
data_enviro_skim <- skimr::skim(dataset_enviroscreen)


#point uncertainty analysis ----
intersected <- fread("StudyAreas/User_Cleaned_Data/uncertainty/points_inside_areas.csv") %>%
  dplyr::select(id) %>%
  mutate(inside_area = TRUE, id = as.character(id)) %>%
  distinct()

all_distances <- fread("StudyAreas/User_Cleaned_Data/uncertainty/all_points_distance_to_densified_vertices_of_area_meters.csv") %>%
  left_join(intersected) %>%
  mutate(inside_area = ifelse(is.na(inside_area), FALSE, inside_area)) %>%
  mutate(distance_m = ifelse(inside_area, 0, HubDist))

hist(all_distances$distance_m)
BootMean(all_distances$distance_m)
BootMean(all_distances$distance_m[!all_distances$inside_area])
mean(all_distances$distance_m[!all_distances$inside_area], na.rm = T)
summary(all_distances$distance_m)

#Dataset source
#https://data.bts.gov/Research-and-Statistics/Trips-by-Distance/w96p-f2qv
#Trash mass conversion matching. 
material_item_list <- site_data_cleaned %>%
  distinct(Material_TT, Item_TT)

write.csv(material_item_list, "material_item_site.csv")

#Generation Rate Changes ----

##Data cleaning ----
input_rate <- site_data_cleaned %>%
  #filter(user_id == 92684) %>%
  #mutate(Date = substr(photo_timestamp,1,nchar(photo_timestamp)-3)) %>%
  group_by(Date, Name, Site_Length_m, Weekend, Sweeping) %>%
  summarise(Intensity = n()) %>%
  arrange(Date) %>%
  group_by(Name) %>%
  mutate(DateDiff = as.numeric(Date) - dplyr::lag(as.numeric(Date), order_by = Name)) %>%
  mutate(generationrate = Intensity/DateDiff/Site_Length_m) %>%
  #filter(Date != as.Date("3/23/2020", format = "%m/%d/%Y")) %>% #Bring back in for 2020 analysis.
  ungroup() 

input_rate_mass <- site_data_cleaned %>%
  #filter(user_id == 92684) %>%
  #mutate(Date = substr(photo_timestamp,1,nchar(photo_timestamp)-3)) %>%
  group_by(Date, Name, Site_Length_m, Weekend, Sweeping) %>%
  summarise(Intensity = sum(weigth_estimate_g, na.rm = T)) %>%
  arrange(Date) %>%
  group_by(Name) %>%
  mutate(DateDiff = as.numeric(Date) - dplyr::lag(as.numeric(Date), order_by = Name)) %>%
  mutate(generationrate = Intensity/DateDiff/Site_Length_m) %>%
  #filter(Date != as.Date("3/23/2020", format = "%m/%d/%Y")) %>% #Bring back in for 2020 analysis.
  ungroup()

weekend_sweeping <- input_rate %>%
  mutate(type = ifelse(Weekend == "Y", "Weekend", "Non-Weekend")) %>%
  bind_rows(input_rate %>%
              filter(Name != "Site 6") %>%
              mutate(type = ifelse(Sweeping == "Y", "Sweeping", "Non-Sweeping"))
  ) %>%
  group_by(Name, type) %>%
  summarise(count = n(), mean = mean(generationrate, na.rm = T), cv = sd(generationrate, na.rm = T)/mean(generationrate, na.rm = T), minmean = BootMean(generationrate)[1], maxmean = BootMean(generationrate)[3])

#Input Rate Correlation
mean_input_rate <- site_data_cleaned %>%
  #filter(user_id == 92684) %>%
  #mutate(Date = substr(photo_timestamp,1,nchar(photo_timestamp)-3)) %>%
  mutate(Date = as.Date(Day, format = "%m/%d/%Y")) %>%
  group_by(Date, Name, Site_Length_m, Cal_Enviro_Screen, Road_Width_m, Landuse_Type, TractID) %>%
  summarise(Intensity = n()) %>%
  arrange(Date) %>%
  group_by(Name) %>%
  mutate(DateDiff = as.numeric(Date) - dplyr::lag(as.numeric(Date), order_by = Name)) %>%
  mutate(generationrate = Intensity/DateDiff/Site_Length_m) %>%
  #filter(Date != as.Date("3/23/2020", format = "%m/%d/%Y")) %>% #Bring back in for 2020 analysis.
  ungroup() %>%
  bind_rows(mutate(., Name = "All")) %>%
  group_by(Name) %>%
  summarise(count = n(), mean = mean(generationrate, na.rm = T), cv = sd(generationrate, na.rm = T)/mean(generationrate, na.rm = T), minmean = BootMean(generationrate)[1], maxmean = BootMean(generationrate)[3]) %>%
  ungroup()  

#Input Rate Correlation Mass
mean_input_rate_mass <- site_data_cleaned %>%
  #filter(user_id == 92684) %>%
  #mutate(Date = substr(photo_timestamp,1,nchar(photo_timestamp)-3)) %>%
  mutate(Date = as.Date(Day, format = "%m/%d/%Y")) %>%
  group_by(Date, Name, Site_Length_m, Cal_Enviro_Screen, Road_Width_m, Landuse_Type, TractID) %>%
  summarise(Intensity = sum(weigth_estimate_g, na.rm = T)) %>%
  arrange(Date) %>%
  group_by(Name) %>%
  mutate(DateDiff = as.numeric(Date) - dplyr::lag(as.numeric(Date), order_by = Name)) %>%
  mutate(generationrate = Intensity/DateDiff/Site_Length_m) %>%
  #filter(Date != as.Date("3/23/2020", format = "%m/%d/%Y")) %>% #Bring back in for 2020 analysis.
  ungroup() %>%
  bind_rows(mutate(., Name = "All")) %>%
  group_by(Name) %>%
  summarise(count = n(), mean = mean(generationrate, na.rm = T), cv = sd(generationrate, na.rm = T)/mean(generationrate, na.rm = T), minmean = BootMean(generationrate)[1], maxmean = BootMean(generationrate)[3]) %>%
  ungroup() 

#Quanitles for all generation rate 
site_data_cleaned %>%
  #filter(user_id == 92684) %>%
  #mutate(Date = substr(photo_timestamp,1,nchar(photo_timestamp)-3)) %>%
  mutate(Date = as.Date(Day, format = "%m/%d/%Y")) %>%
  group_by(Date, Name, Site_Length_m, Cal_Enviro_Screen, Road_Width_m, Landuse_Type, TractID) %>%
  summarise(Intensity = n()) %>%
  arrange(Date) %>%
  group_by(Name) %>%
  mutate(DateDiff = as.numeric(Date) - dplyr::lag(as.numeric(Date), order_by = Name)) %>%
  mutate(generationrate = Intensity/DateDiff/Site_Length_m) %>%
  #filter(Date != as.Date("3/23/2020", format = "%m/%d/%Y")) %>% #Bring back in for 2020 analysis.
  ungroup() %>%
  pull(generationrate) %>%
  quantile(na.rm = T)


##plots ----
ggplot(weekend_sweeping, aes(group = type, color = type)) + 
  geom_errorbar(aes(x = Name, y = mean, ymin = minmean, ymax = maxmean), position = position_dodge(width = 0.8))+
  geom_point(aes(x = Name, y = mean),position = position_dodge(width = 0.8))+
  geom_text(aes(x = Name, y = 0.75, label = paste("n=", count, "", sep = "")), position = position_dodge(width = 0.8), size = 4) +  
  scale_y_log10(limits = c(0.001, 1)) + 
  scale_color_viridis_d() + 
  theme_bw(base_size = 20) + 
  theme(legend.title = element_blank()) + 
  labs(x = "", y = "Generation Rate (#/Day/m)")

ggplot() + 
  geom_boxplot(data = input_rate %>%
                 bind_rows(input_rate %>%
                             mutate(Name = "All")), aes(x = Name, y = generationrate)) + 
  geom_errorbar(data = mean_input_rate, aes(x = Name, y = mean, ymin = minmean, ymax = maxmean),color = "red", width = 0.25)+
  geom_point(data = mean_input_rate, aes(x = Name, y = mean), color = "red")+
  geom_text(data = mean_input_rate, aes(x = Name, y = 0.75, label = paste("n=", count, "", sep = "")), size = 5) +
  theme_classic(base_size = 20) + 
  labs(y = "Generation Rate (#/Day/m)") + 
  scale_y_log10(limits = c(0.001, 1)) 


ggplot() + 
  geom_boxplot(data = input_rate_mass %>%
                 bind_rows(input_rate_mass %>%
                             mutate(Name = "All")), aes(x = Name, y = generationrate)) + 
  geom_errorbar(data = mean_input_rate_mass, aes(x = Name, y = mean, ymin = minmean, ymax = maxmean),color = "red", width = 0.25)+
  geom_point(data = mean_input_rate_mass, aes(x = Name, y = mean), color = "red")+
  geom_text(data = mean_input_rate_mass, aes(x = Name, y = 0.75, label = paste("n=", count, "", sep = "")), size = 5) +
  theme_bw(base_size = 20) + 
  labs(y = "Generation Rate (g/Day/m)") + 
  scale_y_log10() 

ggplot(input_rate, aes(x = Date, y = generationrate)) + 
  geom_point(alpha = 0.5) + 
  geom_line(alpha = 0.5) + 
  facet_grid(cols = vars(Name), scales = "free_x", space = "free_x") + 
  #geom_text(aes(x = Date, y = 0.75, label = Intensity), size = 3) +
  theme_classic(base_size = 18) + 
  labs(y = "Generation Rate (#/Day/m)", x = "Week of Year") + 
  scale_y_log10(limits = c(0.001, 1)) + 
  scale_x_date(date_breaks = "1 week", date_labels = "%W") 
  
  
ggplot(input_rate_mass, aes(x = Date, y = generationrate)) + 
  geom_point(alpha = 0.5, width = 1) + 
  geom_line(alpha = 0.5, width = 1) + 
  facet_grid(Name ~., scales = "free", ncol = 1) + 
  geom_text(aes(x = Date, y = 10, label = round(Intensity, 0)), size = 3) +
  theme_bw(base_size = 18) + 
  labs(y = "Generation Rate (g/Day/m)", x = "Week of Year") + 
  scale_y_log10() + 
  scale_x_date(date_breaks = "1 week", labels = NULL)

ggplot(input_rate, aes(x = Date, y = Name)) + 
  geom_point(alpha = 0.5, size = 4) + 
  geom_text_repel(aes(label = Intensity),
                  size = 6,
                  force_pull   = 0, # do not pull toward data points
                  nudge_y      = 0.5,
                  direction    = "x",
                  angle        = 90,
                  hjust        = 0,
                  segment.size = 0.2,
                  max.iter = 1e4, max.time = 1) +
  #geom_smooth(method = "lm") +
  #facet_wrap(Name ~., scales = "free") + 
  theme_classic(base_size = 20) + 
  labs(y = "") + 
  scale_x_date(date_breaks = "2 month")


mean_for_corrleation <- site_data_cleaned %>%
  mutate(Date = as.Date(Day, format = "%m/%d/%Y")) %>%
  group_by(Date, Name, Site_Length_m, Cal_Enviro_Screen, Road_Width_m, Landuse_Type, TractID) %>%
  summarise(Intensity = n()) %>%
  arrange(Date) %>%
  group_by(Name) %>%
  mutate(DateDiff = as.numeric(Date) - dplyr::lag(as.numeric(Date), order_by = Name)) %>%
  mutate(generationrate = Intensity/DateDiff/Site_Length_m) %>%
  ungroup() %>%
  group_by(Name, TractID, Cal_Enviro_Screen, Road_Width_m, Landuse_Type) %>%
  summarise(mean = mean(generationrate, na.rm = T), cv = sd(generationrate, na.rm = T)/mean(generationrate, na.rm = T), minmean = BootMean(generationrate)[1], maxmean = BootMean(generationrate)[3]) %>%
  ungroup() %>%
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
  mutate(TractID = as.numeric(TractID)) %>%
  left_join(enviroscreen, by = c("TractID" = "tract")) %>%
  left_join(census_data %>%
              mutate(GIDTR = as.numeric(GIDTR)) %>%
              select(LAND_AREA, Tot_Population_CEN_2010, GIDTR), by = c("TractID" = "GIDTR")) %>%
  mutate(Pop_Density= Tot_Population_CEN_2010/LAND_AREA)
  #gather(key, value, -mean) %>%
  #mutate(value = as.numeric(value))
  
mean_for_corrleation_numeric <- mean_for_corrleation %>%
  select_if(is.numeric)

generationcor <- stats::cor(mean_for_corrleation_numeric, method = "spearman")

mean_for_corrleation_numeric %>%
  gather(key = "variable", value = "value", -mean, -minmean, -maxmean) %>%
ggplot(aes(y = mean, x = value)) +
  geom_point() +
  geom_errorbar(aes(ymin = minmean, ymax = maxmean)) + 
  scale_y_log10() + 
  facet_wrap(~variable, scales = "free")+
  theme_bw(base_size = 10)

#Material Types By Site and Date
material_composition <- site_data_cleaned %>%
  group_by(Date, Name, Material_TT) %>%
  summarise(Intensity = n()) %>%
  arrange(Name, Intensity) %>%
  ungroup()

ggplot(arrange(material_composition, Date, Name, Intensity), aes(fill=Material_TT, y=Intensity, x=Date)) + 
  geom_bar(position="fill", stat="identity") +
  facet_wrap(Name ~.,strip.position =  "right", scales = "free_x", ncol = 1) + 
  theme_bw()
#Recognize critically that every day of the week at every site, plastic is the prevailing material type. 

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
  mutate(evenness = shannon/log(numgroups)) %>%
  ungroup()
 

material_diversity_boot <- site_data_cleaned %>%
  #filter(user_id == 92684) %>%
  #mutate(Date = substr(photo_timestamp,1,nchar(photo_timestamp)-3)) %>%
  mutate(Date = as.Date(Day, format = "%m/%d/%Y")) %>%
  group_by(Date, Name) %>%
  dplyr::summarise(shannon = calc_shannon(Material_TT),
                   simpson = calc_simpson(Material_TT),
                   menhincks = calc_menshinicks(Material_TT),
                   numgroups = calc_numgroups(Material_TT)) %>%
  mutate(evenness = shannon/log(numgroups)) %>%
  ungroup() %>%
  group_by(Name) %>%
  summarise(mean_even = mean(evenness, na.rm = T), min_even = BootMean(evenness)[1], max_even = BootMean(evenness)[3], mean_numgroups = mean(numgroups, na.rm = T), min_numgroups = BootMean(numgroups)[1], max_numgroups = BootMean(numgroups)[3]) %>%
  ungroup()%>%
  mutate(type = "Material")

ggplot(material_diversity_boot, aes(x = mean_even, y = mean_numgroups, color = Name, label = Name)) +
  geom_point() +
  geom_errorbar(width=.1, aes(ymin=min_numgroups, ymax=max_numgroups)) +
  geom_errorbar(width=.1, aes(xmin=min_even, xmax=max_even)) +
  geom_label() +
  scale_color_viridis_d(option = "C") + 
  theme_classic()

#ItemDiversity Boot
item_diversity_boot <- site_data_cleaned %>%
  mutate(Date = as.Date(Day, format = "%m/%d/%Y")) %>%
  group_by(Date, Name) %>%
  dplyr::summarise(shannon = calc_shannon(Item_TT),
                   simpson = calc_simpson(Item_TT),
                   menhincks = calc_menshinicks(Item_TT),
                   numgroups = calc_numgroups(Item_TT)) %>%
  mutate(evenness = shannon/log(numgroups)) %>%
  ungroup() %>%
  group_by(Name) %>%
  summarise(mean_even = mean(evenness, na.rm = T), min_even = BootMean(evenness)[1], max_even = BootMean(evenness)[3], mean_numgroups = mean(numgroups, na.rm = T), min_numgroups = BootMean(numgroups)[1], max_numgroups = BootMean(numgroups)[3]) %>%
  ungroup() %>%
  mutate(type = "Item")

ggplot(item_diversity_boot, aes(x = mean_even, y = mean_numgroups, color = Name, label = Name)) +
  geom_point() +
  geom_errorbar(width=.1, aes(ymin=min_numgroups, ymax=max_numgroups)) +
  geom_errorbar(width=.1, aes(xmin=min_even, xmax=max_even)) +
  geom_label() +
  scale_color_viridis_d(option = "C") + 
  theme_classic()

#ItemDiversity Boot
brand_diversity_boot <- site_data_cleaned %>%
  #filter(user_id == 92684) %>%
  #mutate(Date = substr(photo_timestamp,1,nchar(photo_timestamp)-3)) %>%
  mutate(Date = as.Date(Day, format = "%m/%d/%Y")) %>%
  group_by(Date, Name) %>%
  dplyr::summarise(shannon = calc_shannon(Manufacturer),
                   simpson = calc_simpson(Manufacturer),
                   menhincks = calc_menshinicks(Manufacturer),
                   numgroups = calc_numgroups(Manufacturer)) %>%
  mutate(evenness = shannon/log(numgroups)) %>%
  ungroup() %>%
  group_by(Name) %>%
  summarise(mean_even = mean(evenness, na.rm = T), min_even = BootMean(evenness)[1], max_even = BootMean(evenness)[3], mean_numgroups = mean(numgroups, na.rm = T), min_numgroups = BootMean(numgroups)[1], max_numgroups = BootMean(numgroups)[3]) %>%
  ungroup()%>%
  mutate(type = "Brand")

ggplot(brand_diversity_boot, aes(x = mean_even, y = mean_numgroups, color = Name, label = Name)) +
  geom_point() +
  geom_errorbar(width=.1, aes(ymin=min_numgroups, ymax=max_numgroups)) +
  geom_errorbar(width=.1, aes(xmin=min_even, xmax=max_even)) +
  geom_label() +
  scale_color_viridis_d(option = "C") + 
  theme_classic()
#Evenness goes down compared to the other analyses, also less diverse than items. 


joined_diversity <- bind_rows(material_diversity_boot, item_diversity_boot, brand_diversity_boot) %>%
  mutate(type = factor(type, levels = c("Material", "Item", "Brand")))


ggplot(joined_diversity, aes(x = mean_even, y = mean_numgroups, color = Name, label = Name)) +
  geom_point() +
  geom_errorbar(width=.05, aes(ymin=min_numgroups, ymax=max_numgroups)) +
  geom_errorbar(width=.05, aes(xmin=min_even, xmax=max_even)) +
  #geom_label(size = 0.5) +
  scale_color_viridis_d(option = "C") + 
  theme_classic() + 
  facet_wrap(type~.)



ggplot(material_diversity) + 
  geom_point(aes(x = Date, y = shannon)) + 
  geom_point(aes(x = Date, y = simpson), color = "red") + 
  geom_point(aes(x = Date, y = menhincks), color = "blue") + 
  geom_point(aes(x = Date, y = numgroups), color = "green") +
  facet_wrap(Name ~., scales = "free") + 
  theme_bw() + 
  labs(y = "Diversity") + 
  scale_x_date(date_minor_breaks = "1 week")


#Taxonomy Data sets ----
ItemsHierarchy <- read.csv("Taxonomy/Website/Items_Hierarchy_V2.csv")

#Test items not matched 

BrandHierarchy <- read.csv("Taxonomy/Website/BrandManufacturer.csv") %>%
  rename(to = Brand, from = Manufacturer) 
  
MaterialsHierarchy <- read.csv("Taxonomy/Website/Materials_Hierarchy_V2.csv") 

ItemsAlias <- read.csv("Taxonomy/Website/Items_Alias_V2.csv")%>%
  #mutate(RowID = 1:nrow(.)) %>%
  rename(Key = Item)

MaterialsAlias <- read.csv("Taxonomy/Website/Materials_Alias_V2.csv") %>%
  #mutate(RowID = 1:nrow(.)) %>%
  rename(Key = Material)

#Data Processing ----

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

##Item taxonoy ----
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


plot_ly() %>%
  add_trace(
    labels = material_grouped$to,
    parents = material_grouped$from,
    type = 'sunburst',
    maxdepth = 6,
    domain = list(column = 0), 
    branchvalues = 'total',
    texttemplate = paste(material_grouped$to, 
                         "<br>", 
                         round(material_grouped$mean_prop, 2) * 100, 
                         " (", 
                         round(material_grouped$min_prop, 2) * 100, 
                         "-", 
                         round(material_grouped$max_prop, 2) * 100, 
                         ")%", 
                         sep = ""),
    values = material_grouped$mean_prop) %>%
  add_trace(
    labels = item_grouped$to,
    parents = item_grouped$from,
    type = 'sunburst',
    maxdepth = 6,
    domain = list(column = 1), 
    branchvalues = 'total',
    texttemplate = paste(item_grouped$to, 
                         "<br>", 
                         round(item_grouped$mean_prop, 2) * 100, 
                         " (", 
                         round(item_grouped$min_prop, 2) * 100, 
                         "-", 
                         round(item_grouped$max_prop, 2) * 100, 
                         ")%", 
                         sep = ""),
    values = item_grouped$mean_prop) %>%
  add_trace(
    labels = brand_grouped$to,
    parents = brand_grouped$from,
    type = 'sunburst',
    maxdepth = 6,
    domain = list(column = 2), 
    branchvalues = 'total',
    texttemplate = paste(brand_grouped$to, 
                         "<br>", 
                         round(brand_grouped$mean_prop, 2) * 100, 
                         " (", 
                         round(brand_grouped$min_prop, 2) * 100, 
                         "-", 
                         round(brand_grouped$max_prop, 2) * 100, 
                         ")%", 
                         sep = ""),
    values = brand_grouped$mean_prop) %>%
  layout(
    grid = list(columns =3, rows = 1),
    margin = list(l = 0.1, r = 0.1, b = 0.1, t = 0.1),
    sunburstcolorway = c(
      "#636efa","#EF553B","#00cc96","#ab63fa","#19d3f3",
      "#e763fa", "#FECB52","#FFA15A","#FF6692","#B6E880"
    ),
    extendsunburstcolors = TRUE)
 
#try this https://stackoverflow.com/questions/58173316/plotly-sunburst-plot-not-showing-in-jupyter-notebook

#Unbranded things show us the importance of creating forensic labels, Unmerged things show us the importance of data science research in this field to build relational databases. Brands show us the minimum level of responsibility that these companies have for their environmental impact. Propose a fine on companies which is concomittant with their observed environmental impact and subsequent cleanup. Right now we are giving these companies subsidies on their pollution by paying for cleanup. 3% for philip moris and so on. We are incentivising their pollution by reinforcing the idea that cigarettes will just disappear when they enter the environment. This is the largest pollution we have ever seen since the BP oil spill and their should be a similar reponse from governments to end the pollution of our envioronment and hold the companies responsible.  


#Trip Distances ----

#Check summary stats of distances
CompleteDataWithGoogle %>%
  filter(!is.na(DistanceFromLocation)) %>%
  pull(enddate) %>%
  summary()

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

library(grid)
#In meters
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
              method = "lm") + 
  scale_x_log10(breaks= 10^c(1:5)) + 
  scale_y_log10() + 
  theme_bw(base_size = 20) + 
  labs(x = "Human Trip Quantile Distance (m)", y = "Receipt Quantile Distance (m)") + 
  coord_equal()

linear_quantile_regression = lm(log10(receipt_distance_quantiles) ~ log10(montecarlo_distance_quantiles))

#Might not be the best way to do this because the data set is synthesized.
#ks.test(x = montecarlo_vector * 1.60934 * 1000, y =  CompleteDataWithGoogle$DistanceFromLocation)

#Wind and Rain ----

#Rain ----
for(row in 1:nrow(CompleteDataWithGoogle)){
  CompleteDataWithGoogle[row, "precip"] <-rainpresabs(startdate = CompleteDataWithGoogle[row,"dateprintedcleaned"], enddate = CompleteDataWithGoogle[row,"TimestampDatecleaned"])
}

#Receipt Directions ----
for(row in 1:nrow(CompleteDataWithGoogle)){ #This is the inverse of what we would think because wind direction is the inverse. This will tell us what direction the trash came from.
  CompleteDataWithGoogle[row, "bearing"] <- swfscMisc::bearing(lat2 = CompleteDataWithGoogle[row,"LocLat"]*pi/180, 
                                                   lon2 = CompleteDataWithGoogle[row,"LocLon"]*pi/180,
                                                   lat1 = CompleteDataWithGoogle[row,"lat"]*pi/180, 
                                                   lon1 = CompleteDataWithGoogle[row,"lon"]*pi/180)[1]
}

#convert bearing the azimuth? 

#difference between bearings.
for(row in 1:nrow(CompleteDataWithGoogle)){
  CompleteDataWithGoogle[row, "meanbearing"] <- differenceofmean(startdate = CompleteDataWithGoogle[row,"startdate"], enddate = CompleteDataWithGoogle[row,"enddate"])
}

CompleteDataWithGoogle$differencebearings <- angle_diff(CompleteDataWithGoogle$bearing, CompleteDataWithGoogle$meanbearing)

#Wind velocity transport velocity correlation ----

x.circ <- circular(CompleteDataWithGoogle$bearing, type="directions", units="degrees", template="geographics", rotation="clock")
rose.diag(x.circ, bins=10, col="gray", border=NA)

y.circ <- circular(CompleteDataWithGoogle$meanbearing, type="directions", units="degrees", template="geographics", rotation="clock")
rose.diag(y.circ, bins=10, col="gray", border=NA)

cor.circular(x.circ, y.circ, test = T)

CompleteDataWithGoogle$ycoordbearing <- 0.5*sin(rad(CompleteDataWithGoogle$bearing))
CompleteDataWithGoogle$xcoordbearing <- 0.5*cos(rad(CompleteDataWithGoogle$bearing))
CompleteDataWithGoogle$ycoordmeanbearing <- 1*sin(rad(CompleteDataWithGoogle$meanbearing))
CompleteDataWithGoogle$xcoordmeanbearing <- 1*cos(rad(CompleteDataWithGoogle$meanbearing))
CompleteDataWithGoogle$row <- 1:nrow(CompleteDataWithGoogle)

CompleteDataCoordPlot <- CompleteDataWithGoogle %>%
  filter(!is.na(xcoordmeanbearing) & !is.na(xcoordbearing)) %>% #Removes data where we don't have wind or dont have receipt direction from the analysis.
  dplyr::select(xcoordmeanbearing, ycoordmeanbearing, row) %>%
  rename(x = xcoordmeanbearing, y = ycoordmeanbearing) %>%
  bind_rows(CompleteDataWithGoogle %>%
              filter(!is.na(xcoordmeanbearing) & !is.na(xcoordbearing)) %>% 
              dplyr::select(ycoordbearing, xcoordbearing, row) %>%
              rename(x = ycoordbearing, y = xcoordbearing)) 
  

#360 (North) is on the right side.
p3 <- ggplot() + 
  geom_circle(aes(x0 = c(0,0), y0 = c(0,0), r = c(0.5,1))) + 
  geom_point(data = CompleteDataCoordPlot, aes(x = x, y = y), alpha = 0.5, size = 2) + 
  geom_line(data = CompleteDataCoordPlot, aes(x = x, y = y, group = row)) +
  theme_void() + 
  theme(aspect.ratio = 1, legend.position = NULL) + 
  geom_text(aes(x = c(0,1.1,0,-1.1), y = c(1.1, 0, -1.1, 0) , label = c("W", "N", "E", "S")), size = 10) 

plot(x.circ, stack = T)
#plotCircular(area1 = CompleteDataWithGoogle$bearing, area2 = CompleteDataWithGoogle$meanbearing, spokes = )
plot(x = y.circ, stack = T)

ggplot(CompleteDataWithGoogle, aes(x = "differnce", y = differencebearings)) + geom_boxplot(notch = T)

#how many of the receipts could have experienced precipitation?
p4 <- CompleteDataWithGoogle %>%
  filter(!is.na(dateprintedcleaned) & !is.na(TimestampDatecleaned)) %>%
  group_by(precip) %>%
  summarise(count = n()) %>%
  ggplot() + 
  geom_col(aes(x = precip, y = count)) +
  theme_bw(base_size = 20) +
  labs(x = "Precipitation Occured?", y = "Count")

grid.arrange(p4,p3,p1,p2, ncol = 4)

ggplot(CompleteDataWithGoogle, aes(x = differencebearings)) + geom_histogram(bins = 10) + scale_x_continuous(breaks = c(seq(0,180, by = 20)))+ dark_theme_classic(base_size = 20, base_line_size = 2) + labs(x = "Direction Angle Difference")
ggplot(CompleteDataWithGoogle, aes(x = DistanceFromLocation/Timedifference)) + geom_histogram(bins = 10) + scale_x_continuous()+ dark_theme_classic(base_size = 20, base_line_size = 2) + scale_x_log10() + labs(x = "Meters Traveled per Day")

#Summary stats----
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


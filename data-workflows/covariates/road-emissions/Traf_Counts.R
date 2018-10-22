library(sp)
library(rgdal)
library(rgeos)
library(DataCombine)
library(leaflet)
library(tmap)
library(dplyr)

setwd("Desktop/AoT/Traffic")

Traffic <- read.csv("Average_Daily_Traffic_Counts.csv")

coordinates(Traffic) <- Traffic[,c("Longitude", "Latitude")]

Traffic.Map <- leaflet(Traffic) %>% 
  addTiles() %>%
  addCircles(data = Traffic, weight = 2) %>%
  addPolygons(data = Chi.Map)

Traffic.Map

proj4string(Traffic) <- CRS("+init=epsg:4326")
Traffic <- spTransform(Traffic,CRS("+init=epsg:4326"))

#writeOGR(Traffic,".","Chicago_Traffic_2006",driver="ESRI Shapefile")

#Read in Shapefile for Chicago community areas
Chi.Map <- readOGR("Chi_Boundaries")
Chi.Map <- spTransform(Chi.Map ,CRS("+init=epsg:4326"))

#Plot community areas and traffic points together
Traffic.Map %>%
  addPolygons(data = Chi.Map)

#Returns a vector with length = # of Traffic points; index = community area of that point
PiP <- over(Traffic, Chi.Map)

#Combine Community Areas, Location info, and Traffic Volume into one Data Frame
CA.Traffic <- data.frame(PiP$community, Traffic$Latitude, Traffic$Longitude, Traffic$Total.Passing.Vehicle.Volume)
colnames(CA.Traffic) <- c("Community_Area", "Latitude", "Longitude", "Traffic_Volume")


#######Assign Points with NA Community Area to nearest community area###########

#Make df of NAs 
CA.NA <- CA.Traffic[which(is.na(CA.Traffic$Community_Area)),]

#Make NAs spatial and both NAs and map to UTM so can be used with rgeos (Zone 16)
coordinates(CA.NA) <- CA.NA[,c("Longitude", "Latitude")]
proj4string(CA.NA) <- CRS("+init=epsg:4326")

CA.NA <- spTransform(CA.NA,CRS("+init=epsg:26916"))
Chi.Map <- spTransform(Chi.Map ,CRS("+init=epsg:26916"))

#Container for Nearest Community Areas
n <- length(CA.NA)
Nearest.CA <- character(n)

#Iterate through finding the nearest Community Area for each NA
for (i in seq_along(Nearest.CA)) {
  Nearest.CA[i] <- as.character(Chi.Map$community[which.min(gDistance(CA.NA[i,], Chi.Map, byid = TRUE))])
  print(Nearest.CA[i])
}


#Assign NA values to nearest Community Area
#Create counter variable for NA's
j = 1

#Temporarily remake CA.Traffic non-spatial Dataframe
#CA.Traffic <- as.data.frame(CA.Traffic)

for(i in 1:nrow(CA.Traffic)) {
  if(is.na(CA.Traffic$Community_Area)[i]) {
    CA.Traffic$Community_Area[i] = Nearest.CA[j]
    print(CA.Traffic$Community_Area[i])
    j = j + 1
  }
}

#Revert CRS for Chicago Map
Chi.Map <- spTransform(Chi.Map ,CRS("+init=epsg:4326"))

###########################

#Group Traffic data by Community Area and total up volume for each one
CA.Agg <- CA.Traffic %>% 
  group_by(Community_Area) %>%
  summarize(sum(Traffic_Volume))

colnames(CA.Agg) <- c("community", "Traffic_Volume")

#Manually add in Burnside because it doesn't have traffic data so it doesn't get deleted when merge with map
Burnside <- c("BURNSIDE", 0)
CA.Agg <- InsertRow(CA.Agg, Burnside, RowNum = 13)
CA.Agg$Traffic_Volume <- as.integer(CA.Agg$Traffic_Volume)
Chi.Map@data <- left_join(Chi.Map@data, CA.Agg)
#Match orders of community areas
target <- Chi.Map@data$community
View(Chi.Map@data)
target
CA.Agg <- CA.Agg[match(target, CA.Agg$Community_Area),]
View(Chi.Map@data)

summary(Chi.Map)


#Merge Traffic Volumes with Community Areas
Chi.Map@data <- cbind.data.frame(Chi.Map@data, Traffic_Volume = CA.Agg$Traffic_Volume)

#Turn new data frame into spatial points data frame
coordinates(CA.Traffic) <- CA.Traffic[,c("Longitude", "Latitude")]
proj4string(CA.Traffic) <- CRS("+init=epsg:4326")
CA.Traffic <- spTransform(CA.Traffic,CRS("+init=epsg:4326"))

writeOGR(CA.Traffic,".","Traffic_Points_with_Community_Areas",driver="ESRI Shapefile")
writeOGR(Chi.Map,".","Community_Areas_with_Traffic_Volumes",driver="ESRI Shapefile")

tm_shape(Chi.Map) +
  tm_fill("Traffic_Volume", title = "Number of Vehicles per Day", style = "jenks", n = 7, palette = "Reds") + 
  tm_layout(main.title = "Average Daily Traffic Count by Community Area", main.title.size = 1) +
  tm_borders(alpha = 0.5)

  

b <- readOGR("Traffic_Points_with_Community_Areas")



a <- readOGR("Community_Areas_with_Traffic_Volumes")
View(a@data)





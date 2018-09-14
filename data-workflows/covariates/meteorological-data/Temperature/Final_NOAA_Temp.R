library(sp)
library(rgdal)
library(rgeos)
library(leaflet)
library(raster)
library(gstat)
library(tmap)
library(tidyverse)

#Read in the data
noaa.monthly <- read.csv("NOAA_master_monthly_final.csv")

#Preliminary look at data
glimpse(noaa.monthly)

#Select minimum monthly temperatures
temp.monthly.min <- noaa.monthly %>%
  dplyr::select(contains("min_mo"))

#Isolate for Summer 2015
sum.mintemp.2015 <- temp.monthly.min %>%
  dplyr:: select(`X2015_6_temp_min_mo`:`X2015_8_temp_min_mo`)

#Get avg. low temp over Summer
sum.mintemp.2015 <- rowSums(sum.mintemp.2015, na.rm = TRUE) / 3

#Select sensor coordinates/other info
sensor.info <- noaa.monthly %>%
  dplyr::select(STATION:elevation)

#Combine the sensor info with the sensor readings
sensor.temp.min <- cbind(sensor.info, sum.mintemp.2015)
tail(sensor.temp.min) #Tail taken because first few dozen values all NA/0

#Remove the 0s because 0 from the previous line, the NAs are now 0s
sensor.temp.min[sensor.temp.min==0] <- NA
complete.temp.min <- na.omit(sensor.temp.min)

#Take another look
glimpse(complete.temp.min)

#Set lat/lon
coordinates(complete.temp.min) <- complete.temp.min[,c("long", "lat")]
#Set projection to WSG84
proj4string(complete.temp.min) <- CRS("+init=epsg:4326")

#Import map of Chicago/Community Areas
chi.map <- readOGR("Chicago")

#Change projection to WSG84
chi.map <- spTransform(chi.map, CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84
                                    +towgs84=0,0,0"))

#Generate a variogram
sum.mintmp.vgm <- variogram(complete.temp.min$sum.mintemp.2015 ~ 1, complete.temp.min)
plot(sum.mintmp.vgm)

#Fitting a variogram model
sum.fit <- fit.variogram(sum.mintmp.vgm, model=vgm("Sph"))
plot(sum.mintmp.vgm, sum.fit)

#Generate prediction surface grid
pt2grid <- function(ptframe,n) {
  bb <- bbox(ptframe)  
  ptcrs <- proj4string(ptframe)  
  xrange <- abs(bb[1,1] - bb[1,2])  
  yrange <- abs(bb[2,1] - bb[2,2])  
  cs <- c(xrange/n,yrange/n)  
  cc <- bb[,1] + (cs/2)  
  dc <- c(n,n)  
  x1 <- GridTopology(cellcentre.offset=cc,cellsize=cs,cells.dim=dc)  
  x2 <- SpatialGrid(grid=x1,proj4string=CRS(ptcrs))
  return(x2)
}

#Create grid on Chicago
chi.grid <- pt2grid((chi.map),100)

#Krige
sum.kriged <- krige(complete.temp.min$sum.mintemp.2015 ~ 1, complete.temp.min, Chi.Grid, model = sum.fit)
plot(sum.kriged)

#Clip to city of Chicago
chi.sum.kriged <- sum.kriged[chi.map,]

#Visualize
plot(chi.sum.kriged)
title(main = "Average Low Temperature (F) Summer 2015", outer = FALSE)










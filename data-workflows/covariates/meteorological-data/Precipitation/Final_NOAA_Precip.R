library(sp)
library(rgdal)
library(rgeos)
library(leaflet)
library(raster)
library(gstat)
library(tmap)
library(tidyverse)

#Isolate precipitation
precip.monthly <- noaa.monthly %>%
  dplyr::select(contains("precip"))

#Select sensor coordinates/other info
sensor.info <- noaa.monthly %>%
  dplyr::select(STATION:elevation)

#Reattach precip to sensor info
sensor.precip <- cbind(sensor.info, precip.monthly)

#Remove missing readings
complete.sensor.precip <- na.omit(sensor.precip)
glimpse(complete.sensor.precip)

#Set lat/lon
coordinates(complete.sensor.precip) <- complete.sensor.precip[,c("long", "lat")]
#Set projection to WSG84
proj4string(complete.sensor.precip) <- CRS("+init=epsg:4326")

#Import map of Chicago/Community Areas
chi.map <- readOGR("Chicago")

#Change projection to WSG84
chi.map <- spTransform(chi.map, CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84
                                    +towgs84=0,0,0"))

#Generate variogram
may.precip.vgm <- variogram(complete.sensor.precip$X2014_5_precip_mo ~ 1, complete.sensor.precip)
plot(may.precip.vgm)

#Fitting a variogram model
precip.fit <- fit.variogram(may.precip.vgm, model=vgm("Exp"))
#plot
plot(may.precip.vgm, precip.fit)

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
precip.kriged <- krige(complete.sensor.precip$X2014_5_precip_mo ~ 1, complete.sensor.precip, Chi.Grid, model = precip.fit)
plot(precip.kriged)

#Clip to city of Chicago
chi.precip.kriged <- precip.kriged[chi.map,]

#Visualize
plot(chi.precip.kriged)
title(main = "Average Daily Precipitation, May 2014", outer = FALSE)














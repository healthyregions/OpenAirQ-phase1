library(sp)
library(rgdal)
library(rgeos)
library(tidyverse)
library(leaflet)
library(raster)
library(gstat)
library(tmap)

#Read in Point Emissions data
Point.Emissions <- read_csv("./data-workflows/covariates/point-emissions/final.csv")
glimpse(Point.Emissions)

#Set lat/lon
coordinates(Point.Emissions) <- Point.Emissions[,c("Longitude", "Latitude")]
#Set projection to WSG84
proj4string(Point.Emissions) <- CRS("+init=epsg:4326")

#Read in map of outline of Chicago
Chi.Bound <- readOGR("chipoli")
#Reproject to WSG84
Chi.Bound <- spTransform(Chi.Bound, CRS("+init=epsg:4326"))


#Take point emissions only in Chicago 
Chi.PE <- Point.Emissions[Chi.Bound,]

Chi.PE2 <- as.data.frame(Chi.PE)
Chi.PE2$`Emissions.in.Tons` <- sqrt(Chi.PE2$`Emissions.in.Tons`)
colnames(Chi.PE2)[8] <- c("Square Root of Emissions in Tons")
#Set lat/lon
coordinates(Chi.PE2) <- Chi.PE2[,c("Longitude", "Latitude")]
#Set projection to WSG84
proj4string(Chi.PE2) <- CRS("+init=epsg:4326")

tm_shape(Chi.Map) + 
  tm_borders() +
  tm_shape(Chi.PE2) +
  tm_bubbles(size  ="Square Root of Emissions in Tons") + 
  tm_layout(main.title = "Chicago PM2.5 Emissions Sources")

chi.map <- readOGR("./data-workflows/covariates/point-emissions/Chi_Boundaries", "Chi_Boundaries")

#Distance from centroids to nearest emissions source
library(geosphere)

#Take centroids of Community Areas
ca.centroids <- centroid(chi.map)

#Distance btwn centroids and Mn
#Mn source coords extracted and as vector
pe.coords <- Point.Emissions@coords
pe.mtx <- as.matrix(pe.coords)

#Create vector of distances between centroids and Mn emissions (rows = centroids, cols = Mn source)
dist.vctr <- spDists(ca.centroids, pe.mtx, longlat = TRUE)

#Find minimum distance to Mn source for each centroid (applying min to each row)
min.dist <- apply(dist.vctr, 1, min)

#Rebind to CA map
chi.map@data <- cbind(chi.map@data, min.dist)

#Make centroids spatial for plotting
ca.centroids <- as.data.frame(ca.centroids)
names(ca.centroids) <- c("longitude", "latitude")
coordinates(ca.centroids) <- ca.centroids[,c("longitude", "latitude")]
proj4string(ca.centroids) <- CRS("+init=epsg:4326")

#plot 
tm_shape(chi.map) +
  tm_fill(col = "min.dist", style = "jenks", title = "Distance (km)", palette = "-YlOrRd") +
  tm_shape(ca.centroids) +
  tm_dots(size = 0.05) +
  tm_layout(main.title = "Distance from Community Area Centroid to Nearest Point Emissions Source", main.title.size = 1.05)

# Save the min-distances as a Shapefile
writeOGR(chi.map, dsn = "./data-workflows/covariates/point-emissions", layer = "Community_Areas_with_Factory_Distances", driver="ESRI Shapefile")

#Create prediction surface
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
Chi.Grid <- pt2grid(Chi.Bound,100)

plot(Chi.Grid)


#Calculate IDW
PE.Idw <- idw(Chi.PE$`Emissions in Tons` ~ 1,Chi.PE,Chi.Grid,idp=2.0)
plot(PE.Idw)

PE.Raster <- raster(PE.Idw)
plot(PE.Raster)

Chi.Bound$var <- 1

pp <- raster(nrow=100,ncol=100,ext= extent(Chi.Bound))
Chi.Raster <- rasterize(Chi.Bound,pp,"var")

Chi.Interpolation <- Chi.Raster * PE.Raster
plot(Chi.Interpolation)

Chi.Interpolation.2 <- as(Chi.Interpolation,"SpatialGridDataFrame")
names(Chi.Interpolation.2@data) <- c("Total_Emissions")

plot(Chi.Interpolation.2)

#Visualize IDW Map with Location/size of point emissions sources
tm_shape(Chi.Interpolation.2) +  
  tm_raster("Total_Emissions", style = "jenks", title = "Total Emissions") +
  tm_shape(Chi.PE2) +  
  tm_bubbles(size  ="Square Root of Emissions in Tons", alpha = .2) + 
  tm_layout(main.title = "Chicago PM2.5 Emissions Sources") +
  tm_legend(legend.position = c("left", "bottom"), main.title = "Chicago PM2.5 Point Emissions")



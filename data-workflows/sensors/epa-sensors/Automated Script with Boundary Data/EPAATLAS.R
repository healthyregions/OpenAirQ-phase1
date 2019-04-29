#Create Atlas 27 APR 2019
library(raster)
library(gstat)
library(rgdal)
library(sf)
library("rgeos")
library(tmap)
library(leaflet)
library("tmap")
library(sp)
library(DT)
library(tidyverse) # data wranglin
library(data.table)
library(mapview)


CreateInterpolationImageFile <- function(folderpath,SpatialBoundary = NULL,ExtractDailyCut = F){
  # run this if local test
  # datadir = "dashboard/data/epa"
  # setwd(datadir)
  if(!is.null(SpatialBoundary)){
    returnlist <- list(ID = 1:nrow(SpatialBoundary))
  }
  epasource<-list.files(folderpath,pattern = "*.csv")
  for(i in 1 : length(epasource)){
    EPAMYR <- EPAMonthlyYearlyAVG(paste0(folderpath,epasource[i])) #extract yearly and monthly avg data
    Yearlist <-EPAMYR$epamonth[,.(.N),by = .(year)]
    for(j in 1:nrow(Yearlist)){
      YEAR <- Yearlist$year[j]
      MONTH <- 0
      # print(c(EPAMYR$datatype,YEAR,MONTH,i,j))
      ITPLResult <- CreateInterpolationImage(EPAMYR,YEAR,MONTH,byMONTH = F) 
      thisimage <- ITPLResult$resultimage
      Filename <- paste0("epa",EPAMYR$datatype,"_",YEAR,"_",MONTH)
      #SavePath <- paste0(folderpath,Filename,".tif")
      # elevation.sub <- mask(thisimage,SpatialBoundary)
      # plot(elevation.sub)
      
      
      #writeGDAL(thisimage,SavePath, drivername="GTiff",type="Float32")
      if(!is.null(SpatialBoundary)){
        #ext<-raster::extract(raster(SavePath),SpatialBoundary,fun = mean)
        ext<-raster::extract(raster(thisimage),SpatialBoundary,fun = mean)
        returnlist[[Filename]]<-ext
      }
      plotmap(folderpath,thisimage,EPAMYR$datatype,SpatialBoundary,ITPLResult$sitepoint,ext[,1])
    }
    
  }
  return(returnlist)
  # run this for testing result
  # plot(raster("epaDaily Mean PM2.5 Concentration_2018_9.tif")) 
}

EPAMonthlyYearlyAVG <-function(EpaRawData,valueFieldIndex = 7,timeFormat = "%m/%d/%Y", dayMax = T, daycut = 10){
  tf<-function(x,b){sum(x>b)}
  epadataframe <-fread(EpaRawData)
  valuefield <- names(epadataframe)[valueFieldIndex]
  epadataframe$date <- as.Date(epadataframe$Date,format = timeFormat)
  epadataframe$year <- year(epadataframe$date)
  epadataframe$month <- month(epadataframe$date)
  epaloc <- epadataframe[,.(.N),by = .(`Site ID`,SITE_LATITUDE,SITE_LONGITUDE)]
  epasub <- epadataframe[,.(`Site ID`,get(valuefield),year,month)]
  
  if(dayMax)
  {
    averagebymonth <- epasub[,lapply(.SD,mean,na.rm = T), by = .(`Site ID`,month,year)]
    averagebyyear <- averagebymonth[,lapply(.SD,mean,na.rm = T), by = .(`Site ID`,year)]
  }
  else
  {
    averagebymonth <- epasub[,lapply(.SD,tf,daycut), by = .(`Site ID`,month,year)]
    averagebyyear <- averagebymonth[,lapply(.SD,tf,daycut), by = .(`Site ID`,year)]
  }
  return(list(EPARAW = epadataframe, 
              epamonth = averagebymonth, 
              epayear = averagebyyear, 
              datatype = valuefield,
              loc = epaloc))
  
}

CreateInterpolationImage<-function(EPAMYR,YEAR,MONTH,GRID = "",RESL = 100, Datafield = "V2",IDW = T,byMONTH = T){
  
  #given a time 
  #year
  dfyear <- YEAR
  dfmonth <- MONTH
  datafield <- Datafield
  resl <- RESL # resolution
  if(byMONTH){
    inputdatatable<-merge(EPAMYR$epamonth,EPAMYR$loc, by.x = "Site ID", by.y = "Site ID")
    subtable <- inputdatatable[month == dfmonth & year == dfyear,]}
  else
  {
    inputdatatable<-merge(EPAMYR$epayear,EPAMYR$loc, by.x = "Site ID", by.y = "Site ID")
    subtable <- inputdatatable[year == dfyear,]
  }
  
  ###
  # epadataframe$day <- day(epadataframe$date)
  # subtable <- epadataframe[month == dfmonth & year == dfyear & day ==2,]
  # datafield <- "Daily Max 8-hour Ozone Concentration"
  ##
  df <- as.data.frame(subtable)
  df <- as.data.frame(subset(df,(!is.na(df[[datafield]]))))
  coordinates(df) <- df[,c("SITE_LONGITUDE", "SITE_LATITUDE")]
  proj4string(df) <- CRS("+init=epsg:4326")
  
  griddf<-SpatialPoints(cbind(c(-87.439931,-87.969428),c(41.589314,42.050333)),proj4string = CRS("+init=epsg:4326"))
  # proj4string(griddf) <- CRS("+init=epsg:4326")
  # griddf <- SpatialPointsDataFrame(proj4string = CRS("+init=epsg:4326"),bbox = bbox(SpatialPoints(cbind(c(-87.439931,-87.969428),c(41.589314,42.050333)))))
  # plot(kr.vgm, kr.fit)
  # create a grid for interpolation
  if(GRID ==""){
    itpgrid <- pt2grid(griddf,resl)
    print("ss")
    projection(itpgrid) <- CRS("+init=epsg:4326") 
  }else{
    itpgrid <- pt2grid(df,resl)
    projection(itpgrid) <- CRS("+init=epsg:4326") 
    # itpgrid <-GRID
  }
  
  
  if(IDW){
    return(list(resultimage = idw(df[[datafield]] ~ 1,df,itpgrid),sitepoint = df))
  }else
  {
    kr.vgm <- variogram(df[[datafield]] ~ 1, df)
    kr.fit <- fit.variogram(kr.vgm, model=vgm("Mat"),fit.kappa = T) # fit model
    # optimize the value of kappa in a Matern model, using ugly <<- side effect:
    f = function(x) attr(m.fit <<- fit.variogram(kr.vgm, vgm("Mat",nugget=NA,kappa=x)),"SSErr")
    optimize(f, c(0.1, 1000))
    return(list(resultimage = krige(df[[datafield]] ~ 1,df,itpgrid, model= m.fit ),sitepoint = df))
  }
  #plot(Itpr)
}

#generate Grid
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

plotmap <- function(plotpath,thisimage,filename,SpatialBoundary,sitepoint,keyvalue){
  # writeGDAL(thisraster,paste0(plotpath,filename,"2017_rawdata.tif"), drivername="GTiff",type="Float32")
  rawraster<-raster(thisimage)
  thisraster<-mask(raster(thisimage),SpatialBoundary)
  # thisraster<-raster(paste0(plotpath,filename,"2017_rawdata.tif"))
  pal <- colorNumeric(c("#fffedf", "#f5ac4f", "#a04c1c"), values(thisraster),
                      na.color = "transparent")
  
  m <- leaflet(SpatialBoundary)  %>%
    leaflet::setView(-87.623177, 41.881832, 10) %>% 
    addCircleMarkers(~sitepoint$SITE_LONGITUDE,~sitepoint$SITE_LATITUDE,radius = 5, color = "navy")%>% 
    addProviderTiles(providers$CartoDB.Positron)%>%
    addPolygons(color = "grey", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0) %>% 
    leaflet::addLegend(title = filename, pal = pal, values = values(rawraster))%>% # Add default OpenStreetMap map tiles
    addRasterImage(thisraster,colors = pal)
  m1 <- leaflet(SpatialBoundary)  %>%
    leaflet::setView(-87.623177, 41.881832, 10) %>% 
    addProviderTiles(providers$CartoDB.Positron)%>%
    addPolygons(fillColor  = ~pal(keyvalue), weight = 1, smoothFactor = 0.5, color = "grey",
                opacity = 1.0, fillOpacity = 1) %>% 
    addCircleMarkers(~sitepoint$SITE_LONGITUDE,~sitepoint$SITE_LATITUDE,radius = 5, color = "navy")%>% 
    leaflet::addLegend(title = filename, pal = pal, values = values(thisraster))# Add default OpenStreetMap map tiles

  mapshot(m,file = paste0(plotpath,filename,"2017_img.png"))
  mapshot(m1,file = paste0(plotpath,filename,"2017_adm.png"))
  
}

DataFolder <- "***THE PATH OF CSV DATA FILE***"
setwd("*** THE PATH OF SPATIAL BOUNDARY***")

#input spatial boundary of adm
SpatialBoundary <- readOGR("Chicago.shp")
projection(SpatialBoundary) <- CRS("+init=epsg:4326")

#EPAMonthlyYearlyAVG (valueFieldIndex = 5) Other pollutant
#EPAMonthlyYearlyAVG (valueFieldIndex = 7) AQI pollutant

CHICAGOEPA<-CreateInterpolationImageFile(DataFolder,SpatialBoundary)
ChicagoEPA <- as.data.frame(CHICAGOEPA)
ChicagoEPA$COMMUNITYNAME <- SpatialBoundary$community





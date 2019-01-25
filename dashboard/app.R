library(shiny)
library("rgdal")
library("rgeos")
library(tmap)
library(leaflet)
library("tmap")
library(sp)
library(sf)
library(shinydashboard)
library(raster)
library(mapedit)
library(mapview)
library(rlist)
library(DT)
library(leaflet.extras)
library(ggplot2)
library(lubridate)
library(plotly)
library(dygraphs) # interactive time series
# library(xts) # create time series objects (class xs)
library(tidyverse) # data wrangling
library(tmap) #modern data visualizations
library(data.table)
library(RCurl)
library(tiff)#readtiff

library(shinyBS) #tooltips
library(gstat) #kriging
library(stringr) # extract date from the epa date
library(shinyWidgets) #ui used in epa panel
library(shinydashboardPlus) #some ui


source("src/AirQ_Storyboard.R")
# https://ladsweb.modaps.eosdis.nasa.gov/search/
#read aircasting
#https://github.com/HabitatMap/AirCasting/blob/master/doc/api.md
#library(httr)
#tar<-GET("http://aircasting.org/api/averages.json?q[west]=-105.42674388525387&q[east]=-104.28347911474606&q[south]=39.530285217883865&q[north]=39.99792504639966&q[time_from]=1320&q[time_to]=1319&q[day_from]=0&q[day_to]=365&q[year_from]=2015&q[year_to]=2016&q[grid_size_x]=46.98081264108352&q[grid_size_y]=25&q[sensor_name]=AirBeam-PM&q[measurement_type]=Particulate+Matter&q[unit_symbol]=%C2%B5g/m%C2%B3")
# tar<-GET("http://aircasting.org/api/averages.json?q[west]=-105.42674388525387&q[east]=-104.28347911474606&q[south]=39.530285217883865&q[north]=39.99792504639966&q[time_from]=1320&q[time_to]=1319&q[day_from]=0&q[day_to]=365&q[year_from]=2015&q[year_to]=2016&q[grid_size_x]=46.98081264108352&q[grid_size_y]=25&q[sensor_name]=AirBeam-PM&q[measurement_type]=Particulate+Matter&q[unit_symbol]=%C2%B5g/m%C2%B3")
#content(tar)[[111]]$value





#css
SelectFillColor <- "#1A1A1A"
SelectBoundColor <- "#000000"

CircleFillColor <-"#1062DE"
CircleBoundColor <- "#003333" 
CircleTrans <- 0.9
CirBoundTrans <- 0.2
CircleRadius <- 3

CircleHighLight.Color <- "#FF9955"

StoryBoardWidth <-7
MapBWidth <- (12 - StoryBoardWidth )
MapBHeight <- 600

BaseMapStyle <- "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png"

#datasetpath
datapath <-"data/"

#initilization ----
AotNodesNonspatial <- fread(paste0(datapath,"nodes.csv")) #readAotNodes
AotNodes <- st_as_sf(AotNodesNonspatial, coords = c("lon", "lat"), crs = 4326, agr = "constant") #create points obj
ChicagoBoundary <- readOGR("./data/Chicago.shp") 


ChicagoBoundary.NROW<-NROW(ChicagoBoundary)
AotNodes_vis <- AotNodes
Drawned <-1 #the drawned and intersected selection area. this might be infeasible for a multilayer case

EPANode <- st_read(paste0(datapath,"PM2.5YearlyShapefile1.shp"),quiet = T)
EPAPM2_5.breaks <- c(1:10,12,15,35) #Breaks for EPAPM2_5
EPAPM2_5.pal <- colorNumeric(c("#D73027", "#FC8D59", "#D9EF8B", "#FEE08B", "#91CF60", "#1A9850"), 
                             EPAPM2_5.breaks, na.color = "transparent",reverse = T) #Palette for EPAPM2_5

aod.quarterly <- stack("AOD_Large_Quarterly.tif") #Quarterly AOD Data

unique.quarters <- seq(from=as.Date("2014-01-01"), #Names for quarterly data
                       to=as.Date("2018-09-18"),
                       by="quarter")

names(aod.quarterly) <- unique.quarters

aod.average <- raster("largebbox_mean.tif") #4 yr avg

#Manually fix faulty values
values(aod.average)[which(values(aod.average) == 0)] <- NA
values(aod.quarterly)[which(values(aod.average) == 0)] <- NA

monthly.breaks <- seq(from = 0, to = 0.6, 0.05) #Breaks for AOD data
monthly.aod.pal <- colorNumeric(c("green", "yellow", "red", "purple"), monthly.breaks, na.color = "transparent") #Palette for AOD

overall.breaks <- seq(from = 0, to = 0.5, 0.02) #Breaks for AOD data
overall.aod.pal <- colorNumeric(c("green", "yellow", "red"), overall.breaks, na.color = "transparent") #Palette for AOD


ndvi.quarterly <- stack("2017_Quarterly_NDVI.tif")
ndvi.qtrs <- seq(from=as.Date("2017-01-01"), #Names for quarterly data
                 to=as.Date("2017-12-31"),
                 by="quarter")
names(ndvi.quarterly) <- ndvi.qtrs

ndvi.breaks <- seq(from = -0.2, to = 1, 0.1)
ndvi.pal <- colorNumeric(c("lightblue", "yellow", "lightgreen", "green", "darkgreen"), ndvi.breaks, na.color = "transparent")

lc.map <- readOGR("Land_Cover_Indices")


#story board predefinied variables -------------
RegionID <-1 # give a region id
BestStory_n <- 6 # number of story board create
infTable<-as.data.frame(fread("data/merged_datatable.csv"))
MSB <-fread("data/Dynamic_Generate_InforBox.csv",fill = F)

CPTC <- F #If compare two counties
CPTC.name <-""
CPTC.namecompared <-""
CPTC.originstory <- ""
storyname <-""
CPTC.regionid <-""
CPTC.rankidtable <-""

thisarrowcolor <-	rgb(0.5,0.5,0.5,0.5)

#Import EPA panel variables --------
EPA.Description <- fread("data/Description.csv",header = F)
epa_panel.airpollutiontype <- c("CO","Pb","NO2","Ozone","PM10","PM2.5","SO2")#type of air pollution
if(T){
  
  
  #input epa data
  ChicagoEPA <- fread("./data/ChicagoEPA.csv")
  #input field validation function 
  ValidateNameofEPA <-function(NameChrArray,month = T){
    #return the valid month or year average field
    #we might add check for the sub-type and other sensor source
    if(month)
      return((as.numeric(NameChrArray[[3]]) %in% 1:12)&
               (as.numeric(NameChrArray[[2]]) %in% 1920:2050)&
               (substr(NameChrArray[[1]],1,3) %in% c("epa")))
    else
      return((as.numeric(NameChrArray[[3]]) %in% 0)&
               (as.numeric(NameChrArray[[2]]) %in% 1920:2050)&
               (substr(NameChrArray[[1]],1,3) %in% c("epa")))
  }
  #extract epa data time point- remove id and community name
  ChicagoEPATimePoint <- names(ChicagoEPA)[3:(ncol(ChicagoEPA)-1)]
  #create the metadata from epa preprocessed result
  CreateEPAMeta<-function(NameChrArray){
    NameChrArray <- unlist(strsplit(NameChrArray,'_'))
    Month <- as.numeric(NameChrArray[[3]])
    Year <- as.numeric(NameChrArray[[2]])
    DataSource <- substr(NameChrArray[[1]],1,3) 
    Date <- (paste0(Year,"-",ifelse(Month ==0, 6,Month),"-01"))
    #is valid field
    Valid <- ((as.numeric(NameChrArray[[3]]) %in% 0:12)&
                (as.numeric(NameChrArray[[2]]) %in% 1920:2050)&
                (substr(NameChrArray[[1]],1,3) %in% c("epa")))
    Subtype <- epa_panel.airpollutiontype[which(unlist(lapply(epa_panel.airpollutiontype, grepl, NameChrArray[[1]])))]
    
    # list(Month = Month, Year = Year, DataSource = DataSource, Valid = Valid, Subtype = Subtype)
    
    return(list(Month = Month, Year = Year, DataSource = DataSource, Valid = Valid, Subtype = Subtype, Date = Date))
  }
  
  #create visualize 
  EPAMeta<- (lapply(ChicagoEPATimePoint, CreateEPAMeta))
  EPAMetaF<- setDT(do.call(rbind.data.frame, EPAMeta)) #do.call otherwise the list will be collapsed.
  EPAMetaF$Subtype <- as.character(EPAMetaF$Subtype)
  EPAMetaF$ID <- as.vector(1:length(ChicagoEPATimePoint)+2)
  ifmonth <- T
  #extractvalue
  
  #create the plotly result of pollutants
  CreateTracePlotForOnePollutatn <- function(EPAMetaF, ChicagoEPA, typename = "CO", ifmonth = T){
    p<-plot_ly( type = 'scatter', mode = 'markers') 
    numobs <- nrow(ChicagoEPA)
    for(i in 1:numobs){
      #try to lapply this part, iteration migth cause severe lag
      communityname <- ChicagoEPA$COMMUNITYNAME[i]
      if(ifmonth)
      {
        extractedcol<-EPAMetaF[Subtype == typename & Month != 0 & Valid,]
      }else{
        extractedcol<-EPAMetaF[Subtype == typename & Month == 0 & Valid,]
      }
      extractedcol$Date <-  as.Date(extractedcol$Date)
      extractedcol <- extractedcol[order(Date)]
      extractedvalue <- as.numeric(as.matrix(ChicagoEPA[COMMUNITYNAME == communityname])[extractedcol$ID])
      p<-add_lines(p,x = extractedcol$Date, y = extractedvalue ,name = communityname, color = I("grey"),
                   line = list(shape = "spline"))
      
    }
    return(p)}
  #create epa-panel preload result
  EPA.Proload <- T
  EPA.ProloadPlotlychart <- list()
  if(EPA.Proload){
    for(i in 1:length(epa_panel.airpollutiontype))
    {
      p <- CreateTracePlotForOnePollutatn(EPAMetaF = EPAMetaF, ChicagoEPA =  ChicagoEPA, typename = epa_panel.airpollutiontype[i])
      EPA.ProloadPlotlychart <- rbind(EPA.ProloadPlotlychart,data.table(PType = epa_panel.airpollutiontype[i], Chart = list(p)))
    }
    
  }
  
  
}#if not initilize
#color
ChicagoAirColor <- list(
  transparent = 'rgba(0,0,0,0)',
  default = '#708090',
  highlight = "#AFEEEE"
)
#test text
tt<-"We use data directly from NASA. The Moderate Resolution Imaging Spectroradiometer 
(MODIS) satellite provides daily global coverage"
#margin of the plotly pie chart
m <- list(
  l = 10,
  r = 10,
  b = 10,
  t = 30,
  pad = 0
)
# Air pollution table epa -------------------------------------------------
epa_panel.airpollutiontype <- c("CO","Pb","NO2","Ozone","PM10","PM2.5","SO2")#type of air pollution


#Jion the infTable
JoinedSHP <- infTable
CN <- "CN"
JoinedSHP$CN <- toupper(infTable[[CN]])
ChicagoBoundary <- st_as_sf(merge(ChicagoBoundary,JoinedSHP,by.y = "CN", by.x = "community"))


#method ----
#ReadAotData ----
ReadAotData<-function(ExtraDate,ThisWorkPath)
{
  DataType = c("complete","public")
  if(is.Date(ExtraDate))
  {
    #get the name
    DateText <- format(ExtraDate, format = "%Y-%m-%d"); 
    
    FileNameWithoutExt<-paste0("chicago-",DataType[1],".daily.",DateText)
    FileName<-paste0("chicago-",DataType[1],".daily.",DateText,".tar")
    AotDateUrl <- paste0("https://s3.amazonaws.com/aot-tarballs/",FileName)
    SaveUrl <-paste0(ThisWorkPath,FileName) #Aot Chicago Public Daily
    file.path <- c(AotDateUrl)
    file.dest <- c(SaveUrl)
    if(!file.exists(file.dest)){
      download.file(file.path, file.dest,method="curl")
      if(file.info(file.dest)$size<1500)
      {
        pc <<- list(DWS = FALSE)
        return(pc)
      }
      
    }
    untar(file.dest,exdir=ThisWorkPath)
    if(file.info(file.dest)$size<1500)
    {
      pc <<- list(DWS = FALSE)
      return(pc)
    }
    outupt.data <- fread(paste0(ThisWorkPath,FileNameWithoutExt,"/data.csv.gz"))
    outupt.nodes <- fread(paste0(ThisWorkPath,FileNameWithoutExt,"/nodes.csv"))
    outupt.nodes.spt <- SpatialPointsDataFrame(outupt.nodes[,c('lon','lat')],outupt.nodes)
    outupt.prjN <- proj4string(outupt.nodes.spt) <- CRS("+init=epsg:4326")
    
    
    pc <- list (DWS = TRUE, data = outupt.data,nodes = outupt.nodes, spt = outupt.nodes.spt, prjN = outupt.prjN)
    setDT(pc$data)
    setDT(pc$nodes)
  }
  return(pc)
}

#InputEpaDatato3dSURFACE ----
# Interpolate Surface -----------------------------------------------------

#gstat::idw()
epasource<-list.files("data/EPA/",pattern = "*.csv")
epadata<-list()
for(i in 1: length(epasource)){
  epadata[[i]]<-CreateAirPollutantPointDataSet(paste0("data/EPA/",epasource[i]))
}

epr <- epadata[[i]]
framesnumber <- 100 #prepared frame in a epa surface
measuregrid <- as.matrix(epr[,c("lat","lon")])


cbox <- c(42.116887,41.570783,-87.424981,-88.030087) #chicago box
ndim <- c(20,20)
x <- seq(cbox[2],cbox[1],by=(cbox[1]-cbox[2])/ndim[1])
y <- seq(cbox[4],cbox[3],by=(cbox[3]-cbox[4])/ndim[2])
# grid <- as.matrix(expand.grid(x=x, y=y))
# dis_matrix<-1/sqrt(cdist(measuregrid, grid))
# distancematrix <- (dis_matrix)/rep(rowSums(dis_matrix),nrow(grid))

tdata <- epr[,2:(ncol(epr)-2)]
for(i in 1:ncol(tdata)){
  tdata[is.na(tdata[,i]),i] <- mean(tdata[,i], na.rm = TRUE)
}

# interppoint <- t(t(as.matrix(distancematrix))%*%as.matrix(tdata))
# interppoint <- t(as.matrix(tdata))%*%as.matrix(distancematrix)

# tdata <- epr[,2:(ncol(epr))]
# for(i in 1:ncol(tdata)){
#   tdata[is.na(tdata[,i]),i] <- mean(tdata[,i], na.rm = TRUE)
# }


frame <- 1:framesnumber
z.si <- list()
x.si<-list()
y.si<-list()
# saveGIF({
#   for(ik in 1:framesnumber){
#     spline_interpolated <- interp(epr$lon, epr$lat, tdata[,ik],
#                                   xo=y,
#                                   yo=x,
#                                   linear = FALSE, extrap = TRUE)
#     x.si <- spline_interpolated$x
#     y.si <- (spline_interpolated$y)
#     z.si <- as.matrix(spline_interpolated$z)
#     persp(x.si, y.si, z.si, theta = 45 , phi = 35, expand = 0.4, col = "lightblue")
#     # plot_ly(x = x.si, y = y.si, z = z.si) %>% add_contour()
#   }
# }, interval = 0.1, ani.width = 550, ani.height = 550)
for(ik in 1:framesnumber){
  spline_interpolated <- interp(epr$lon, epr$lat, tdata[,ik],
                                xo=y,
                                yo=x,
                                linear = FALSE, extrap = TRUE)
  x.si[[ik]] <- spline_interpolated$x
  y.si[[ik]] <- (spline_interpolated$y)
  z.si[[ik]] <- as.matrix(spline_interpolated$z)
  # plot_ly(x = x.si, y = y.si, z = z.si) %>% add_contour()
}
#ReadEpaData ----

# CreateTheStoryBoard -----------------------------------------------------

rankmatrix <- as.data.frame(infTable)
ninfTable <- sapply(infTable, as.numeric)
ncol<-NCOL(infTable)
nrow<-NROW(infTable)
for(i in 1:ncol){
  ca <- rank(as.data.frame(ninfTable[,i]))
  # if(length())
  ca[which(ninfTable[,i] %in% NA)] <- NA
  rankmatrix[,i] <- ca
}

CreateINPresult<-function(){
  
  FileNameList <- list.files(paste0(datapath,"EPA/"),pattern = "*.tif") #FileName List in EPA data set
  rawdate<-strsplit(str_extract(FileNameList[1], "[0-9]+-[0-9]+-[0-9]+_[0-9]"),'_')
  thisdate<-as.Date(unlist(rawdate)[1])
  # year = 1 month = 0 rawdate[2] 
  InterpResultList <- data.table(date = c(thisdate), year = (unlist(rawdate)[2]), data  = c(raster(paste0(datapath,"EPA/",FileNameList[1]))))
  for(i in 2:length(FileNameList)){
    rawdate<-strsplit(str_extract(FileNameList[i], "[0-9]+-[0-9]+-[0-9]+_[0-9]"),'_')
    thisdate<-as.Date(unlist(rawdate)[1])
    InterpResultList<-rbind(InterpResultList,data.table(date = c(thisdate), year = (unlist(rawdate)[2]),data  = c(raster(paste0(datapath,"EPA/",FileNameList[i])))))
  }
  return(InterpResultList)
}

InterpResultList<-CreateINPresult()#IinitializedEPA

#ui ----
ui <- dashboardPage(
  dashboardHeader(title = "Open Air Chicago"),
  dashboardSidebar(
    sidebarMenu(id = "tablist",
                menuItem("Home", tabName = "Home"),
                menuItem("About", tabName = "About"),
                menuItem("Pollution Measures",
                         menuSubItem("PM 2.5", "pm"),
                         menuSubItem("Aeorosol Optical Depth", "aod")),
                menuItem("Pollution Drivers",
                         menuSubItem("Weather", tabName = "noaa"),
                         menuSubItem("Traffic", tabName = "road_emissions"),
                         menuSubItem("Factory", tabName = "factory_emissions"),
                         menuSubItem("Elevation",tabName = "elevation"),
                         menuSubItem("Greenness", tabName = "ndvi"),
                         menuSubItem("Land Cover", tabName = "landcover")),
                menuItem("Population Measures",
                         menuSubItem("Demographic Data", tabName = "demographic"),
                         menuSubItem("Public Health", tabName = "health")),
                menuSubItem("Explore Array of Things", tabName = "aot"),
                menuSubItem("Explore EPA Stations","expepa")
    )
  ),
  dashboardBody(
    tags$head(tags$style(
      HTML('
           @keyframes example {
           from {border-radius: 0px;}
           to {border-radius: 20px;background-color:white;color:rgba(50,50,50,0.5);}
           }
           @keyframes slide-up {
           0% {
           opacity: 0;
           transform: translateY(20px);
           }
           100% {
           opacity: 1;
           transform: translateY(0);
           }
           }
           .info-box {height: 45px;  margin : 0in; float: left; border: 0px;} 
           .info-box:hover {
           animation: example;
           animation-name: example;
           animation-duration: 1s;
           animation-timing-function: ease-in-out;
           animation-fill-mode: forwards;
           -webkit-animation-fill-mode: forwards;
           
           }
           .info-box-icon {height: 100%; line-height: 100%; padding-top: 20px }
           .bg-lime {background-color:#00ff80!important; }
           .bg-olive {background-color:	#FFD700!important; }
           #inf *,#sinf * {background-color:rgba(255,0,0,0); border-top:0px}
           #homerow *  {padding-left:0px}
           .info-box-content {padding: 0px;}
           .info-box-content > p {padding :1px; font-size: 16px;}
           .info-box-number {font-size: 26px; padding-top:2px; padding-bottom:1px}
           .leaflet-control-container {border-top:2px;}
           #inf {padding: 2px; padding-rigt:2px}
           #inf > * {padding: 4px; }
           #inf box-body {padding: 0px; width:100%;}
           #sinf > * {padding: 4px}
           #sinf  {padding: 0px;animation: slide-up 1s ease-in-out;}
           #CN {padding: 0px; background-color: rgba(255,0,0,0); text-align: center; 
           border-color: rgba(255,0,0,0); margin-bottom:0px}
           #InfPannel {padding: 0px ; border-radius: 30px;}
           #InfPannel > * {padding-left: 6px; padding-right: 0px}
           #MapPannel > * {padding-left: 0px; padding-right: 0px}
           #MainContent .box {border-top:0px; padding-left: 3px; padding-right: 2px}
           #Homepage-infp-plotly {padding: 0 0 0 15px}
           #comparedrow {margin-bottom: 5px; padding: 0 2px 0 2px}
           #epa_panel_page *  {padding-left: 0; padding-right: 0 margin-left: 2px; margin-right:2px}
           
           #box3col  {background-color:rgba(255,0,0,0)}
           .leaflet-container {
           }
           @keyframes TransMap {
           from {}
           to {
           transform: rotate(-90deg) scale(2) translateY(-50%) translateX(50%);}
           }
           @keyframes TransAbsPanel {
           from {
           top: -100px;
           transform: translateY(-20%);
           }
           to {
           transform: translateY(0%);
           border-radius: 10px;}
           }
           div#lv2 {
           height: 500px;
           background-color: rgba(255,255,255,1)
           }
           div#lv2_ab:hover {
           animation: TransMap;
           animation-name: TransMap;
           animation-duration: 1s;
           animation-timing-function: ease-in-out;
           animation-fill-mode: forwards;
           -webkit-animation-fill-mode: forwards;
           }
           #EPAPolltantsControl {
           margin-bottom: 2px;
           }
           #EPAPolltantsControl *{
           margin-bottom: 1px;
           }
           #EPAPolltantsControl > *{
           padding-bottom: 1px;
           margin-left: 3px;
           margin-right: 3px;
           height: 54px;
           }
           #EPAPolltantsControl_Panel{
           width: 80%;
           margin: auto;
           padding-bottom: 3px;
           transform: translateY(-40%);
           }
           #CuteBox {
           height: 0px;
           position: relative;
           }
           #CuteBox > * {
           z-index: 10000000;
           position: relative;
           top: -81px;
           left: 69%;
           }
           #CuteBox *.box{
           height: 100px;
           width: 100px;
           border: 0;
           box-shadow: 0px 0px 5px #888888;
           clip-path: polygon(-36% 32%, 76% 21%, 99% 62%, 3% 54%);
           -webkit-clip-path: polygon(-36% 32%, 76% 21%, 99% 62%, 3% 54%);
           }
           #m1 {
           width : 5px;
           }
           #m2 {
           width : 10px!important;
           height: 200px;!important;
           background-color: green;
           overflow: hidden;
           }
           #m2:hover {
           transition: width 0.35s ease-in-out;
           width : 7700%!important;
           background-color: black;
           overflow: visible;
           }
           #m3 {
           height: 200px!important;
           width: inhert!important;
           z-index:1000000;
           }
           #m4 {
           position: relative;
           right: -10px;
           top: -110%;
           width: 85%!important;
           height: 100%!important;
           z-index: 5000;
           }
           #m3_1a{
           height: 20%;
           width: 10px;
           background-color: #FFF0F5;
           }
           #m3_2a{
           height: 80%;
           width: 10px;
           background-color: #708090;
           }
           #m3_1{
           height: 40%;
           width: 10px;
           background-color: #AFEEEE;
           }
           #m3_2{
           height: 60%;
           width: 10px;
           background-color: #708090;
           }
           #v1,#v1a{
           width: 50%;
           height:100%;
           background-color: rgb(23, 48, 74);
           }
           #v2{
           position: relative;
           top:-100%;
           right:-50%;
           width: 120%;
           height: 100%;
           background-color:  #708090;
           }
           #CuteBox *.box{
           background: -webkit-linear-gradient(left, rgba(112, 127, 143, 0.33) , rgb(112, 127, 143));
           }
           #CuteBox #CommunityName{
           transform: translateX(-55%);
           font-size: 20px;
           position: relative;
           top: -244px;
           color: rgb(44, 60, 60);
           animation: TransAbsPanel 1s ease-in-out;
           -webkit-animation: TransAbsPanel 1s ease-in-out;
           transition: ease-in-out;
           text-align: center;
           width: 179px;
           background-color: #afeeee;
           }
           #CuteBox > h4{
           position: relative;
           top:-246px;
           left: 88%;
           color: rgb(226, 255, 255);
           }
           #v2 > p{
           padding: 3em 4em 1em 3em;
           color: rgba(255,255,255,1);
           }
           #v1 > h1{
           font-size: 150px;
           color: rgba(175,238,238,1);
           text-align:left;
           }
           #v1 > h3{
           font-size: 120px;
           color: rgba(175,238,238,1);
           text-align:right;
           }
           #v1a > h3{
           font-size: 120px;
           color: #FFF0F5;
           text-align:right;
           }
           #radiob{
           position: relative;
           top: -7%;
           }
           .btn-default{
           background-color: rgb(255,255,255);
           border-color:  rgba(225,225,225,0.2);
           }
           .btn-default.active{
           background-color: rgb(175,238,238)!important;
           border-color:  rgba(175,238,238,0.2)!important;
           }
           .btn-default.hover{
           background-color: rgb(175,238,238)!important;
           border-color:  rgba(225,225,225,0.2);
           }
           '))),
    tabItems(
      #First tab content ----
      tabItem(tabName = "Home",
              fixedRow(id = "titlerow", width = 12, 
                       column(width = 4, checkboxInput("CPTCB", 
                                                       label = "cross community comparison",
                                                       value = F)),
                       column(width = 8,verbatimTextOutput("CN"))),
              fixedRow(id = "homerow",column(id = "inf", width = 12,
                                             # uiOutput("HSB") #Homepae Stroy Board
                                             infoBoxOutput("inf1",width = 4),
                                             infoBoxOutput("inf2",width = 4),
                                             infoBoxOutput("inf3",width = 4))),
              fixedRow(id = "comparedrow",  
                       conditionalPanel(id = "sinf", condition = "input.CPTCB", 
                                        infoBoxOutput("inf4",width = 4),
                                        infoBoxOutput("inf5",width = 4),
                                        infoBoxOutput("inf6",width = 4))),
              fixedRow(id = "MainContent",
                       column(id = "InfPannel", width = 7,
                              box(id = "Homepage-infp-plotly", width = 12,
                                  plotlyOutput("MainInfPlot"))
                       ),
                       column(id = "MapPannel",
                              width = 5,
                              leafletOutput("HLM",height = 700) #Homepage Leaflet Map
                       )
              )
      ),
      #About-----
      tabItem(tabName = "About",
              img(src='AirQAbout.png', height = 800, align = "center")),
      
      #tabitem noaa ----
      tabItem(tabName = "noaa",
              
              fluidRow(
                box(
                  width = 4,
                  h1("Meteorological Data"),
                  selectInput(inputId="type", label="Choose type of data",c('Temperature Min', 'Temperature Max', 'Precipitation','Sensor locations')),
                  sliderInput(inputId="year",label = "Choose a year",
                              value = 2012, min=2012,max=2018),
                  selectInput(inputId="monthOrYear",  label="Choose Monthly or Yearly",c('Yearly', 'Monthly')),
                  conditionalPanel(
                    condition = "input.monthOrYear == 'Monthly'",
                    
                    sliderInput(inputId="month",label = "Choose a month",
                                value = 1, min=1,max=12)),
                  br(),
                  h4("About AOD"),
                  p("Temperature and preciptation both impact short and longterm air pollution trends,
                    and together consitute long-term climate patterns"),
                  br(),
                  h4("Data Source"),
                  p("This data is from NOAA.")
                  ),
                
                
                box(
                  width = 8,
                  leafletOutput("working_map", width = 800, height = 600)
                )
              ),
              fluidRow(
                plotOutput("graph")
              )
      ),
      #tabitem aod
      tabItem(
        "aod", fluidPage(
          
          fluidRow(
            box(
              width = 4,
              h1("Aerosol Optical Depth"),
              radioButtons("AODYM","Average Time Window: (Quarterly/4 Year Average):",c("Quarterly"= "qtr","Overall"= "ovr")),
              br(),
              h4("About Aerosol Optical Depth (AOD)"),
              p("Aerosol optical depth is a measure of the extinction of the solar beam by dust
                and haze. In other words, particles in the atmosphere (dust, smoke, pollution)
                can block sunlight by absorbing or by scattering light."),
              br(),
              h4("Data Source"),
              p("We use data directly from NASA. The Moderate Resolution Imaging Spectroradiometer
                (MODIS) satellite provides daily global coverage, but the 10 km resolution of its
                aerosol optical depth (AOD) product is not suitable for studying spatial variability
                of aerosols in urban areas. Recently, a new Multi-Angle Implementation of Atmospheric
                Correction (MAIAC) algorithm was developed for MODIS which provides AOD at 1 km
                resolution.")
              
              ),
            
            box(width = 8,
                sliderInput("AODT", "Select time period:",
                            min = strptime("2014/01/01","%Y/%m/%d"), 
                            max = strptime("2018/07/01","%Y/%m/%d"),
                            value = strptime("2016/04/01","%Y/%m/%d"),
                            timeFormat = "%Y/%m",
                            step = as.difftime(92, units = "days"),
                            animate = animationOptions(interval = 2000)),
                
                # tags$style(type="text/css",
                #            "#MainMap.recalculating { opacity: 1.0 }"),
                conditionalPanel(condition = "input.AODYM == 'qtr'",
                                 leafletOutput("aodmapquarterly",height = MapBHeight)),
                conditionalPanel(condition = "input.AODYM == 'ovr'",
                                 leafletOutput("aodmapoverall",height = MapBHeight))
            )
              )
          
              )
          ),
      
      #tabitem aot ----
      tabItem(tabName = "aot",
              fluidRow(
                box(
                  width = 12,
                  editModUI("editor")
                ),
                box(
                  actionButton("DWLDAot","Load AoT Data"),
                  actionButton("VisAot","Visualize Aot Data"),
                  dateInput('DownloadDate',
                            label = 'Date input: yyyy-mm-dd',
                            value = Sys.Date()
                  ),
                  selectInput("AotField","AotField",c("Node_id","Value")),
                  DT::dataTableOutput("visAot_Text")
                )
              )
      ),
      #tabitem demographic_data ----
      tabItem(tabName = "demographic",
              fluidRow(
                box(
                  width = 4,
                  selectInput(inputId ="demographic_type",
                              label = "Choose demographic data",
                              choices = c("Percent of Crowded Housing",
                                          "Percent of Households Below Poverty",
                                          "Percent Unemployed",
                                          "Percent Without a High School Diploma",
                                          "Per Capita Income",
                                          "Hardship Index"))
                ),
                box(
                  width = 8,
                  leafletOutput("demographic_map",width = 800, height = 600)
                )
              )
      ),
      #tabitem public health
      tabItem(tabName = "health",
              fluidRow(
                box(
                  width = 4,
                  selectInput(inputId = "health_type",
                              label = "Choose Public Health Data",
                              choices = c("Birth Rate",
                                          "Low Birth Rate",
                                          "Teen Birth Rate",
                                          "Lung Cancer",
                                          "Cancer",
                                          "Tuberculosis"
                              ))
                ),
                box(
                  width = 8,
                  leafletOutput("health_map",width = 800, height = 600)
                )
              )
      ),
      
      tabItem(tabName = "elevation",
              fluidRow(
                box(
                  width = 4,
                  p("test text")
                ),
                box(
                  width = 8,
                  leafletOutput("elevation_map",width = 800, height = 600)
                )
              )
      ),
      
      tabItem(tabName = "ndvi",
              fluidRow(
                box(
                  width = 4,
                  h1("NDVI"),
                  h4("About Normalized Difference Vegetation Index (NDVI)"),
                  p("NDVI is a measure of greenness calculated from near infrared and red bands.
                    It ranges in value from -0.1 to 0.1, with 0.1 and below generally corresponding 
                    to areas with little greenery while 0.5-0.6 and above typically indicate the
                    presence of large amounts of vegetation coverage. "),
                  br(),
                  h4("Data Source"),
                  p("We used NDVI data from NASA's MODIS Terra satellite. While NDVI is available 
                    at higher resolutions through datasets such as LANDSAT 8, the MODIS dataset 
                    provides a consistent, 16-day NDVI image. This  product comes at a 250 meter 
                    spatial resolution and is less susceptible to interference from heavy cloud 
                    coverage due to the time scale.")
                  ),
                
                box(width = 8,
                    sliderInput("NDVIT", "Select quarter:",
                                min = strptime("2017/01/01","%Y/%m/%d"), 
                                max = strptime("2017/10/01","%Y/%m/%d"),
                                value = strptime("2017/01/01","%Y/%m/%d"),
                                timeFormat = "%Y/%m",
                                step = as.difftime(92, units = "days"),
                                animate = animationOptions(interval = 2000)),
                    
                    # tags$style(type="text/css",
                    #            "#MainMap.recalculating { opacity: 1.0 }"),
                    
                    leafletOutput("ndvimap",height = MapBHeight))
                  )
                  ),
      tabItem(tabName = "landcover",
              fluidRow(
                box(
                  width = 4,
                  h1("Land Cover"),
                  radioButtons("lctype",
                               "Select Land Cover Index to Show:",
                               c("Green Index" = "grn_ndx",
                                 "Blue Index" = "blu_ndx",
                                 "Gray Index" = "gry_ndx")),
                  br(),
                  h4("About Land Cover"),
                  p("Land cover indicates the development or material occupying a given area.
                    While the dataset used for this dashboard provided hundreds of detailed 
                    categories of land coverage, we aggregated these into three color-indices
                    in order to quantify the most important aspects. The green index is the 
                    sum-total percentage of the following classifications provided by the USGS:
                    Agricultural & Developed Vegetation, Forest & Woodland, Introduced & Semi-Natural 
                    Vegetation, and Shrub & Herb Vegetation as part of a given Community Area. The 
                    blue index is the sum-total percentage of Open Water as part of a given Community 
                    Area. Lastly, the gray index is the sum-total percentage of Developed & Other 
                    Human Use and Recently Disturbed or Modified land as part of a given Community Area."),
                  br(),
                  h4("Data Source"),
                  p("The land cover data was created from a modified version of the USGS's
                    National Gap Analysis Project Land Cover satellite imagery. The raster
                    data was aggregated by Community Area and then converted into the 
                    respective indices using the afformentioned methodology.")
                  
                  ),
                
                box(width = 8,
                    conditionalPanel(condition = "input.lctype == 'grn_ndx'",
                                     leafletOutput("lcgreenmap",height = MapBHeight)),
                    conditionalPanel(condition = "input.lctype == 'blu_ndx'",
                                     leafletOutput("lcbluemap",height = MapBHeight)),
                    conditionalPanel(condition = "input.lctype == 'gry_ndx'",
                                     leafletOutput("lcgraymap",height = MapBHeight))
                )
                  )
              
                  ),
      
      #tabitem road_emissions ----
      tabItem(tabName = "road_emissions",
              fluidRow(
                box(
                  width = 4,
                  h1("Road Emissions"),
                  selectInput(inputId="road_emit_type", 
                              label="Choose type of data",
                              c("Traffic Volume", "Road Lengths")),
                  conditionalPanel(condition = "input.road_emit_type == 'Traffic Volume'",
                                   br(),
                                   h4("About Traffic Volume"),
                                   p("Traffic volume is one way of measuring traffic pollution. 
                                     We aggregated at the community-area level the original traffic counts 
                                     collected in each point of the road and divided them by the area of 
                                     each community area to estimate the average traffic pollution per 1 squared-mile. 
                                     The darker red area indicates the higher traffic volume, 
                                     which implies that traffic volumes are highly concentrated on 
                                     Loop (downtown area) and Hyde Park."),
                                   br(),
                                   h4("Data Source"),
                                   p("The original traffic counts data set, Average Daily Traffic Counts, 
                                     is from the City of Chicago Data Portal.")),
                  conditionalPanel(condition = "input.road_emit_type == 'Road Lengths'",
                                   br(),
                                   h4("About Road Lengths"),
                                   p("Road length is another way of measuring traffic pollution. 
                                     We calculated the total length of roads in each community area and 
                                     divided it by the area of each community area 
                                     to estimate the average traffic pollution per 1 squared-mile. 
                                     Since the darker red color indicates the longer length of roads, 
                                     Edison Park and Montclare seem to have longer length of roads in total than the other areas. 
                                     Besides, the roads are also concentrated in community areas near downtown."),
                                   br(),
                                   h4("Data Source"),
                                   p("The original road lengths data set is from OpenStreetMap."))
                                   ),
                box(
                  width = 8,
                  leafletOutput("road_emissions_map", width = 800, height = 600)
                )
      )
      ),

      #tabitem factory_emissions ----
      tabItem(tabName = "factory_emissions",
              fluidRow(
                box(
                  width = 4,
                  h1("Factory Emission"),
                  h4("About Factory Emission"),
                  p(" "),
                  br(),
                  h4("Data Source"),
                  p(" ")
                ),
                box(
                  width = 8,
                  leafletOutput("factory_emissions_map", width = 800, height = 600)
                )
              )
      ),
            
      # pm tab ------------------------------------------------------------------
      tabItem(
        "pm", fluidPage(
          
          fluidRow(
            box(
              width = 4,
              h1("PM 2.5"),
              radioButtons("EPAYM","Average Time Window: (monthly/yearly):",c("Yearly"=0,"Monthly"=1)),
              checkboxInput("EPASiteOn", "Show EPA Monitoring Station", value = TRUE, width = NULL),
              br(),
              h4("About PM 2.5"),
              p("PM2.5 refers to atmospheric particulate matter (PM) that have a diameter of less than 
                2.5 micrometers, which is about 3% the diameter of a human hair. PM2.5 are deadly because 
                their particles are so small that they are inhaled and trapped more deeply in the lungs and 
                bloodstream, posing significant health risks to the respiratory and cardiovascular systems 
                including aggravated asthma, decreased lung function, nonfatal heart attacks etc."),
              p("Susceptible groups with pre-existing lung or heart disease, as well as elderly people and 
                children, are particularly vulnerable. Based on known health effects, both short-term (24-hour) and long-term (annual mean) guidelines 
                are recommended by the World Health Organisation. These are 10 µg/m3 annual mean, and 25 µg/m3 
                24-hour mean."),
              br(),
              h4("Data Source"),
              p("PM2.5 measures in the air here are collected from Air Quality System (AQS) data, which contains 
                representative ambient air pollution data across different states collected by federal, state, 
                local, and tribal air pollution control agencies from over thousands of monitors. Stations in this area are from the EPA.")           
              
              ),
            
            box(width = 8,
                sliderInput("EPAT", "Select time period:",
                            min = strptime("2015/01/15","%Y/%m/%d"), 
                            max = strptime("2017/12/31","%Y/%m/%d"),
                            value = strptime("2015/07/16","%Y/%m/%d"),
                            timeFormat = "%Y/%m",
                            step = as.difftime(30,units = "days"),
                            animate = animationOptions(interval = 500)),               
                # tags$style(type="text/css",
                #            "#MainMap.recalculating { opacity: 1.0 }"),
                leafletOutput("MainMap",height = MapBHeight))
              )
          
              )
          ),
      tabItem("expepa",  fluidPage(id = "fluidpage1",
                                   fluidRow(id = "EPAMainContent",
                                            column(width = 8,
                                                   fixedRow( 
                                                     column(id = "lv2",width = 12,
                                                            div(id = "radiob",radioGroupButtons(inputId = "epa_panel_checkbox",justified = T,
                                                                                                direction = "horizontal",
                                                                                                label = "Air Pollutants",
                                                                                                choices = epa_panel.airpollutiontype)),
                                                            fixedRow(
                                                              column(width = 1,div(id="m1",
                                                                                   div(id = "m2",
                                                                                       div(id = "m3",
                                                                                           div(id = "m3_1"),div(id="m3_2")),
                                                                                       div(id = "m4",
                                                                                           div(id="v1",h3("5%")),
                                                                                           div(id="v2",p(tt))
                                                                                       ))),
                                                                     br(),
                                                                     div(id="m1",
                                                                         div(id = "m2",
                                                                             div(id = "m3",
                                                                                 div(id = "m3_1a"),div(id="m3_2a")),
                                                                             div(id = "m4",
                                                                                 div(id="v1a",h3("10%")),
                                                                                 div(id="v2",p(tt))
                                                                             )))),
                                                              column(textOutput("EPA_Description"), width = 10)))
                                                     # fixedRow(
                                                     #   plotlyOutput("EPAPieChart_DataRealiability",width = "49%",height = "200px",inline = T),
                                                     #   plotlyOutput("EPAPieChart_MaximumDailyCut",width = "49%",height = "200px",inline = T))
                                                     # )
                                                   )
                                            ),
                                            column(id = "box3col",width = 4,
                                                   div(id = "CuteBox",box(id = "Q1B",height = 200,width = 200),textOutput("CommunityName"),h4("CO")),
                                                   fixedRow( width = 12, leafletOutput("EPANMAP",height = 500)))),
                                   fluidRow(id = "EPAPlotlyPanel",
                                            plotlyOutput("EPAPlotlyChart"))
      )),
      # epa_panel ---------------------------------------------------------------
      tabItem("epa_panel",fluidPage(id = "epa_panel_page",
                                    column(width = 1, 
                                           checkboxGroupButtons(inputId = "epa_panel_checkbox",
                                                                direction = "vertical",
                                                                label = "Air Pollutants",
                                                                width = 6,
                                                                choices = epa_panel.airpollutiontype)),
                                    column(width = 11,
                                           fluidRow("epa_panel_global",background = "aqua",
                                                    column(width= 8,
                                                           plotlyOutput("epa_trace"),
                                                           radioGroupButtons(inputId = "epa_trace_radio",
                                                                             label = "Air Pollutants",
                                                                             width = 6,
                                                                             choices = epa_panel.airpollutiontype,
                                                                             choiceValues = 1:length(epa_panel.airpollutiontype)),
                                                           plotlyOutput("epa_panel_global_plotly",height = 700),
                                                           sliderInput("epa_panel_time","epa time",min = 1, max = 100,1)),
                                                    column(width = 4,
                                                           tabBox(width = 12,
                                                                  tabPanel(title = "Pie",
                                                                           h1("epa_panel_global_tab1_box")),
                                                                  tabPanel(title = "Surface", h1("epa_panel_global_tab2_box")),
                                                                  tabPanel(title = "Stat.", h1("epa_panel_global_tab3_box")))
                                                    )
                                                    
                                           ),
                                           fluidRow(
                                             box("epa_panel_local",background = "orange", width = 12)
                                           ))
                                    
      ))
      
      
              )
    
    
    
  )
  
  )




#server ----
server = function(input, output,session){
  
  # aot logic ----
  ns <- NS("editor")# set namespace
  
  
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g Area",
    ChicagoBoundary$community, ChicagoBoundary$shape_area
  ) %>% lapply(htmltools::HTML)
  base_map <- leaflet(ns("map")) %>% 
    addProviderTiles(providers$Stamen.TonerLite)%>% 
    addDrawToolbar(
      # targetLayerId = "draw",
      targetGroup = "draw",
      # polygonOptions = FALSE,
      circleOptions = FALSE,
      # rectangleOptions = FALSE,
      polylineOptions = FALSE,
      markerOptions = FALSE,
      circleMarkerOptions = FALSE,
      singleFeature = TRUE
      #editOptions = editToolbarOptions()
    )%>% 
    addPolygons(data = ChicagoBoundary, color = "darkslategray",fillOpacity  = 0.1, stroke = FALSE,
                highlight = highlightOptions(
                  # weight = 5,
                  color = "#666",
                  # dashArray = "",
                  fillOpacity = 0.7),
                # bringToFront = TRUE),
                label = labels)%>%
    addFeatures(AotNodes)
  
  #addLayersControl(overlayGroups = c("draw"), options = layersControlOptions(collapsed = FALSE))
  drawn <- callModule(editMod, "editor", base_map)
  #brushing
  testdrawn <-function(){
    req(drawn()$finished)
    if(Drawned <= length(drawn()$finished$X_leaflet_id))
    {
      qk_intersect_test <<- st_intersection(drawn()$finished[length(drawn()$finished$X_leaflet_id),], nc)
      Drawned <<- Drawned +1
    }
    return(qk_intersect_test) 
  }
  #refresh the map
  RefreshMap <- function(){
    qpal <- colorQuantile(
      palette = "Blues",
      domain = AotNodes_vis$V1)
    
    leafletProxy(ns("map"),data = AotNodes_vis) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      removeControl(layerId = "L1") %>% #remove the original legend then add a new one
      clearMarkers()%>%
      addCircleMarkers(
        color = ~qpal(V1),
        weight = 2,
        opacity = 0.8,
        radius = 3,
        fill=TRUE,
        popup =~as.character(V1)
      )%>%
      leaflet::addLegend(pal = qpal, values = ~V1, opacity = 1,layerId = "L1")
  }
  #load aot
  observeEvent(input$DWLDAot,{
    #download data at given date
    SRAot <<- ReadAotData(input$DownloadDate,"data/")
    req(SRAot$DWS)
    print(paste0("Data loaded",as.character(input$DownloadDate),as.character(SRAot$DWS)))
    vq <- SRAot$data[,.(.N),by=.(parameter)]
    updateSelectInput(session,"AotField", choices = as.character(vq$parameter))
  })
  #visualize aot
  UpdateSelectedResult<-eventReactive(input$VisAot, {
    
    req(SRAot$DWS)
    SearchCode<<-paste0("SRAot$data[parameter == '",input$AotField,"' & !is.na(value_raw),.(median(as.numeric(as.character(value_hrf)))),by = .(node_id)]")
    SearchResult<<-eval(parse(text = SearchCode))
    #join the data 2 the points proxy the leaflet
    AotNodes_vis <<- merge(AotNodes,SearchResult,by.x = "node_id", by.y = "node_id")
    RefreshMap()
    
    SearchResult
    
  })
  output$visAot_Text <- DT::renderDataTable(UpdateSelectedResult())
  # aot logic end ----
  # EPA PANEL LOGIC ---------------------------------------------------------
  output$epa_panel_global_plotly <- renderPlotly({
    plot_ly(x = x.si[[input$epa_panel_time]], y = y.si[[input$epa_panel_time]], z = ~z.si[[input$epa_panel_time]]) %>% add_surface()
  })
  output$epa_trace <- renderPlotly({
    index <- which(input$epa_trace_radio == c("CO","Pb","NO2","Ozone","PM10","PM2.5","SO2"))
    epa_date <- as.Date(names(epadata[[index]])[2:(ncol(epadata[[index]])-2)],format = "%m/%d/%Y")
    p<-plot_ly( type = 'scatter', mode = 'markers') 
    for(i in 1:nrow(epadata[[index]])){
      thisy<- t(as.matrix(epadata[[index]][i,2:(ncol(epadata[[index]]) - 2)]))
      p <-add_trace(p,
                    x = epa_date,
                    y = thisy[,1], 
                    mode = 'markers',
                    name = epadata[[index]][i,1])
    }
    p
  })
  
  # Home Page logic ---------------------------------------------------------
  output$MainInfPlot <- renderPlotly({
    thisnut <- HPR()
    #name of the community 
    CPTC.namecompared
    ExtractOneRow <- infTable[regionid,]
    #plot a fixed informatin for one given community ordered as:
    #-h
    #air pollution pm2.5 aod
    #road emission ??
    #covariate - elevate greeness temperture rainfall
    IFT <- infTable
    fixb_epa <- "epa"
    fixb_aod <- "aod"
    fixb_bar_color <- 'rgba(38, 24, 74, 0.8)'
    fixb_bar_color2 <- 'rgba(248, 248, 249)'
    fieldsn <- 9
    
    qpal <- colorQuantile(
      palette = "Blues",
      domain = c(0,1))
    
    p<-plot_ly(ExtractOneRow,color = "red", alpha = 0.5)
    m <- list(
      l = 160,
      r = 10,
      b = 10,
      t = 40,
      pad = 0
    )
    for(i in 1:fieldsn){
      thisid <- i+22
      positive<-MSB$isPositive[which(MSB$FieldName ==fixb_epa )]
      fixb_epa <- names(infTable)[thisid]
      fixb_name <- MSB$`Name of attributes`[which(MSB$FieldName ==fixb_epa )]
      fixb_max <- max(infTable[[fixb_epa]],na.rm = T)
      fixb_avg <- mean(infTable[[fixb_epa]],na.rm = T)/fixb_max
      
      thisx <- ifelse(is.na(ExtractOneRow[[fixb_epa]]/fixb_max)
                      ,0,ExtractOneRow[[fixb_epa]]/fixb_max)
      fixb_bar_color <- ifelse((thisx>fixb_avg&&positive=="T") |(thisx<fixb_avg&&positive=="F" ),
                               "#00ff80","orange")
      fixb_bar_color2 <- fixb_bar_color
      p<-add_bars(p,
                  orientation = 'h',
                  name = CPTC.name,
                  width = 0.2,
                  text = (ExtractOneRow[[fixb_epa]]),  hoverinfo = 'text',
                  marker = list(color = fixb_bar_color,
                                line = list(color = fixb_bar_color2)),
                  x = thisx , y = fixb_name)
      p<-add_annotations(p,"City", x = fixb_avg , y = fixb_name, ax =0, arrowwidth = 1,arrowcolor = "#B0E0E6",
                         showarrow = T,arrowhead = 4, ay = -20, arrowsize = 0.5, yshift = -8,
                         font = list(color = '#264E86',
                                     family = 'arial',
                                     size = 7),
                         arrowside = 'none', bordercolor = "grey50")
      if(CPTC){
        RExtractOneRow <- infTable[CPTC.regionid,]
        fixb_compared <- ifelse(is.na(RExtractOneRow[[fixb_epa]]/fixb_max)
                                ,0,RExtractOneRow[[fixb_epa]]/fixb_max)
        p<-add_annotations(p,CPTC.name, x = fixb_compared , y = fixb_name, ax =0, arrowwidth = 1,arrowcolor = "#B0E0E6",
                           showarrow = T,arrowhead = 4, ay = -24, arrowsize = 0.5, yshift = -8,
                           font = list(color = '#264E86',
                                       family = 'Arial',
                                       size = 7),
                           arrowside = 'none',
                           yanchor = "bottom")
        # p<-add_bars(p,
        #             orientation = 'h',
        #             name = CPTC.namecompared,
        #             width = 0.2,
        #             text = (RExtractOneRow[[fixb_epa]]),  hoverinfo = 'text',
        #             marker = list(color = "grey50",
        #                           line = list(color = "grey50")),
        #             x = fixb_compared , y = fixb_epa)
      }
    }
    p<-p %>% layout(showlegend = F, bargap = 0.3, margin = m, 
                    barmode = "group",
                    xaxis = list(type = "linear",zeroline = F, 
                                 showline = F,showgrid = F,showticklabels = F), 
                    yaxis = list(type = "category", color = "grey50"))
    p
    # subplot(plots,nrows = fieldsn,shareX = T,margin =  0.02)
  })
  observe({
    # print(input$CPTCB)
    CPTC <<- input$CPTCB
    output$CN <-  renderText({ ifelse(CPTC,
                                      paste0("⬆️",CPTC.name,"  V.S.  ",CPTC.namecompared,"⬇️"),
                                      CPTC.name)})
  })
  output$HLM <- renderLeaflet({
    p<-leaflet() %>% 
      addTiles(urlTemplate = BaseMapStyle) %>% 
      flyTo(lng = -87.6298, lat = 41.8781, 10) 
    # APPENDMAP()
    p
    
  })
  APPENDMAP <-function(){
    thispal <-AirQ_PAL(domain = ChicagoBoundary$epa)
    leafletProxy('HLM')%>% 
      addPolygons(data = ChicagoBoundary,
                  color = "#008080",
                  opacity = 0.4,
                  fillColor = thispal(ChicagoBoundary$epa),
                  weight = 1,
                  smoothFactor = 0.9,
                  fillOpacity  = 0.2, stroke = T,
                  highlight = highlightOptions(
                    fillColor = "#FF8C00",
                    fillOpacity = 1,
                    bringToFront = TRUE,
                    sendToBack = T),
                  label = labels,group = 'pg',layerId = ~community)
  }
  observe({
    click<-input$HLM_shape_click
    if(is.null(click))
      return()
    # ChicagoBoundary
  })
  observe({
    zoom<-input$HLM_zoom
    if(!is.null(zoom)){
      if(zoom >6)
        APPENDMAP()
    }
    
  })
  HPR <- reactive({
    click<-input$HLM_shape_click
    if(is.null(click))
      click$id <- "Hyde Park"
    
    # if(CPTC){
    #   CPTC.name <<- click$id
    #   output$CN <-  renderText({ CPTC.name  })
    # }
    #find my id by name
    if(!CPTC||CPTC.originstory==""){
      CPTC.name <<- click$id
    }
    CPTC.namecompared <<- click$id
    output$CN <-  renderText({ ifelse(CPTC,
                                      paste0("⬆️",CPTC.name,"  V.S.  ",CPTC.namecompared,"⬇️"),
                                      CPTC.name)})
    
    
    
    
    
    # click$id<-replace(click$id,'\'','')
    click$id<-ifelse(click$id == "OHARE","O'Hare",click$id)
    regionid<<- which(toupper(infTable[[CN]])==toupper(click$id))
    if(!CPTC||CPTC.regionid ==""){
      CPTC.regionid <<-regionid 
    }
    # storyname<<-FindtheStory(regionid,BestStory_n,infTable,ncol,ChicagoBoundary.NROW,rankmatrix)
    # thisstory$longstory <- NULL
    # if(!CPTC||storyname==""){
    resultstory <- FindtheStory(regionid,BestStory_n,infTable,ncol,ChicagoBoundary.NROW,rankmatrix,CPTC,CPTC.rankidtable)
    storyname<<-resultstory$result
    
    CPTC.rankidtable <<-resultstory$CPTC.rankidtable 
    # }
    
    wholestory<-GenrateStoryBoards(storyname,`Community Area Name`,ChicagoBoundary.NROW,MSB)
    # for(i in 1:BestStory){
    #   thisstory$longstory[i]<-CreateDescription(StoryItem = thisstory,i = i)}
    
    #search for the story
    #create story one by one 
    return(wholestory)
  })
  output$inf1 <- renderInfoBox({
    CreateInfbox(1)
    
  })
  output$inf2 <- renderInfoBox({
    CreateInfbox(2)
  })
  output$inf3 <- renderInfoBox({
    CreateInfbox(3)
  })
  
  CreateInfbox <- function(id){
    second<-ifelse(is.null(input$CPTC.checkbox),F,input$CPTC.checkbox)
    thisinut<-HPR()
    if(!CPTC||CPTC.originstory==""){
      CPTC.originstory<<-thisinut
    }
    if(id>3){
      id <- id-3
      INFB<-infoBox(thisinut$FieldName[id],
                    value = paste0(thisinut$Value[id],thisinut$unit[id]),
                    subtitle =  thisinut$Subtitle[id],
                    icon = AirQGetIcon(thisinut$Icon[id]),
                    color = thisinut$Color[id],
                    fill = T,
                    width = 12
      )
    }else{
      INFB<-infoBox(CPTC.originstory$FieldName[id],
                    value = paste0(CPTC.originstory$Value[id],CPTC.originstory$unit[id]),
                    subtitle =  CPTC.originstory$Subtitle[id],
                    icon = AirQGetIcon(CPTC.originstory$Icon[id]),
                    color = CPTC.originstory$Color[id],
                    fill = T,
                    width = 12)
      
    }
    
    return(INFB)
    
  }
  output$inf4 <- renderInfoBox({
    CreateInfbox(4)
  })
  
  output$inf5 <- renderInfoBox({
    CreateInfbox(5)
  })
  
  output$inf6 <- renderInfoBox({
    CreateInfbox(6)
  })
  
  # pm logic -----
  # here is a trial of the uioutput
  
  
  observeEvent(input$EPAT,{
    #find and vis epa plot
    # nowdate <- as.Date(input$EPAT)
    # # req(InterpResultList)
    # p2<-InterpResultList[date == paste0(year(nowdate),"-",month(nowdate),"-01") & year == 0]
    # # print(input$EPAT)
    # if(nrow(p2)>0)
    # leafletProxy("MainMap") %>%
    #   addRasterImage(p2$data[[1]],opacity=0.5,layerId = "EPA", colors=EPAPM2_5.pal)%>%
    #   addLegend(pal = EPAPM2_5.pal,values = (1:33), title = "EPA PM2.5",layerId = "EPALegend")
    
  })
  
  RefreshEPASurface <- reactive({
    
    nowdate <- as.Date(input$EPAT)
    # req(InterpResultList)
    if(input$EPAYM == 1)
    {
      p2<-InterpResultList[year(date) == year(nowdate) & year == input$EPAYM]
    }
    else
    {
      p2<-InterpResultList[date == paste0(year(nowdate),"-",month(nowdate),"-01") & year == input$EPAYM]
    }
    
    if(nrow(p2)==0)
      p2<-InterpResultList[date == "2015-02-01" & year == 0]
    p2$data[[1]]
    
  })
  
  observeEvent(input$IniEPA,{
    #initialize the epa data / creat plots
  })
  output$MainMap <- renderLeaflet({
    
    m1<-leaflet() %>% 
      addTiles(urlTemplate = BaseMapStyle) %>% 
      setView(lng = -87.6298, lat = 41.8781, 11) %>%  
      addRasterImage(RefreshEPASurface(),opacity=0.5,layerId = "EPA", colors=EPAPM2_5.pal)%>%
      addLegend(pal = EPAPM2_5.pal,values = (1:33), title = "EPA PM2.5",layerId = "EPALegend") %>% 
      addCircleMarkers(data = EPANode,opacity = ifelse(input$EPASiteOn,1,0),fillOpacity = ifelse(input$EPASiteOn,0.2,0)) %>% 
      leaflet::addPolygons(data = ChicagoBoundary, 
                           color = "darkslategray",  opacity = 0.7, stroke = T,weight = 0.2, fillOpacity = 0.1,
                           highlight = highlightOptions(color = "#666",fillOpacity = 0.7,bringToFront = F,sendToBack = T),
                           label = labels)
    m1
    # EPASiteOn  %>% 
    
  })
  
  
  # pm logic end-----
  #AOD start
  
  #Generate AOD map
  output$aodmapquarterly <- renderLeaflet({
    in.time <- input$AODT
    in.time <- as.Date(in.time)
    
    in.time <- floor_date(in.time, "month") #Round to first of month (easier w names) 
    
    alt.time <- paste("X", as.character(in.time), sep = "")
    alt.time <- gsub(pattern = "-", replacement = ".", alt.time)
    print(alt.time)
    
    qtr.names <- names(aod.quarterly)
    selected.qtr <- which(qtr.names == alt.time)
    
    a <- leaflet() %>%
      addTiles(urlTemplate = BaseMapStyle) %>%
      setView(lng = -87.6298, lat = 41.8781, 9) %>%
      addRasterImage(aod.quarterly[[selected.qtr]], opacity = 0.4, colors = monthly.aod.pal) %>%
      leaflet::addLegend(pal = monthly.aod.pal, values = values(aod.quarterly[[selected.qtr]])) %>%
      addPolygons(data = ChicagoBoundary, 
                  color = "darkslategray",
                  fillOpacity  = 0.01, 
                  stroke = TRUE,
                  opacity = 1,
                  weight = 1,
                  highlight = highlightOptions(
                    # weight = 5,
                    color = "#666",
                    # dashArray = "",
                    fillOpacity = 0.3),
                  # bringToFront = TRUE),
                  label = labels)
    
  })
  
  output$aodmapoverall <- renderLeaflet({
    a <- leaflet() %>%
      addTiles(urlTemplate = BaseMapStyle) %>%
      setView(lng = -87.6298, lat = 41.8781, 9) %>%
      addRasterImage(aod.average, opacity = 0.4, colors = monthly.aod.pal) %>%
      leaflet::addLegend(pal = monthly.aod.pal, values = values(aod.average)) %>%
      addPolygons(data = ChicagoBoundary, 
                  color = "darkslategray",
                  fillOpacity  = 0.01, 
                  stroke = TRUE,
                  opacity = 1,
                  weight = 1,
                  highlight = highlightOptions(
                    # weight = 5,
                    color = "#666",
                    # dashArray = "",
                    fillOpacity = 0.3),
                  # bringToFront = TRUE),
                  label = labels)
  })
  
  #AOD End
  
  output$working_map <- renderLeaflet({
    
    Chicagoshp <- readOGR("./data","Chicago")
    PointsShp<- readOGR("./data", "NOAASensorsShp")
    Data = read.csv('./data/NOAA_master_yearly.csv')
    DataMonthly = read.csv('./data/NOAA_master_monthly_final.csv')
    Merged = merge(PointsShp, Data, by.x='STATION', by.y='STATION')
    MergedMonthly = merge(PointsShp, DataMonthly, by.x='STATION', by.y='STATION' )
    title = paste(input$type,input$year, sep=" ")
    
    key = c('Temperature Min', 'Temperature Max', 'Precipitation', 'etc')
    val = c("_temp_min","_temp_max","_precip")
    typee = val[which(key==input$type)]
    
    if(input$monthOrYear=='Yearly')
    {
      data_point = paste("X",input$year,typee,"_yr" ,sep="")
      working_map = tm_shape(Chicagoshp)+tm_borders(alpha=1)+
        tm_shape(Merged)+ tm_dots(alpha =1, title=title,col = data_point, size = data_point,border.col="black",border.lwd=0.1,border.alpha=0.4, style = "quantile", palette = "Reds")
    }
    else
    {
      title = paste(input$type,input$month,input$year, sep=" ")
      data_point = paste("X",input$year,"_",input$month,typee,"_mo" ,sep="")
      working_map = tm_shape(Chicagoshp)+tm_borders(alpha=1)+
        tm_shape(MergedMonthly)+ tm_dots(alpha =1, title=title,col = data_point, size = data_point,border.col="black",border.lwd=0.1,border.alpha=0.4, style = "quantile", palette = "Reds")
    }
    
    tmap_leaflet(working_map)})
  #ChicagoDemographicMap -----------
  output$demographic_map <- renderLeaflet({
    demographic_data <- st_read("data/chicago_demographic.shp")
    key <- c("Percent of Crowded Housing",
             "Percent of Households Below Poverty",
             "Percent Unemployed",
             "Percent Without a High School Diploma",
             "Per Capita Income",
             "Hardship Index")
    val = c("PERCENT_O",
            "PERCENT_H",
            "PERCENT_AG",
            "PERCENT__1",
            "PER_C",
            "HARDS")
    type.col = val[which(key==input$demographic_type)]
    demographic_map <- 
      tm_shape(demographic_data) +
      tm_fill(col = type.col,
              style = "quantile",
              title = "test"
      )  +
      tm_borders() 
    # tm_layout(title = "Demographic Data by Community Area 2008-2012", title.position = c("right","bottom"))
    LF<-tmap_leaflet(demographic_map)
    LF
  })
  output$elevation_map <- renderLeaflet({
    ChicagoBoundary <- st_read("data/Chicago.shp")
    elevation_data <- raster("chicago_elevation.tif")
    elevation_map <- tm_shape(ChicagoBoundary) +
      tm_borders() +
      tm_shape(elevation_data) +
      tm_raster() 
    elev_map <- tmap_leaflet(elevation_map)
    elev_map
    
  })
  
  #### NDVI Start ####
  
  output$ndvimap <- renderLeaflet({
    in.ndvi.time <- input$NDVIT
    
    in.ndvi.time <- as.Date(in.ndvi.time)
    
    in.ndvi.time <- floor_date(in.ndvi.time, "quarter") #Round to first of month (easier w names) 
    
    alt.ndvi.time <- paste("X", as.character(in.ndvi.time), sep = "")
    alt.ndvi.time <- gsub(pattern = "-", replacement = ".", alt.ndvi.time)
    
    ndvi.names <- names(ndvi.quarterly)
    selected.ndvi.qtr <- which(ndvi.names == alt.ndvi.time)
    
    a <- leaflet() %>%
      leaflet::addTiles(urlTemplate = BaseMapStyle) %>%
      setView(lng = -87.6298, lat = 41.8781, 9) %>%
      leaflet::addRasterImage(ndvi.quarterly[[selected.ndvi.qtr]], opacity = 0.4, colors = ndvi.pal) %>%
      leaflet::addLegend(pal = ndvi.pal, values = values(ndvi.quarterly[[selected.ndvi.qtr]])) %>%
      leaflet::addPolygons(data = ChicagoBoundary, 
                           color = "darkslategray",
                           fillOpacity  = 0.01, 
                           stroke = TRUE,
                           opacity = 1,
                           weight = 1,
                           highlight = highlightOptions(
                             # weight = 5,
                             color = "#666",
                             # dashArray = "",
                             fillOpacity = 0.3),
                           # bringToFront = TRUE),
                           label = labels)
    
  })
  
  #### NDVI END ####
  
  
  #### LAND COVER START ####
  output$lcgreenmap <- renderLeaflet({
    grn.map <- tm_shape(lc.map) +
      tm_borders() +
      tm_fill(col = "grn_ndx", 
              style = "jenks", 
              palette = "Greens",
              alpha = 0.5,
              title = "Green Index")
    grn.map <- tmap_leaflet(grn.map) 
  })
  
  output$lcbluemap <- renderLeaflet({
    blu.map <- tm_shape(lc.map) +
      tm_borders() +
      tm_fill(col = "blu_ndx", 
              style = "jenks", 
              palette = "Blues", 
              alpha = 0.5,
              title = "Blue Index")
    blu.map <- tmap_leaflet(blu.map) 
  })
  
  output$lcgraymap <- renderLeaflet({
    gry.map <- tm_shape(lc.map) +
      tm_borders() +
      tm_fill(col = "gry_ndx", 
              style = "jenks", 
              palette = "Greys", 
              alpha = 0.5,
              title = "Gray Index")
    gry.map <- tmap_leaflet(gry.map) 
  })
  
  
  #### LAND COVER END ####
  
  #Chicago health map
  output$health_map <- renderLeaflet({
    health_data <- st_read("data/HealthIndicators.shp")
    key <- c("Birth Rate",
             "Low Birth Rate",
             "Teen Birth Rate",
             "Lung Cancer",
             "Cancer",
             "Tuberculosis"
    )
    val = c("BirthRate",
            "LowBi_ight",
            "TeenB_Rate",
            "LungCancer",
            "Cance_ites",
            "Tuber_osis")
    type.col1 = val[which(key==input$health_type)]
    health_map <- 
      tm_shape(health_data) +
      tm_fill(col = type.col1,
              style = "quantile",
              title = input$health_type
      )  +
      tm_borders() +
      tm_layout(title = "Chicago Public Health", title.position = c("right","bottom"))
    tmap_leaflet(health_map)
  })
  # road_emissions output ----
  output$road_emissions_map <- renderLeaflet({
    trffc_vol <- readOGR("./data", "Community_Areas_with_Traffic_Volumes")
    road_length <- readOGR("./data", "Chi_Road_Lengths")
    
    trffc_vol@data$Trffc_V_area <- 
      trffc_vol@data$Trffc_V / trffc_vol@data$shape_r
    road_length@data$rd_lngt_area <- 
      road_length@data$rd_lngt / road_length@data$shape_r
    
    trffc_vol@data <- select(trffc_vol@data, -perimtr)
    road_length@data <- select(road_length@data, -perimtr)
    
    road_emission_data <- switch(input$road_emit_type, 
                                 "Traffic Volume" = "trffc_vol", 
                                 "Road Lengths" = "road_length")
    
    if(input$road_emit_type == "Traffic Volume")
    {
      road_col <- "Trffc_V_area"
      road_emissions_map <- 
        tm_shape(trffc_vol) + 
        tm_borders(alpha = 0.4) +
        tm_fill(col= road_col, 
                style = "jenks",
                n = 5,
                bolder.lwd = 0.1, 
                bolder.alpha = 0.4, 
                palette = "Reds", 
                title = "Traffic Volume per Square Foot" ) +
        tm_layout(
          main.title = "Traffic Volume by Community Area of Chicago",
          main.title.size = 1.2
        )
    }
    if(input$road_emit_type == "Road Lengths")
    {
      road_col <- "rd_lngt_area"
      road_emissions_map <- 
        tm_shape(road_length) + 
        tm_borders(alpha = 0.4) +
        tm_fill(col= road_col, 
                style = "jenks",
                n = 5,
                bolder.lwd = 0.1,
                bolder.alpha = 0.4, 
                palette = "Reds", 
                title = "Total Length of Road per Square Foot") +
        tm_layout(
          main.title = "Road Lengths by Community Area of Chicago",
          main.title.size = 1.2
        )
    }
    tmap_leaflet(road_emissions_map)
  })
  
  # factory_emissions output ----
  output$factory_emissions_map <- renderLeaflet({
    factory <- readOGR("./data", "Community_Areas_with_Factory_Distances")
    factory_emissions_map <-
      tm_shape(factory) +
      tm_fill(col = "min_dist", 
              style = "jenks", 
              n = 5,
              bolder.lwd = 0.1,
              bolder.alpha = 0.4,
              palette = "Reds",
              title = "Distance (km)") +
      tm_layout(
        main.title = "Distance from Community Area Centroid to Nearest Point Emissions Source", 
        main.title.size = 1.05
      )
    
    
    tmap_leaflet(factory_emissions_map)
  })  
  
  
  
  #expepa-----
  output$CommunityName <- renderText({
    ThisHighLight <- HPR1()
    # if(is.null(ThisHighLight)){
    #   
    # }
    ThisHighLight
  })
  
  output$EPA_Description <- renderText({
    #the check epa source part is repeated create it as a function!
    index <- which(input$epa_panel_checkbox == epa_panel.airpollutiontype)
    typename <- epa_panel.airpollutiontype[index]
    
    EPA.Description[V1==typename]$V2
    
  })
  #load map in the main pannel
  output$EPANMAP <- renderLeaflet({
    p<-leaflet(height = 1000) %>% 
      addTiles(urlTemplate = BaseMapStyle) %>%
      flyTo(lng = -87.6298, lat = 41.8781, 10 ,options = list(duration = 5, easeLinearity = 0.1)) 
    # APPENDMAP()
    p
  })
  #reactive to click event on the map
  HPR1 <- reactive({
    click<-input$EPANMAP_shape_click
    if(is.null(click)){
      if(!is.null(ChicagoBoundary))
        return(as.character(ChicagoBoundary$community[1]))}
    return(click$id)
  })
  #when the flyto is about stopping load the map
  observe({
    zoom <-input$EPANMAP_zoom
    ThisHPR<-HPR1()
    if(!is.null(ThisHPR))
    {
      index <- which(ChicagoBoundary$community == ThisHPR)
      thiscolor <- rep("grey",77)
      thiscolor[index] <- ChicagoAirColor$highlight
    }
    
    if(!is.null(zoom)){
      if(zoom > 6){
        if(is.null(ThisHPR)){
          leafletProxy('EPANMAP') %>% 
            addPolygons(data = ChicagoBoundary, color= ChicagoAirColor$default, opacity = 1, weight = 1, fillOpacity = 0.2,smoothFactor = 0.9,
                        layerId = ~community)
          
        }else
        {
          leafletProxy('EPANMAP') %>% 
            addPolygons(data = ChicagoBoundary,color= thiscolor, opacity = 1, weight = 1, fillOpacity = 0.2,smoothFactor = 0.9,
                        layerId = ~community)
        }
      }
      # APPENDMAP()
      
    }
  })
  
  #plotly
  output$EPAPlotlyChart <- renderPlotly({
    # plotly() %>% 
    ThisHighLight <- HPR1()
    # print(ThisHighLight)
    index <- which(input$epa_panel_checkbox == epa_panel.airpollutiontype)
    p <- EPA.ProloadPlotlychart$Chart[[index]]
    typename <- epa_panel.airpollutiontype[index]
    # p<-CreateTracePlotForOnePollutatn(EPAMetaF = EPAMetaF, ChicagoEPA =  ChicagoEPA, typename = "Ozone")
    if(!is.null(ThisHighLight))
    {
      if(ifmonth)
      {
        extractedcol<-EPAMetaF[Subtype == typename & Month != 0 & Valid,]
      }else{
        extractedcol<-EPAMetaF[Subtype == typename & Month == 0 & Valid,]
      }
      extractedcol$Date <-  as.Date(extractedcol$Date)
      extractedcol <- extractedcol[order(Date)]
      #plotly::style() use plotly style to change the style
      extractedvalue <- as.numeric(as.matrix(ChicagoEPA[COMMUNITYNAME == ThisHighLight])[extractedcol$ID])
      p <- add_lines(EPA.ProloadPlotlychart$Chart[[index]] ,x = extractedcol$Date, y = extractedvalue ,name = ThisHighLight, color = I("#AFEEEE"),
                     line = list(shape = "spline"))
    }
    p
  })
  
  output$EPAPieChart_MaximumDailyCut <- renderPlotly({
    
    labels <- c("Polluted","Good Weather")
    values <- c("0.2","0.8")
    plot_ly(labels = labels, values = values, marker = list(colors = c(ChicagoAirColor$default,ChicagoAirColor$highlight))) %>%
      add_pie(hole = 0.6) %>% 
      layout(showlegend = F,margin = m,paper_bgcolor= ChicagoAirColor$transparent,
             plot_bgcolor= ChicagoAirColor$transparent)
  })
  
  output$EPAPieChart_DataRealiability <- renderPlotly({
    input <- which(input$epa_panel_checkbox == epa_panel.airpollutiontype)
    thinr <- runif(1)
    labels <- c("Missing Data","Validate Data")
    values <- c(thinr,1-thinr)
    plot_ly(labels = labels, values = values) %>%
      add_pie(hole = 0.6) %>% 
      layout(showlegend = F,margin = m, 
             paper_bgcolor= ChicagoAirColor$transparent,
             plot_bgcolor= ChicagoAirColor$transparent)
  })
}

shinyApp(ui = ui, server=server)


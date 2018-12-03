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


health <- st_read("data/HealthIndicators.shp")


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

EPANode <- st_read(paste0(datapath,"PM2.5YearlyShapefile1.shp"))
EPAPM2_5.breaks <- c(1:10,12,15,35) #Breaks for EPAPM2_5
EPAPM2_5.pal <- colorNumeric(c("#D73027", "#FC8D59", "#D9EF8B", "#FEE08B", "#91CF60", "#1A9850"), 
                             EPAPM2_5.breaks, na.color = "transparent",reverse = T) #Palette for EPAPM2_5

aod.yearly <- stack("./data/Yearly_Aod_Stack_Reproj.tif") #Yearly AOD Data
names(aod.yearly) <- c("2014", "2015", "2016", "2017") 
aod.average <- raster("./data/AOD_Average_4_Year_Reproj.tif")

#Manually fix faulty value
values(aod.average)[which(values(aod.average) == 0)] <- NA

aod.monthly.names <- read.csv("./data/aod.monthly.names.csv")
aod.monthly <- stack("./data/AOD_Monthly_Avgs.tif")

names(aod.monthly) <- aod.monthly.names$x

monthly.breaks <- seq(from = 0, to = 1, 0.1) #Breaks for AOD data
monthly.aod.pal <- colorNumeric(c("green", "yellow", "red", "purple"), monthly.breaks, na.color = "transparent") #Palette for AOD

yearly.breaks <- seq(from = 0.1, to = 0.3, 0.02) #Breaks for AOD data
yearly.aod.pal <- colorNumeric(c("green", "yellow", "red", "purple"), yearly.breaks, na.color = "transparent") #Palette for AOD


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
                         menuSubItem("Weather", "noaa"),
                         menuSubItem("Traffic", "road_emissions")),
                menuItem("Population Measures",
                         menuSubItem("Demographic Data", tabName = "demographic"),
                         menuSubItem("Public Health", tabName = "health")),
                menuSubItem("Explore Array of Things", tabName = "aot"),
                menuSubItem("Explore EPA Stations","epa_panel")
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
              h1("About Open Air Chicago"),
              br(),
              p("This project is"),
              br(),
              h3("Dashboard"),
              p(),
              h3("Data"),
              p(),
              h4("Monitoring Data"),
              h4("Aerosol Optical Depth (AOD) Data"),
              h4("Covariate Data"),
              h3("Analysis"),
              p()
              
      ),
      #tabitem noaa ----
      tabItem(tabName = "noaa",
              
              fluidRow(
                box(
                  width = 4,
                  selectInput(inputId="type", label="Choose type of data",c('Temperature Min', 'Temperature Max', 'Precipitation','Sensor locations')),
                  sliderInput(inputId="year",label = "Choose a year",
                              value = 2012, min=2012,max=2018),
                  selectInput(inputId="monthOrYear",  label="Choose Monthly or Yearly",c('Yearly', 'Monthly')),
                  conditionalPanel(
                    condition = "input.monthOrYear == 'Monthly'",
                    
                    sliderInput(inputId="month",label = "Choose a month",
                                value = 1, min=1,max=12),
                    
                    selectInput("var", 
                                label = "Choose a medical condition:",
                                choices = c("Asthma Cases",
                                            "Diabetes Cases",
                                            "Depression Cases"),
                                selected = "Asthma Cases") 
                  ) ),
                box(
                  width = 8,
                  leafletOutput("working_map")
                )
              ),
              fluidRow(
                plotOutput("graph")
              )
      ),
      #tabitem aod
      tabItem(tabName = "aod",
              box(
                h1("Aerosol Optical Depth (AOD)"),
                width = 4,
                selectInput(inputId = "selecttime",
                            label = "Overall, Yearly, or Monthly Averages?",
                            choices = c("Overall", "Yearly",
                                        "Monthly")),

              br(),
              h4("About AOD"),
              p("Aerosol optical depth is a measure of the extinction of the solar beam by dust 
                and haze. In other words, particles in the atmosphere (dust, smoke, pollution) 
                can block sunlight by absorbing or by scattering light."),
              h4("Data Source"),
              p("We use data directly from NASA. The Moderate Resolution Imaging Spectroradiometer 
                (MODIS) satellite provides daily global coverage, but the 10 km resolution of its 
                aerosol optical depth (AOD) product is not suitable for studying spatial variability 
                f aerosols in urban areas. Recently, a new Multi-Angle Implementation of Atmospheric 
                Correction (MAIAC) algorithm was developed for MODIS which provides AOD at 1 km 
                resolution.")

              ),
              box(
                width = 8,
                conditionalPanel(condition = "input.selecttime == 'Yearly'", 
                sliderInput("aodyear", "Select Year:",
                            min = strptime("2014/01/04","%Y/%m/%d"), 
                            max = strptime("2018/09/18","%Y/%m/%d"),
                            value = strptime("2014/01/04","%Y/%m/%d"),
                            timeFormat = "%Y/%m",
                            step = as.difftime(365, units = "days"),
                            animate = animationOptions(interval = 500))
                ),
                conditionalPanel(condition = "input.selecttime == 'Monthly'", 
                                 sliderInput("aodmonth", "Select Month:",
                                             min = strptime("2014/01/01","%Y/%m/%d"), 
                                             max = strptime("2018/09/18","%Y/%m/%d"),
                                             value = strptime("2014/01/01","%Y/%m/%d"),
                                             timeFormat = "%Y/%m",
                                             step = as.difftime(30 ,units = "days"),
                                             animate = animationOptions(interval = 500))
                                 
              ),
              conditionalPanel(condition = "input.selecttime == 'Yearly'",
                               box(
                                 width = 8,
                                 leafletOutput("aodmapyearly")
                               )),
              conditionalPanel(condition = "input.selecttime == 'Monthly'",
                               box(
                                 width = 8,
                                 leafletOutput("aodmapmonthly")
                               )),
              conditionalPanel(condition = "input.selecttime == 'Overall'",
                               box(
                                 width = 8, height = 600,
                                 leafletOutput("aodmapoverall")
                               ))
              
      )),
      
      
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
                  leafletOutput("demographic_map")
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
                  leafletOutput("health_map")
                )
              )
      ),
        
      #tabitem road_emissions ----
      tabItem(tabName = "road_emissions",
              fluidRow(
                box(
                  width = 4,
                  selectInput(inputId="road_emit_type", 
                              label="Choose type of data",
                              c("Traffic Volume", "Road Lengths")
                  )
                ),
                box(
                  width = 8,
                  leafletOutput("road_emissions_map")
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
    
    leaflet() %>% 
      addTiles(urlTemplate = BaseMapStyle) %>% 
      setView(lng = -87.6298, lat = 41.8781, 11) %>%  
      addRasterImage(RefreshEPASurface(),opacity=0.5,layerId = "EPA", colors=EPAPM2_5.pal)%>%
      #addPolygons(data = Chicagoshp, color = "darkslategray",fillOpacity  = 0., stroke = TRUE,
      #            highlight = highlightOptions(
                    # weight = 5,
      #              color = "#666",
                    # dashArray = "",
      #              fillOpacity = 0.7,
      #              bringToFront = TRUE),
      #            label = labels,layerId = "Chicago")%>% 
      addLegend(pal = EPAPM2_5.pal,values = (1:33), title = "EPA PM2.5",layerId = "EPALegend") %>% 
      addCircleMarkers(data=EPANode,opacity = ifelse(input$EPASiteOn,1,0),fillOpacity = ifelse(input$EPASiteOn,0.2,0))
    
    # EPASiteOn  %>% 
    
  })
  
  
  # pm logic end-----
  #AOD start
  
  #Generate AOD map
  output$aodmapyearly <- renderLeaflet({
    in.timeyr <- input$aodyear
    in.time <- as.POSIXct(in.timeyr, origin="1970-01-01")
    yr <- year(in.time)
    selected.yr <- paste("X", yr, sep = "")
    
    yr.names <- names(aod.yearly)
    selected.yr <- which(yr.names == selected.yr)
    
    a <- leaflet() %>% 
      addTiles() %>% 
      addRasterImage(aod.yearly[[selected.yr]], opacity = 0.7, colors = yearly.aod.pal) %>% 
      leaflet::addLegend(pal = yearly.aod.pal, values = values(aod.yearly[[selected.yr]])) %>% 
      addPolygons(data = ChicagoBoundary, color = "darkslategray",fillOpacity  = 0.01, stroke = FALSE,
                  highlight = highlightOptions(
                    # weight = 5,
                    color = "#666",
                    # dashArray = "",
                    fillOpacity = 0.3),
                  # bringToFront = TRUE),
                  label = labels)

  })

  output$aodmapmonthly <- renderLeaflet({
    
    in.timemo <- input$aodmonth
    in.timemo <- as.POSIXct(in.timemo, origin="1970-01-01")
    mo <- lubridate::month(in.timemo)
    yr <- year(in.timemo)
    selected.mo <- paste(mo, yr, sep = "")
    selected.mo <- paste("X", selected.mo, sep = "")
    mo.names <- names(aod.monthly)
    selected.mo.index <- which(mo.names == selected.mo)
    
    a <- leaflet() %>% 
      addTiles() %>% 
      addRasterImage(aod.monthly[[selected.mo.index]], opacity = 0.7, colors = monthly.aod.pal) %>% 
      leaflet::addLegend(pal = monthly.aod.pal, values = values(aod.monthly[[selected.mo.index]])) %>% 
      addPolygons(data = ChicagoBoundary, color = "darkslategray",fillOpacity  = 0.01, stroke = FALSE,
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
      addTiles() %>% 
      addRasterImage(aod.average, opacity = 0.7, colors = yearly.aod.pal) %>% 
      leaflet::addLegend(pal = yearly.aod.pal, values = values(aod.average)) %>% 
      addPolygons(data = ChicagoBoundary, color = "darkslategray",fillOpacity  = 0.01, stroke = FALSE,
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
        ) +
        tm_view(view.legend.position = c("left", "bottom"))
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
        ) +
        tm_view(view.legend.position = c("left", "bottom"))
    }
    tmap_leaflet(road_emissions_map)
  })

}

shinyApp(ui = ui, server=server)


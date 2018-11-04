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
library(ggplot2)
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

library(gstat) #kriging
library(stringr) # extract date from the epa date


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

StoryBoardWidth <-4
MapBWidth <- (12 - StoryBoardWidth )
MapBHeight <- 900

BaseMapStyle <- "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png"

#initilization ----
AotNodesNonspatial <- fread("data/nodes.csv") #readAotNodes
AotNodes <- st_as_sf(AotNodesNonspatial, coords = c("lon", "lat"), crs = 4326, agr = "constant") #create points obj
ChicagoBoundary <- st_read("Chicago.shp")
AotNodes_vis <- AotNodes
Drawned <-1 #the drawned and intersected selection area. this might be infeasible for a multilayer case



aod.yearly <- stack("Yearly_Aod_Stack_Reproj.tif") #Yearly AOD Data
aod.overall <- raster("AOD_Average_4_Year_Reproj.tif") #4 year avg AOD
aod.yearly.overall <- stack(aod.yearly, aod.overall) #Stack rasters
names(aod.yearly.overall) <- c("X2014", "X2015", "X2016", "X2017", "Overall") 

aod.breaks <- c(.10, .12, .14, .16, .18, .20, .22, .24, .26, .28) #Breaks for AOD data
aod.pal <- colorNumeric(c("green", "yellow", "red", "purple"), aod.breaks, na.color = "transparent") #Palette for AOD


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

#ReadEpaData ----
#read the epa data from the project package sourced from 2018113
ReadEpaData <- function()
{
   
   readpath <- "./data-workflows/sensors/epa-sensors/"
   readpath1 <- "data/"
   filename <- "PM2.5MonthlyShapefile.shp"
   if(file.exists(paste0(readpath1,filename)))
     EPAdata <- readOGR(paste0(readpath1,filename))
   else
     EPAdata <- readOGR(paste0(readpath,filename))
   return(EPAdata)
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
#Inter Time Series data
Interp <- function(inputdatatable,timepoint,resl)
{
  datefield <- GetTimeStamp(year(timepoint),month(timepoint),"epa")
  df <- as.data.frame(inputdatatable)
  df <- as.data.frame(subset(df,(!is.na(df[[datefield]]))))
  coordinates(df) <- df[,c("SITE_LO", "SITE_LA")]
  proj4string(df) <- CRS("+init=epsg:4326")
  vgm <- variogram(df[[datefield]] ~ 1, df)
  sph<- fit.variogram(vgm, model=vgm("Sph"))
  # plot(vgm, sph)
  itpgrid <- pt2grid(df,resl)
  projection(itpgrid) <- CRS("+init=epsg:4326")  
  Itpr <- (krige(df[[datefield]] ~ 1,df,itpgrid, model=sph))
  # va<-writeGDAL(Itpr, "corrected.tif", drivername="GTiff", type="Float32") 
  return(Itpr)
}
GetTimeStamp <- function(year,month,type)
{
  # switch(type,
  #        epa = (month==1?paste0("X",year,"_",month,"_MPC"):paste0("X",year,"_",month,"MPC"))
  # )
  if(type == "epa")
  {
    if(month==1)
    {
      return(paste0("X",year,"_",month,"_MPC"))
    }
    else
    {
      return(paste0("X",year,"_",month,"MPC"))
    }
  }
}

# EPADATA<-ReadEpaData()
# plot(Interp(EPADATA,as.Date("2017-01-01"),100))

CreateINPresult<-function(){
  
  EPADATA<<-ReadEpaData()
  nl<-names(EPADATA)
  i <- 1
  
  rawdate<-strsplit(str_extract(nl[i+11], "[0-9]+_[0-9]+"),'_')
  thisdate<-as.Date(paste0(unlist(rawdate)[1],"-",unlist(rawdate)[2],"-01"))
  InterpResultList <- data.table(date = c(thisdate), field = c(nl[i+11]),itpr = c(Interp(EPADATA,thisdate,100)))
   
  for(i in 2:(length(nl)-11))
  {
    rawdate<-strsplit(str_extract(nl[i+11], "[0-9]+_[0-9]+"),'_')
    thisdate<-as.Date(paste0(unlist(rawdate)[1],"-",unlist(rawdate)[2],"-01"))
    InterpResultList<-rbind(InterpResultList,data.table(date = c(thisdate), field = c(nl[i+11]),itpr = c(Interp(EPADATA,thisdate,100))))
  }
  return(InterpResultList)

}

InterpResultList<-CreateINPresult()#IinitializedEPA
#test text in the storyboard
testtext <- "When I wrote the following pages, or rather the bulk of them, I lived alone, in the woods, a mile from any neighbor, in a house which I had built myself, on the shore of Walden Pond, in Concord, Massachusetts, and earned my living by the labor of my hands only. I lived there two years and two months. At present I am a sojourner in civilized life again.
I should not obtrude my affairs so much on the notice of my readers if very particular inquiries had not been made by my townsmen concerning my mode of life, which some would call impertinent, though they do not appear to me at all impertinent, but, considering the circumstances, very natural and pertinent. Some have asked what I got to eat; if I did not feel lonesome; if I was not afraid; and the like." 



#ui ----
ui <- dashboardPage(
  dashboardHeader(title = "Open Air Chicago"),
  dashboardSidebar(
    sidebarMenu(id = "tablist",
      menuItem("Home", tabName = "Home"),
      menuItem("About", tabName = "About"),
      menuItem("Pollution Measures",
      menuSubItem("PM 2.5", "pm"),
      menuSubItem("Aeorosol-Optical-Depth", "aod")),
      menuItem("Pollution Drivers",
      menuSubItem("Weather", "noaa"),
      menuSubItem("Traffic", "road_emissions")),
      menuItem("AoT", tabName = "aot")
    )
  ),
  dashboardBody(
    tabItems(
      #First tab content ----
      tabItem(tabName = "Home",
              h1("Home")
      ),
      #About-----
      tabItem(tabName = "About",
              h1("CSDS Air Quality Analysis Application"),
              h2("Center for Spatial Data Science"),
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
                width = 4,
                selectInput(inputId = "aodyear", 
                            label = "Year:",
                            choices = c("2014" = "X2014", 
                                        "2015" = "X2015", 
                                        "2016" = "X2016", 
                                        "2017" = "X2017", 
                                        "Overall")),
                
                checkboxInput(inputId = "outline",
                              label = "Show city boundaries?",
                              value = FALSE)
                
              ),
              box(
                width = 8,
                leafletOutput("aodmap")
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
      tabItem(
        "pm", fluidPage(
          fluidRow(
            box(width = StoryBoardWidth,title = "This is the story box",
                tags$p(testtext)),
            box(width = MapBWidth, title = "This is the map box",
                leafletOutput("MainMap"))
          ),
          fluidRow(
            box(width = StoryBoardWidth,
                sliderInput("EPAT", "EPA:",
                            min = strptime("2015/01/15","%Y/%m/%d"), 
                            max = strptime("2017/12/31","%Y/%m/%d"),
                            value = strptime("2015/01/16","%Y/%m/%d"),
                            timeFormat = "%Y/%m",
                            step = as.difftime(30,units = "days"),
                            animate = animationOptions(interval = 500)),
                actionButton("IniEPA","Initialize the EPA DATA")
            )
          )
        )
      )
      
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
  # pm logic -----
  # here is a trial of the uioutput
 
  
  observeEvent(input$EPAT,{
    #find and vis epa plot
    nowdate <- as.Date(input$EPAT)
    # req(InterpResultList)
    p2<-InterpResultList[date == paste0(year(nowdate),"-",month(nowdate),"-01")]
    # print(input$EPAT)
    if(nrow(p2)>0)
    leafletProxy("MainMap") %>%
      addRasterImage(raster(p2$itpr[[1]]),opacity=0.2,layerId = "EPA")
  })
  
  observeEvent(input$IniEPA,{
    #initialize the epa data / creat plots
  })
  output$MainMap <- renderLeaflet({
    leaflet(height = MapBHeight) %>% 
      addTiles(urlTemplate = BaseMapStyle) %>% 
      setView(lng = -87.6298, lat = 41.8781, 9) 
      
  })
  
  
  # pm logic end-----
  #AOD start
  
  #Check for outline checkbox and plot city outline if necessary
  observe({
    proxy <- leafletProxy("chicago")
    
    if (input$outline) {
      proxy %>% addPolygons(data = ChicagoBoundary, color = "black", fill = FALSE)
    }
    else {
      proxy %>% clearShapes()
    }
  })
  #Generate AOD map
  output$aodmap <- renderLeaflet({
    a <- leaflet() %>% 
      addTiles() %>% 
      addRasterImage(aod.yearly.overall[[input$aodyear]], opacity = 0.7, colors = aod.pal) %>% 
      addLegend(pal = aod.pal, values = values(aod.yearly.overall[[input$aodyear]]), title = "Aerosol Optical Depth")
    
    #Keep chicago outline rendered when year is changed 
    if (input$outline) {
      a %>% addPolygons(data = ChicagoBoundary, color = "black", fill = FALSE)
    }
    else {
      a 
    }
    
  })
  
  #AOD End
 
  output$working_map <- renderLeaflet({
    
    Chicagoshp <- readOGR(".","Chicago")
    PointsShp<- readOGR(".", "NOAASensorsShp")
    Data = read.csv('NOAA_master_yearly.csv')
    DataMonthly = read.csv('NOAA_master_monthly_final.csv')
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
  # road_emissions output ----
  output$road_emissions_map <- renderLeaflet({
    trffc_vol <- readOGR(".", "Community_Areas_with_Traffic_Volumes")
    road_length <- readOGR(".", "Chi_Road_Lengths")
    
    road_emission_data <- switch(input$road_emit_type, 
                                 "Traffic Volume" = "trffc_vol", 
                                 "Road Lengths" = "road_length")
    
    if(input$road_emit_type == "Traffic Volume")
    {
      road_col <- "Trffc_V"
      road_emissions_map <- 
        tm_shape(trffc_vol) + 
        tm_fill(col= road_col, 
                style = "jenks",
                n = 5,
                bolder.lwd = 0.1, 
                bolder.alpha = 0.4, 
                palette = "Reds", 
                title = "Traffic Volume" ) +
        tm_layout(
          main.title = "Traffic Volume by Community Area of Chicago",
          main.title.size = 1.2
        )
    }
    if(input$road_emit_type == "Road Lengths")
    {
      road_col <- "rd_lngt"
      road_emissions_map <- 
        tm_shape(road_length) + 
        tm_fill(col= road_col, 
                style = "jenks",
                n = 5,
                bolder.lwd = 0.1,
                bolder.alpha = 0.4, 
                palette = "Reds", 
                title = "Total Length of Road") +
        tm_layout(
          main.title = "Road Lengths by Community Area of Chicago",
          main.title.size = 1.2
        )
    }
    tmap_leaflet(road_emissions_map)
  })
  
}

shinyApp(ui = ui, server=server)

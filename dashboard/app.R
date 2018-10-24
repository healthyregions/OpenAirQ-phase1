library(shiny)
library("rgdal")
library("rgeos")
library(tmap)
library(leaflet)
library("tmap")
library(sp)
library(sf)
library(shinydashboard)


ui <- dashboardPage(
  dashboardHeader(title = "Test Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Homepage", tabName = "homepage"),
      menuItem("Weather", tabName = "noaa"),
      menuItem("Area-Emissions", tabName = "area_emissions"),
      menuItem("Demographic-Data", tabName = "demographic_data"),
      menuItem("Elevation", tabName = "elevation"),
      menuItem("Land-Cover", tabName = "land_cover"),
      menuItem("Land-Use", tabName = "land_use"),
      menuItem("NDVI", tabName = "ndvi"),
      menuItem("Point-Emissions", tabName = "point_emissions"),
      menuItem("Road-Emissions", tabName = "road_emissions")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "homepage",
              h1("CSDS Air Quality Analysis Application Homepage"),
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
      tabItem(tabName = "noaa",
              h1("Weather (Temperature and Precipitation)"),
              fluidRow(
                box(
                  width = 4,
                  h3("Weather (Temperature and Precipitation)"),
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

                    )


                  ),
                 
                
                box(
                  width = 8,
                  leafletOutput("working_map")
                )
              ),
              
              fluidRow(
                plotOutput("graph")
                
              )
      ),
      tabItem(tabName = "area_emissions",
        h1("Area-Emissions")
      ),
      tabItem(tabName = "demographic_data",
        h1("Demographic-Data")
        
      ),
      tabItem(tabName = "elevation",
              h1("Elevation")
        
      ),
      tabItem(tabName = "land_cover",
              h1("Land-Cover")
        
      ),
      tabItem(tabName = "land_use",
              h1("Land-Use")
        
      ),
      tabItem(tabName = "ndvi",
              h1("NDVI")
        
      ),
      tabItem(tabName = "point_emissions",
              h1("Point-Emissions")
        
      ),
      tabItem(tabName = "road_emissions",
              h1("Road-Emissions")
        
      )
        
      )
    
  )
)



server = function(input, output){
  
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
  
  
}

shinyApp(ui = ui, server=server)


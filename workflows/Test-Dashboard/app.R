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
      menuItem("NOAA", tabName = "noaa"),
      menuItem("etc", tabName = "etc")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
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


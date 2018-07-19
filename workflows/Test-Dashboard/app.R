library(shiny)
library("rgdal")
library("rgeos")
library(tmap)
library(leaflet)
library("tmap")
library(sp)
library(sf)

#Chicagoshp = readOGR(".", "Chicago")
ui= fluidPage(
  
    titlePanel("Dashboard Prototype v1.4"),
    sidebarLayout(
    sidebarPanel(
      selectInput(inputId="type", label="Choose type of data",c('Temperature Min', 'Temperature Max', 'Precipitation', 'etc')),
      sliderInput(inputId="year",label = "Choose a year",
                  value = 2012, min=2012,max=2018),
      selectInput(inputId="monthOrYear",  label="Choose Monthly or Yearly",c('Yearly', 'Monthly')),
      sliderInput(inputId="month",label = "Choose a month",
                  value = 1, min=1,max=12)
      
    ),
    mainPanel(
      leafletOutput("working_map"),
      plotOutput("graph")
    )
  )
  
  
  
  #plotOutput(outputId = "hist")
  
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
      tm_shape(MergedMonthly)+ tm_dots(alpha =1, title="Precipitation - March 2013",col = data_point, size = data_point,border.col="black",border.lwd=0.1,border.alpha=0.4, style = "quantile", palette = "Reds")
    }
   
    tmap_leaflet(working_map)})
  
  
}

shinyApp(ui = ui, server=server)


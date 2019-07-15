

### Create Tab Page for Variables with Quarterly Data
generateQuarterlyTab <- function(tabname, variablename, variabledescription, sourcedescription,
                                  mapheight = 500) 
  {
  tabItem(tabName = tabname,
          fluidRow(
            box(width = 4,
              tabsetPanel(
                tabPanel(title = "Description",
                  h3(variablename),
                  p(variabledescription)),
                tabPanel(title = "Source",
                  h4("Data Source"),
                  p(sourcedescription)))
              ),
            box(width = 8,
                sliderInput(paste(tabname, "dt", sep = "_"), "Select quarter:",
                            min = strptime("2014/01/01","%Y/%m/%d"), 
                            max = strptime("2018/12/31","%Y/%m/%d"),
                            value = strptime("2016/07/01","%Y/%m/%d"),
                            timeFormat = "%Y/%m",
                            step = as.difftime(92, units = "days"),
                            animate = animationOptions(interval = 5000)),
                leafletOutput(paste(tabname, "map", sep = "_"),height = mapheight),
                radioGroupButtons(paste(tabname, "rad", sep = "_"), "Select Color Palette", 
                             c("Overall" = "ovr", "Yearly" = "yr", "Quarterly" = "qtr"), 
                             selected = "ovr"))
              ))
}


### Create Tab Page for Variables with One Time Data
generateOneTimeTab <- function(tabname, variablename, variabledescription, sourcedescription,
                               mapheight = 500) {
  tabItem(tabName = tabname,
          fluidRow(
            box(width = 4,
              tabsetPanel(
                tabPanel(title = "Description",
                         h3(variablename),
                         p(variabledescription)),
                tabPanel(title = "Source",
                         h4("Data Source"),
                         p(sourcedescription))) 
              ),
            box(width = 8,
                leafletOutput(paste(tabname, "map", sep = "_"), height = mapheight)
            )
              ))
}

##### Date Slider Output to Raster Layer Name
getLayerName <- function(in.date, variable) {
  in.date <- as.Date(in.date)
  
  var.month <- month(in.date)
  var.yr <- year(in.date)
  
  var.qtr <- ceiling(var.month/3)
  
  var.yr <- substring(var.yr, 3, 4) 
  
  this.var.name <- paste(variable, var.qtr, var.yr, sep = "_")
}

##### Generate Leaflet Map 
dashMap <- function(layername, layerpal, raster = master.raster, area = large.area) {
  
    leaflet(layername) %>%
    addProviderTiles("Hydda.Full") %>%
    addRasterImage(raster[[layername]], opacity = 0.4, colors = layerpal) %>%
    leaflet::addLegend(pal = layerpal, values = values(raster[[layername]]), title = gsub("_.*","",layername)) %>%
    addPolygons(data = area, 
                color = "darkslategray",
                fillOpacity  = 0.01, 
                stroke = TRUE,
                opacity = 1,
                weight = 1,
                highlight = highlightOptions(
                  weight = 2, 
                  color = "gray", 
                  fillOpacity = 0.05),
                popup = paste(area["COUNTYNAME"][[1]], "County"))
}

##### For use in observe function with slider

sliderProxy <- function(mapname, layername, layerpal, raster = master.raster) {
  leafletProxy(mapname) %>%
    clearControls() %>%
    clearImages() %>%
    addRasterImage(raster[[layername]], opacity = 0.4, colors = layerpal) %>%
    leaflet::addLegend(pal = layerpal, values = values(raster[[layername]]), title = gsub("_.*","",layername))
}


#Creates palette
#qtr: Palette covers single quarter
#ovr: Palette covers entire 5 year period (input "AOD", "NDVI", etc)

palFromLayer <- function(layername, style = "ovr", colors = c("green", "yellow", "orange", "red"), 
                         raster = master.raster, nacolor = "transparent") {
  if(style == "qtr") {
    
    this.raster <- raster[[layername]]
    
    max <- raster::maxValue(this.raster)
    min <- raster::minValue(this.raster)
    
  } else if (style == "ovr") {
    layer.var <- substring(layername, 1, 3)
    
    this.raster <- raster[[which(grepl(layer.var, names(master.raster)))]]
    
    max <- max(maxValue(this.raster))
    min <- min(minValue(this.raster))
    
  } else if (style == "yr") {
    layer.var <- substring(layername, 1, 3)
    var.raster <- raster[[which(grepl(layer.var, names(master.raster)))]]

    layer.yr <- substring(layername, (nchar(layername) - 1), nchar(layername))
    this.raster <- var.raster[[which(grepl(layer.yr, names(var.raster)))]]

    max <- max(maxValue(this.raster))
    min <- min(minValue(this.raster))
  }

  inc <- (max - min) / 10 
    
  breaks <- seq(from = min, to = max, inc)
    
  pal <- colorNumeric(colors, breaks, na.color = nacolor) 

}






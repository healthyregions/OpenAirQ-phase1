library(rgdal)
library(rgeos)
library(sp)
library(tmap)
library(ggplot2)


traffic.vol <- readOGR(".", "Community_Areas_with_Traffic_Volumes")
head(traffic.vol)

road.length <- readOGR(".", "Chi_Road_Lengths")
head(road.length)



# Distribution
ggplot(traffic.vol@data) +
  geom_density(aes(x = Trffc_V))

ggplot(traffic.vol@data) +
  geom_histogram(aes(x = Trffc_V), binwidth = 10000)



ggplot(road.length@data) +
  geom_density(aes(x = rd_lngt))

ggplot(road.length@data) +
  geom_histogram(aes(x = rd_lngt), binwidth = 1)




# Mapping Choropleth map for traffic volume. (I would recommend jenks with 5 bins)
tm_shape(traffic.vol) + 
  tm_borders(alpha = 1) + 
  tm_fill(
    col = "Trffc_V", 
    style = "jenks", 
    bolder.lwd = 0.1, 
    bolder.alpha = 0.4, 
    n = 5,  
    palette = "Reds", 
    title = "Traffic Volume") +
  tm_layout(
    main.title = "Traffic Volume by Community Area of Chicago",
    main.title.size = 1.2
  )


# Mapping Choropleth map for road lengths. (I would recommend jenks with 5 bins)
tm_shape(road.length) + 
  tm_borders(alpha = 1) + 
  tm_fill(
    col = "rd_lngt",
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






# Compared maps

tm_shape(traffic.vol) + 
  tm_borders(alpha = 1) + 
  tm_fill(col = "Trffc_V", style = "jenks", bolder.lwd = 0.1, bolder.alpha = 0.4, palette = "Reds", n = 6)

tm_shape(traffic.vol) + 
  tm_borders(alpha = 1) + 
  tm_fill(col = "Trffc_V", style = "jenks", bolder.lwd = 0.1, bolder.alpha = 0.4, palette = "Reds", n = 7)

tm_shape(traffic.vol) +
  tm_borders(alpha = 1) + 
  tm_fill(col = "Trffc_V", style = "sd", bolder.lwd = 0.1, bolder.alpha = 0.4, palette = "Reds")

tm_shape(road.length) + 
  tm_borders(alpha = 1) + 
  tm_fill(col = "Trffc_V", style = "quantile", bolder.lwd = 0.1, bolder.alpha = 0.4, palette = "Reds", n = 7)






tm_shape(road.length) + 
  tm_borders(alpha = 1) + 
  tm_fill(col = "rd_lngt", style = "jenks", bolder.lwd = 0.1, bolder.alpha = 0.4, palette = "Reds", n = 6)

tm_shape(road.length) + 
  tm_borders(alpha = 1) + 
  tm_fill(col = "rd_lngt", style = "jenks", bolder.lwd = 0.1, bolder.alpha = 0.4, palette = "Reds", n = 7)

tm_shape(road.length) +
  tm_borders(alpha = 1) + 
  tm_fill(col = "rd_lngt", style = "sd", bolder.lwd = 0.1, bolder.alpha = 0.4, palette = "Reds")

tm_shape(road.length) + 
  tm_borders(alpha = 1) + 
  tm_fill(col = "rd_lngt", style = "quantile", bolder.lwd = 0.1, bolder.alpha = 0.4, palette = "Reds", n = 7)




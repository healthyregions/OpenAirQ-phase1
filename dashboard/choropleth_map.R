library(rgdal)
library(rgeos)
library(sp)
library(tmap)
library(ggplot2)


Roademission <- readOGR(".", "Community_Areas_with_Traffic_Volumes")
head(Roademission)

# Distribution
ggplot(Roademission@data) +
  geom_density(aes(x = Trffc_V))

ggplot(Roademission@data) +
  geom_histogram(aes(x = Trffc_V), binwidth = 10000)


# Mapping Choropleth map. (I would recommend jenks with 7 bins)
tm_shape(Roademission) + 
  tm_borders(alpha = 1) + 
  tm_fill(col = "Trffc_V", style = "jenks", bolder.lwd = 0.1, bolder.alpha = 0.4, palette = "Reds", n = 7)

tm_shape(Roademission) + 
  tm_borders(alpha = 1) + 
  tm_fill(col = "Trffc_V", style = "jenks", bolder.lwd = 0.1, bolder.alpha = 0.4, palette = "Reds", n = 6)

tm_shape(Roademission) + 
  tm_borders(alpha = 1) + 
  tm_fill(col = "Trffc_V", style = "jenks", bolder.lwd = 0.1, bolder.alpha = 0.4, palette = "Reds", n = 5)

tm_shape(Roademission) + 
  tm_borders(alpha = 1) + 
  tm_fill(col = "Trffc_V", style = "sd", bolder.lwd = 0.1, bolder.alpha = 0.4, palette = "Reds")

tm_shape(Roademission) + 
  tm_borders(alpha = 1) + 
  tm_fill(col = "Trffc_V", style = "quantile", bolder.lwd = 0.1, bolder.alpha = 0.4, palette = "Reds", n = 7)


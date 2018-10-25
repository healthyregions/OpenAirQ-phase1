library(rgdal)
library(raster)
library(tmap)
library(rgeos)
library(sp)
library(leaflet)
setwd("C:/Users/arpan/Documents/NDVI")
#NDVI file
NDVI = raster('US_eMAH_NDVI.2017.073-079.QKM.VI_NDVI.005.2017090144955.tif')
plot(NDVI)


crs(NDVI)
str(NDVI)
str(NDVI@data)

CRS.new <- CRS("+init=EPSG:4326")
proj = projectRaster(NDVI, crs = CRS.new)



comarea = readOGR(dsn = ".", layer = "geo_export_c6dd6fe7-da82-4f79-b23b-ef61336a7389")
comarea = spTransform(comarea, CRS.new)
#str(comarea@bbox)
ext = extent(comarea@bbox[1, 1], comarea@bbox[1, 2], comarea@bbox[2, 1], comarea@bbox[2, 2])

crop_proj = crop(proj, ext)

ndvi_vec = rasterToPoints(crop_proj, spatial = TRUE)
by_area = intersect(ndvi_vec, comarea)


averages = rep(0, nrow(comarea@data))

for(i in 1:nrow(comarea@data)) {
  averages[i] = sum(by_area@data$US_eMAH_NDVI.2017.073.079.QKM.VI_NDVI.005.2017090144955 * (by_area@data$community == comarea@data$community[i])) / 
                sum(by_area@data$community == comarea@data$community[i])
}

comarea@data$ndvi_avg = averages

writeOGR(comarea, dsn = ".", layer = "ndvi_avg", driver = "ESRI Shapefile")


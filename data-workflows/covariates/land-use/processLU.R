library(rgdal)
library(rgeos)
library(raster)

setwd("C:/Users/arpan/Documents/LandUse")
all = readOGR(dsn = ".", layer = "all")

str(all)
crs(all)

CRS.new <- CRS("+init=EPSG:4326")
proj = spTransform(all, CRS.new)
crs(proj)

unique(all@data$LANDUSE)

resids = as.numeric(proj@data$LANDUSE) <= 5
coms = as.logical((as.numeric(proj@data$LANDUSE) >= 6) * (as.numeric(proj@data$LANDUSE) <= 13))
insts = as.logical((as.numeric(proj@data$LANDUSE) >= 14) * (as.numeric(proj@data$LANDUSE) <= 22))
inds = as.logical((as.numeric(proj@data$LANDUSE) >= 23) * (as.numeric(proj@data$LANDUSE) <= 28))
tcuws = as.logical((as.numeric(proj@data$LANDUSE) >= 29) * (as.numeric(proj@data$LANDUSE) <= 40) + (as.numeric(proj@data$LANDUSE) == 58))
ags = as.numeric(proj@data$LANDUSE) == 41
opens = as.logical((as.numeric(proj@data$LANDUSE) >= 42) * (as.numeric(proj@data$LANDUSE) <= 46) + (as.numeric(proj@data$LANDUSE) == 56))
vacs = as.logical((as.numeric(proj@data$LANDUSE) >= 47) * (as.numeric(proj@data$LANDUSE) <= 54))
waters = as.logical((as.numeric(proj@data$LANDUSE) == 55) + (as.numeric(proj@data$LANDUSE) == 57))
invs = as.numeric(proj@data$LANDUSE) >= 59

residential = subset(proj, resids)
commercial = subset(proj, coms)
institutional = subset(proj, insts)
industrial = subset(proj, inds)
tcuw = subset(proj, tcuws)
agriculture = subset(proj, ags)
open_space = subset(proj, opens)
vacant_construction = subset(proj, vacs)
water = subset(proj, waters)
invalid = subset(proj, invs)

length(unique(residential@data$LANDUSE)) + length(unique(commercial@data$LANDUSE)) + length(unique(institutional@data$LANDUSE)) + 
  length(unique(industrial@data$LANDUSE)) + length(unique(tcuw@data$LANDUSE)) + length(unique(agriculture@data$LANDUSE)) + 
  length(unique(open_space@data$LANDUSE)) + length(unique(vacant_construction@data$LANDUSE)) + length(unique(water@data$LANDUSE)) + 
  length(unique(invalid@data$LANDUSE))

writeOGR(proj, dsn = getwd(), layer = "proj", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(residential, dsn = getwd(), layer = "residential", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(commercial, dsn = getwd(), layer = "commercial", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(institutional, dsn = getwd(), layer = "institutional", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(industrial, dsn = getwd(), layer = "industrial", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(tcuw, dsn = getwd(), layer = "tcuw", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(agriculture, dsn = getwd(), layer = "agriculture", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(open_space, dsn = getwd(), layer = "openspace", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(vacant_construction, dsn = getwd(), layer = "vacantconstruction", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(water, dsn = getwd(), layer = "water", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(invalid, dsn = getwd(), layer = "invalid", driver = "ESRI Shapefile", overwrite_layer = TRUE)









                     
                     
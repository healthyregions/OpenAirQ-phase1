library(rgdal)
library(raster)
library(tmap)
library(rgeos)
library(sp)
library(leaflet)

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

writeOGR(proj, dsn = ".", layer = "proj", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(residential, dsn = ".", layer = "residential", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(commercial, dsn = ".", layer = "commercial", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(institutional, dsn = ".", layer = "institutional", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(industrial, dsn = ".", layer = "industrial", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(tcuw, dsn = ".", layer = "tcuw", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(agriculture, dsn = ".", layer = "agriculture", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(open_space, dsn = ".", layer = "openspace", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(vacant_construction, dsn = ".", layer = "vacantconstruction", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(water, dsn = ".", layer = "water", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(invalid, dsn = ".", layer = "invalid", driver = "ESRI Shapefile", overwrite_layer = TRUE)


#community area shapefile
comarea = readOGR(dsn = ".", layer = "geo_export_c6dd6fe7-da82-4f79-b23b-ef61336a7389")
new_crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#new_crs = CRS("+init=EPSG:4326")
comarea = spTransform(comarea, new_crs)
keep = c("LANDUSE", "ShapeLen", "Shape_Area", "community", "area_numbe")

residential = readOGR(dsn = ".", layer = "residential")
residential = intersect(residential, comarea)
residential@data = residential@data[keep]
writeOGR(residential, dsn = ".", layer = "residential_chi", driver = "ESRI Shapefile", overwrite_layer = TRUE)

commercial = readOGR(dsn = ".", layer = "commercial")
commercial = intersect(commercial, comarea)
commercial@data = commercial@data[keep]
writeOGR(commercial, dsn = ".", layer = "commercial_chi", driver = "ESRI Shapefile", overwrite_layer = TRUE)

institutional = readOGR(dsn = ".", layer = "institutional")
institutional = intersect(institutional, comarea)
institutional@data = institutional@data[keep]
writeOGR(institutional, dsn = ".", layer = "institutional_chi", driver = "ESRI Shapefile", overwrite_layer = TRUE)

industrial = readOGR(dsn = ".", layer = "industrial")
industrial = intersect(industrial, comarea)
industrial@data = industrial@data[keep]
writeOGR(industrial, dsn = ".", layer = "industrial_chi", driver = "ESRI Shapefile", overwrite_layer = TRUE)

tcuw = readOGR(dsn = ".", layer = "tcuw")
tcuw = intersect(tcuw, comarea)
tcuw@data = tcuw@data[keep]
writeOGR(tcuw, dsn = ".", layer = "tcuw_chi", driver = "ESRI Shapefile", overwrite_layer = TRUE)

agriculture = readOGR(dsn = ".", layer = "agriculture")
agriculture = intersect(agriculture, comarea)
agriculture@data = agriculture@data[keep]
writeOGR(agriculture, dsn = ".", layer = "agriculture_chi", driver = "ESRI Shapefile", overwrite_layer = TRUE)

open_space = readOGR(dsn = ".", layer = "openspace")
open_space = intersect(open_space, comarea)
open_space@data = open_space@data[keep]
writeOGR(open_space, dsn = ".", layer = "open_space_chi", driver = "ESRI Shapefile", overwrite_layer = TRUE)

vacant_construction = readOGR(dsn = ".", layer = "vacantconstruction")
vacant_construction = intersect(vacant_construction, comarea)
vacant_construction@data = vacant_construction@data[keep]
writeOGR(vacant_construction, dsn = ".", layer = "vacant_construction_chi", driver = "ESRI Shapefile", overwrite_layer = TRUE)

water = readOGR(dsn = ".", layer = "water")
water = intersect(water, comarea)
water@data = water@data[keep]
writeOGR(water, dsn = ".", layer = "water_chi", driver = "ESRI Shapefile", overwrite_layer = TRUE)

invalid = readOGR(dsn = ".", layer = "invalid")
invalid = intersect(invalid, comarea)
invalid@data = invalid@data[keep]
writeOGR(invalid, dsn = ".", layer = "invalid_chi", driver = "ESRI Shapefile", overwrite_layer = TRUE)

coverage = data.frame(residential = rep(0, nrow(comarea@data)), commercial = rep(0, nrow(comarea@data)), institutional = rep(0, nrow(comarea@data)),
                         industrial = rep(0, nrow(comarea@data)), tcuw = rep(0, nrow(comarea@data)), agriculture = rep(0, nrow(comarea@data)),
                         open_space = rep(0, nrow(comarea@data)), vacant_construction = rep(0, nrow(comarea@data)), water = rep(0, nrow(comarea@data)),
                         invalid = rep(0, nrow(comarea@data)), total = rep(0, nrow(comarea@data)))
rownames(coverage) = comarea@data$community

total_area = comarea@data$shape_area

residential_chi = readOGR(dsn = ".", layer = "residential_chi")
commercial_chi = readOGR(dsn = ".", layer = "commercial_chi")
institutional_chi = readOGR(dsn = ".", layer = "institutional_chi")
industrial_chi = readOGR(dsn = ".", layer = "industrial_chi")
tcuw_chi = readOGR(dsn = ".", layer = "tcuw_chi")
agriculture_chi = readOGR(dsn = ".", layer = "agriculture_chi")
open_space_chi = readOGR(dsn = ".", layer = "open_space_chi")
vacant_construction_chi = readOGR(dsn = ".", layer = "vacant_construction_chi")
water_chi = readOGR(dsn = ".", layer = "water_chi")
invalid_chi = readOGR(dsn = ".", layer = "invalid_chi")

for (i in 1:nrow(coverage)) {
  com_name = as.character(comarea@data$community[i])
  coverage[com_name, "residential"] = sum(residential_chi@data$Shape_Area * (residential_chi@data$community == com_name)) / total_area[i]
  coverage[com_name, "commercial"] = sum(commercial_chi@data$Shape_Area * (commercial_chi@data$community == com_name)) / total_area[i]
  coverage[com_name, "institutional"] = sum(institutional_chi@data$Shape_Area * (institutional_chi@data$community == com_name)) / total_area[i]
  coverage[com_name, "industrial"] = sum(industrial_chi@data$Shape_Area * (industrial_chi@data$community == com_name)) / total_area[i]
  coverage[com_name, "tcuw"] = sum(tcuw_chi@data$Shape_Area * (tcuw_chi@data$community == com_name)) / total_area[i]
  coverage[com_name, "agriculture"] = sum(agriculture_chi@data$Shape_Area * (agriculture_chi@data$community == com_name)) / total_area[i]
  coverage[com_name, "open_space"] = sum(open_space_chi@data$Shape_Area * (open_space_chi@data$community == com_name)) / total_area[i]
  coverage[com_name, "vacant_construction"] = sum(vacant_construction_chi@data$Shape_Area * (vacant_construction_chi@data$community == com_name)) / total_area[i]
  coverage[com_name, "water"] = sum(water_chi@data$Shape_Area * (water_chi@data$community == com_name)) / total_area[i]
  coverage[com_name, "invalid"] = sum(invalid_chi@data$Shape_Area * (invalid_chi@data$community == com_name)) / total_area[i]
}

comarea@data$residential = coverage[, "residential"]
comarea@data$commercial = coverage[, "commercial"]
comarea@data$institutional = coverage[, "institutional"]
comarea@data$industrial = coverage[, "industrial"]
comarea@data$tcuw = coverage[, "tcuw"]
comarea@data$agriculture = coverage[, "agriculture"]
comarea@data$open_space = coverage[, "open_space"]
comarea@data$vacant_construction = coverage[, "vacant_construction"]
comarea@data$water = coverage[, "water"]
comarea@data$invalid = coverage[, "invalid"]

writeOGR(comarea, dsn = ".", layer = "comarea_ratios", driver = "ESRI Shapefile")










                     
                     
#process epa data
#by: Eric Jionghua WANG 
#Create interpolation surface with kriging
library(gstat) #kriging
library(stringr) # extract date from the epa date
library(sf)
library(sp)
library(rgdal)
library(data.table)
library(raster)


Output <- T #Save the result as file
EPAWorkPath <- c( "./data-workflows/sensors/epa-sensors/","data/","")
EPAFileName <- c("PM2.5MonthlyShapefile.shp","PM2.5YearlyShapefile1.shp")

#ReadEpaData ----
#read the epa data from the project package sourced from 2018113
ReadEpaData <- function(filename)
{
  for(i in 1:length(EPAWorkPath)){
    if(file.exists(paste0(EPAWorkPath[i],filename))){
      print(paste0(EPAWorkPath[i],filename))
      EPAdata <- readOGR(paste0(EPAWorkPath[i],filename))
      return(EPAdata)
    }
  }
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
Interp <- function(inputdatatable,timepoint,resl,nametype)
{
  datefield <- GetTimeStamp(year(timepoint),month(timepoint),nametype)
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
  if(type == "month")
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
  else
  {
    return(paste0("X",year,"_Daily"))
  }
}

# EPADATA<-ReadEpaData()
# plot(Interp(EPADATA,as.Date("2017-01-01"),100))

CreateINPresult<-function(){
  
  EPADATA<<-ReadEpaData(EPAFileName[2])
  nl<-names(EPADATA)
  i <- 1
  
  rawdate<-strsplit(str_extract(nl[i+11], "[0-9]+_[0-9]+"),'_')
  thisdate<-as.Date(paste0(unlist(rawdate)[1],"-",unlist(rawdate)[2],"-01"))
  InterpResultList <- data.table(date = c(thisdate), field = c(nl[i+11]),itpr = c(Interp(EPADATA,thisdate,100,"month")))
  
  for(i in 2:(length(nl)-(11+5)))
  {
    rawdate<-strsplit(str_extract(nl[i+11], "[0-9]+_[0-9]+"),'_')
    thisdate<-as.Date(paste0(unlist(rawdate)[1],"-",unlist(rawdate)[2],"-01"))
    print(i)
    InterpResultList<-rbind(InterpResultList,data.table(date = c(thisdate), field = paste0(year(thisdate),"-",month(thisdate),"-01_0EPAPMTPF"),itpr = c(Interp(EPADATA,thisdate,100,"month"))))
  }
  #the years
  for(i in 1:3)
  {
    thisdate<- as.Date(paste0(2014+i,"-01-01"))
    InterpResultList<-rbind(InterpResultList,data.table(date = c(thisdate), field =  paste0(year(thisdate),"-",month(thisdate),"-01_1EPAPMTPF"),itpr = c(Interp(EPADATA,thisdate,100,"year"))))
  }
  
  if(Output){
    i<-1
    for(i in 1:39){
      writeGDAL(InterpResultList$itpr[[i]], paste0(EPAWorkPath[3],InterpResultList$field[i],".tif"), drivername="GTiff", type="Float32") 
    }
  }
  return(InterpResultList)
  
}

AD<-CreateINPresult()

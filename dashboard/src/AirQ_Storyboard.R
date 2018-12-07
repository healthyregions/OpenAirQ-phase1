#read and create storyboard table from sheet
#by: Eric 2018.11.18
# -this function create a storyboard's icon color subtitle fieldname and value
# given a fieldname proposed by the click on one community area
library('stringi')
library(fields) #rdist for interpolation
library(rdist)
library(ramify)
library(animation)
library(akima)
library(rgl)
#test text in the storyboard
testtext <<- "When I wrote the following pages, or rather the bulk of them, I lived alone, in the woods, a mile from any neighbor, in a house which I had built myself, on the shore of Walden Pond, in Concord, Massachusetts, and earned my living by the labor of my hands only. I lived there two years and two months. At present I am a sojourner in civilized life again.
I should not obtrude my affairs so much on the notice of my readers if very particular inquiries had not been made by my townsmen concerning my mode of life, which some would call impertinent, though they do not appear to me at all impertinent, but, considering the circumstances, very natural and pertinent. Some have asked what I got to eat; if I did not feel lonesome; if I was not afraid; and the like." 


GenrateStoryBoards <-function(Beststory,CommunityName,totalregion,MSB){
  
  #Beststory is the story generated based on the numbers
  NumberofFields <- NROW(Beststory)
  
  #test the existence of the meta data sheet
  
  #set the column name of the result table
  colname <- c("StoryboardName","Precentage",
               "Value","Rank","Icon","Color","Color2","Subtitle","unit")
  result <- data.frame(matrix(ncol = length(colname), nrow = 0))
  FieldIndex <- 1
  colnames(result) <- colname
  Icon <- icon("cog", lib = "glyphicon")
  Color <- "#FF4500" #orange
  Color2 <- "#00BFFF" #blue
  Subtitle <- ""
  #generate the result one by one field
  for(i in 1:NumberofFields){
    #FindMyStory
    FieldIndex<-which(Beststory$FieldName[i]== MSB$FieldName)
    
    if(length(FieldIndex)!=0){
      FieldIndex <- FieldIndex[1] #In case we have more than one result
      
    Thisprect <- ifelse(MSB$isPositive[FieldIndex]=="T",Beststory$Precentage[i],1-Beststory$Precentage[i]) 
    Thiscolor <- ifelse(Thisprect>0.66,"lime",ifelse(Thisprect>0.33,"olive","orange"))
    result<-rbind(result,data.table(
        StoryboardName = MSB$`Name of attributes`[FieldIndex], 
        Precentage = Beststory$Precentage[i], 
        Value = format(Beststory$Value[i],digits = 2,nsmall = 2), 
        Rank = Beststory$Rank[i], 
        Icon = MSB$Icon[FieldIndex], 
        Color = Thiscolor, Color2 = MSB$Color2[FieldIndex], 
        Subtitle = MSB$`Name of attributes`[FieldIndex],
        unit = MSB$Unit[FieldIndex]))
    }
  }
  return(result)
  ## = name = value = subtitle =icon = color
  # CreateDescription(StoryItem = Beststory,i = i,totalregion = totalregion,fieldname =MSB$`Name of attributes`[FieldIndex])
  
}

CreateDescription <- function(StoryItem,i,totalregion,fieldname){
  #create a desciption given one story
  #The field name is very high/low, which is the ** in chicago
  return(paste0("The ",fieldname," is very ",
                ifelse(StoryItem$Precentage[i]>0.5,"high","low"),
                ", which is ", format(StoryItem$Value[i],digits = 2,nsmall = 2)," (",as.integer(totalregion - StoryItem$Rank[i]+1),"th) in Chicago"))}


FindtheStory <-function(regionid,BestStory_n,infTable,ncol,nrow,rankmatrix,CPTC,CPTC.rankidtable){
  nl<-names(infTable)
  rankidtable <- matrix(nrow = 4,ncol = ncol,data = 1:4*ncol)
  rankidtable[1,]<-c(1:ncol)
  rankidtable[2,]<-as.matrix(rankmatrix[regionid,]) # rank
  rankidtable[3,]<-abs(as.matrix(rankmatrix[regionid,])-nrow/2) #important level
  rankidtable[4,]<-(as.matrix(rankmatrix[regionid,]/nrow)) #precent
  rankidtable[3,1:2] <- 0 # igorn the id and name
  #rank the most important issue in this region
  
  va<-(rankidtable[,order(-rankidtable[3,])])
  if(!CPTC||CPTC.rankidtable==""){
    CPTC.rankidtable <- rankidtable #if does compare
  }else{
    va<-(rankidtable[,order(-CPTC.rankidtable[3,])])
  }
  colname <- c("FieldName","Precentage","Value","Rank")
  result <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(result) <- colname

  for(si in 1: BestStory_n){
    thisFiledID <- va[1,si]
    result<-rbind(result,data.table(FieldName = nl[thisFiledID],Precentage = va[4,si], Rank = va[2,si],Value=infTable[regionid,thisFiledID] ))
  }

  return(list(result =result, CPTC.rankidtable = CPTC.rankidtable))
}

AirQGetIcon <- function(Icontext){
  if(stri_detect_fixed(Icontext,"glyphicon-")){ 
    Icon <- icon(substr(Icontext,11,str_length(Icontext)),lib = "glyphicon")}
  else{ 
    Icon <- icon(Icontext)}
  return(Icon)
}

  AirQ_PAL <-function(domain,palette = "Blues", bin = NULL ){
  return(colorQuantile(palette, domain = domain))
}


# Handle Air Pollution Data -----------------------------------------------
  
# numCores <- detectCores()
# cl <- makeCluster(numCores)
if(F){
datadir = "dashboard/data/epa"
setwd(datadir)
epasource<-list.files(getwd(),pattern = "*.csv")
datefield <- "Date"
siteidfield <- "Site ID"
siteidinresulttable <-"SiteID"
valuefield <- "Daily Mean PM2.5 Concentration"
i<- 1 

# s_epr<-as.data.frame(scale(as.data.frame(epr),))
# plot_ly(s_epr, x = s_epr$lat, y = s_epr$lon, z = s_epr$`01/05/2018`, type = "surface")
epr<-CreateAirPollutantPointDataSet(epasource[2],valuefield = "Daily Mean PM10 Concentration")
epr<-CreateAirPollutantPointDataSet(epasource[3])}

#create monthly or year avg

#create dialy raw data
CreateAirPollutantPointDataSet <-function(path, datefield = "Date",siteidfield = "Site ID",siteidinresulttable = "SiteID", valuefield = "",coordinatesfield = c("SITE_LATITUDE","SITE_LONGITUDE")){
epadataframe <-fread(path)
valuefield <- names(epadataframe)[5]
# epadataframe$date <- as.Date(epadataframe$Date,format = "%d/%m/%Y")
epa_obst<-unique(epadataframe$date)

colname <- c(siteidinresulttable,unique(epadataframe[[datefield]])) #get uniquedata
rowname <- unique(epadataframe[[siteidfield]])
rowname <- rowname[order(rowname)]

result <- data.frame(matrix(ncol = length(colname), nrow = length(rowname)))
colnames(result) <- colname
result[[1]] <- rowname
for(i in 2:length(colname)){
  onetimeframe <- epadataframe[Date == colname[i],
                               .(mean(get(valuefield))),
                               by = .(get(siteidfield))]
  diffrow <- rowname[!rowname %in% onetimeframe$get]
  diffrow <- cbind(diffrow, rep(NA,length(diffrow)))
  onetimeframe <- rbind(as.matrix(onetimeframe),diffrow)
  onetimeframe <- onetimeframe[order(onetimeframe[,1]),]
  result[[colname[i]]]<-onetimeframe[,2]
}
# coordinatesfield<-c(siteidfield,coordinatesfield)
coord<-epadataframe[,.(lat = mean(get(coordinatesfield[1])),lon = mean(get(coordinatesfield[2])))
                    ,by = get(siteidfield)]
result <- cbind(result,coord[,2:3])
# result <- cbind(result,epadataframe[,..coordinatesfield])
# result<-merge(result,epadataframe,by.x = siteidinresulttable, by.y = siteidfield,)
return(result)
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
#read a year and month avg from one epa point dataset
#Create a object say both raw and processed data
EPAMonthlyYearlyAVG <-function(EpaRawData){
  epadataframe <-fread(EpaRawData)
  valuefield <- names(epadataframe)[5]
  epadataframe$date <- as.Date(epadataframe$Date,format = "%m/%d/%Y")
  epadataframe$year <- year(epadataframe$date)
  epadataframe$month <- month(epadataframe$date)
  epaloc <- epadataframe[,.(.N),by = .(`Site ID`,SITE_LATITUDE,SITE_LONGITUDE)]
  epasub <- epadataframe[,.(`Site ID`,get(valuefield),year,month)]
  averagebymonth <- epasub[,lapply(.SD,mean,na.rm = T), by = .(`Site ID`,month,year)]
  averagebyyear <- averagebymonth[,lapply(.SD,mean,na.rm = T), by = .(`Site ID`,year)]
  
  return(list(EPARAW = epadataframe, 
              epamonth = averagebymonth, 
              epayear = averagebyyear, 
              datatype = valuefield,
              loc = epaloc))

}
#Create Interpolatio Image
CreateInterpolationImage<-function(EPAMYR,YEAR,MONTH,GRID = "",RESL = 100, Datafield = "V2",IDW = T,byMONTH = T)
{

#given a time 
#year
dfyear <- YEAR
dfmonth <- MONTH
datafield <- Datafield
resl <- RESL # resolution
if(byMONTH){
  inputdatatable<-merge(EPAMYR$epamonth,EPAMYR$loc, by.x = "Site ID", by.y = "Site ID")
  subtable <- inputdatatable[month == dfmonth & year == dfyear,]}
else
{
  inputdatatable<-merge(EPAMYR$epayear,EPAMYR$loc, by.x = "Site ID", by.y = "Site ID")
  subtable <- inputdatatable[year == dfyear,]
}

###
# epadataframe$day <- day(epadataframe$date)
# subtable <- epadataframe[month == dfmonth & year == dfyear & day ==2,]
# datafield <- "Daily Max 8-hour Ozone Concentration"
##
df <- as.data.frame(subtable)
df <- as.data.frame(subset(df,(!is.na(df[[datafield]]))))
coordinates(df) <- df[,c("SITE_LONGITUDE", "SITE_LATITUDE")]
proj4string(df) <- CRS("+init=epsg:4326")

griddf<-SpatialPoints(cbind(c(-87.439931,-87.969428),c(41.589314,42.050333)),proj4string = CRS("+init=epsg:4326"))
# proj4string(griddf) <- CRS("+init=epsg:4326")
# griddf <- SpatialPointsDataFrame(proj4string = CRS("+init=epsg:4326"),bbox = bbox(SpatialPoints(cbind(c(-87.439931,-87.969428),c(41.589314,42.050333)))))
# plot(kr.vgm, kr.fit)
# create a grid for interpolation
if(GRID ==""){
  itpgrid <- pt2grid(griddf,resl)
  print("ss")
  projection(itpgrid) <- CRS("+init=epsg:4326") 
}else{
  itpgrid <- pt2grid(df,resl)
  projection(itpgrid) <- CRS("+init=epsg:4326") 
  # itpgrid <-GRID
}


if(IDW){
  return(idw(df[[datafield]] ~ 1,df,itpgrid))
}else
{
  kr.vgm <- variogram(df[[datafield]] ~ 1, df)
  kr.fit <- fit.variogram(kr.vgm, model=vgm("Mat"),fit.kappa = T) # fit model
  # optimize the value of kappa in a Matern model, using ugly <<- side effect:
  f = function(x) attr(m.fit <<- fit.variogram(kr.vgm, vgm("Mat",nugget=NA,kappa=x)),"SSErr")
  optimize(f, c(0.1, 1000))
  return(krige(df[[datafield]] ~ 1,df,itpgrid, model= m.fit ))
}
#plot(Itpr)
}
CreateInterpolationImageFile <- function(folderpath,SpatialBoundary = NULL){
  # run this if local test
  # datadir = "dashboard/data/epa"
  # setwd(datadir)
  if(!is.null(SpatialBoundary)){
    returnlist <- list(ID = 1:nrow(SpatialBoundary))
  }
  epasource<-list.files(folderpath,pattern = "*.csv")
  for(i in 1 : length(epasource)){
    EPAMYR <- EPAMonthlyYearlyAVG(paste0(folderpath,epasource[i]))
    Monthlist<-EPAMYR$epamonth[,.(.N),by = .(month, year)]
    for(j in 1:nrow(Monthlist)){
      YEAR <- Monthlist$year[j]
      MONTH <- Monthlist$month[j]
      # print(c(EPAMYR$datatype,YEAR,MONTH,i,j))
      thisimage <- CreateInterpolationImage(EPAMYR,YEAR,MONTH)
      Filename <- paste0("epa",EPAMYR$datatype,"_",YEAR,"_",MONTH)
      SavePath <- paste0(folderpath,Filename,".tif")
      writeGDAL(thisimage,SavePath, drivername="GTiff",type="Float32")
      if(!is.null(SpatialBoundary)){
        ext<-raster::extract(raster(SavePath),SpatialBoundary,fun = mean)
        returnlist[[Filename]]<-ext
      }
    }
    Yearlist <-EPAMYR$epamonth[,.(.N),by = .(year)]
    for(j in 1:nrow(Yearlist)){
      YEAR <- Yearlist$year[j]
      MONTH <- 0
      # print(c(EPAMYR$datatype,YEAR,MONTH,i,j))
      thisimage <- CreateInterpolationImage(EPAMYR,YEAR,MONTH,byMONTH = F)
      Filename <- paste0("epa",EPAMYR$datatype,"_",YEAR,"_",MONTH)
      SavePath <- paste0(folderpath,Filename,".tif")
      writeGDAL(thisimage,SavePath, drivername="GTiff",type="Float32")
      if(!is.null(SpatialBoundary)){
        ext<-raster::extract(raster(SavePath),SpatialBoundary,fun = mean)
        returnlist[[Filename]]<-ext
      }
    }
    
  }
  return(returnlist)
  # run this for testing result
  # plot(raster("epaDaily Mean PM2.5 Concentration_2018_9.tif")) 
}

# SpatialBoundary <- readOGR("Chicago.shp")
# projection(SpatialBoundary) <- CRS("+init=epsg:4326") 
# CHICAGOEPA<-CreateInterpolationImageFile("EPA/",SpatialBoundary)


# saveformat <- 'paste0(\"epa\",EPAMYR$datatype,\"_\",YEAR,\"_\",MONTH,\".tif\")'










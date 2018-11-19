#read and create storyboard table from sheet
#by: Eric 2018.11.18
# -this function create a storyboard's icon color subtitle fieldname and value
# given a fieldname proposed by the click on one community area
library('stringi')
GenrateStoryBoards <-function(Beststory,CommunityName,totalregion,MSB){
  
  #Beststory is the story generated based on the numbers
  NumberofFields <- NROW(Beststory)
  
  #test the existence of the meta data sheet
  
  #set the column name of the result table
  colname <- c("StoryboardName","Precentage","Value","Rank","Icon","Color","Color2","Subtitle")
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

      
      result<-rbind(result,data.table(
        StoryboardName = MSB$`Name of attributes`[FieldIndex], 
        Precentage = Beststory$Precentage[i], Value = Beststory$Value[i], Rank = Beststory$Rank[i], 
        Icon = MSB$Icon[FieldIndex], Color = MSB$Color[FieldIndex], Color2 = MSB$Color2[FieldIndex], 
        Subtitle = CreateDescription(StoryItem = Beststory,i = i,totalregion = totalregion)))
    }
  }
  return(result)
  ## = name = value = subtitle =icon = color
  
  
}

CreateDescription <- function(StoryItem,i,totalregion){
  #create a desciption given one story
  #The field name is very high/low, which is the ** in chicago
  return(paste0("The ",StoryItem$FieldName[i]," is very ",
                ifelse(StoryItem$Precentage[i]>0.5,"high","low"),
                ", which is ",StoryItem$Value[i]," (",as.integer(totalregion - StoryItem$Rank[i]),"th) in Chicago"))}


FindtheStory <-function(regionid,BestStory_n,infTable,ncol,nrow,rankmatrix){
  nl<-names(infTable)
  rankidtable <- matrix(nrow = 4,ncol = ncol,data = 1:4*ncol)
  rankidtable[1,]<-c(1:ncol)
  rankidtable[2,]<-as.matrix(rankmatrix[regionid,]) # rank
  rankidtable[3,]<-abs(as.matrix(rankmatrix[regionid,])-nrow/2) #important level
  rankidtable[4,]<-(as.matrix(rankmatrix[regionid,]/nrow)) #precent
  rankidtable[3,1:2] <- 0 # igorn the id and name
  #rank the most important issue in this region
  va<-(rankidtable[,order(-rankidtable[3,])])
  colname <- c("FieldName","Precentage","Value","Rank")
  result <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(result) <- colname

  for(si in 1: BestStory_n){
    thisFiledID <- va[1,si]
    result<-rbind(result,data.table(FieldName = nl[thisFiledID],Precentage = va[4,si], Rank = va[2,si],Value=infTable[regionid,thisFiledID] ))
  }

  return(result)
}

AirQGetIcon <- function(Icontext){
  if(stri_detect_fixed(Icontext,"glyphicon-")){ 
    Icon <- icon(substr(Icontext,11,str_length(Icontext)),lib = "glyphicon")}
  else{ 
    Icon <- icon(Icontext)}
  return(Icon)
}
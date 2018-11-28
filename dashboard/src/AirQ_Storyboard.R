#read and create storyboard table from sheet
#by: Eric 2018.11.18
# -this function create a storyboard's icon color subtitle fieldname and value
# given a fieldname proposed by the click on one community area
library('stringi')

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
    print("aaa")
    print(rankidtable)
    print("ccc")
    print(CPTC.rankidtable)
  }else{
    va<-(rankidtable[,order(-CPTC.rankidtable[3,])])
    print("bbb")
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


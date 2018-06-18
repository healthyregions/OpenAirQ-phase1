#Script to extract the Array of Things data (public dataset) from a specific day
#The script uses the preliminary AoT API and data are not up-to-date for now (latest data on May,9th). Once the API will be fully operational, a function will extract only the previous day's data.

#LIBRARIES

library(tidyverse) # (ggplot2, dplyr, readr, etc)
library(sf) # simple features (spatial)
library(jsonlite)
library(reshape2)

#Set working directory to project directory
setwd("~/Geoda_airquality/workflows/sensors")

#GET THE PUBLIC DATASET

#Request informations about the latest data
latest <- fromJSON('https://dev.plenar.io/api/v2/aot/@head')
#Request metadata about the API
metadat <- fromJSON('https://dev.plenar.io/api/v2/aot/@describe')

meta <- c("updated_at","timestamp","node_id","longitude","latitude","inserted_at","human_address","aot_meta_id")
page_size <- '50000'
url <- paste0('https://dev.plenar.io/api/v2/aot?page_size=',page_size)
nbPages <- fromJSON(url)$meta$counts$total_pages
data <- data.frame()
for(i in 0:nbPages){
  r <- fromJSON(paste0(url, "&page=", i))$data
  message("Retrieving page ", i)
  #Reshape the dataframe in a tidy format (split the sensor.parameter column in two columns)
  r <- flatten(r$observations) %>% cbind(r) %>% select(-c(observations,location,id))
  data <- rbind(data,r)
}

obs <- melt(data=data,id.vars=c('timestamp','node_id'),measure.vars=colnames(data)[which(!colnames(data) %in% meta)]) 
obs$variable <- as.character(obs$variable)
obs <- separate(data = obs, col = variable, into = c('sensor','parameter'), sep = "\\.")

#Request on timestamp don't seem to work yet, idea of script to extract data from the last day when API fully operational
# bday <- '2018-05-09' #Day for which you want to extract the data (yyyy-mm-dd)
# eday <- '2018-05-10'
# url <- paste0('https://dev.plenar.io/api/v2/aot?timestamp=in{"lower":"',bday,'T00:00:00.00000","upper":"',eday,'T00:00:00.00000"}&page_size=5000')

#Nodes location : Keep node_id, latitude, longitude and human_address fields in data.csv
nodes <- data %>% select(node_id, latitude, longitude, human_address) %>% distinct()

write_csv(data,paste0('Data/AoT/Public/data_',day,'.csv')) #Save data
write_csv(nodes,paste0('Data/AoT/Public/nodes.csv')) #Save nodes location metadata

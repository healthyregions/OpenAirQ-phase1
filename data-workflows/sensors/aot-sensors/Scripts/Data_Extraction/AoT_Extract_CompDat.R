#Script to extract the Array of Things data (complete dataset) from the previous day
# Start by downloading the data with AoT_Extract_CompDat.sh

#LIBRARIES

library(tidyverse) # (ggplot2, dplyr, readr, etc)
library(sf) # simple features (spatial)

#Set working directory to project directory
setwd("~/Geoda_airquality/workflows/sensors")

#GET THE COMPLETE DATASET FOR THE PREVIOUS DAY 
# Run bash script AoT_DataExtraction.sh to download / untar and extract data from yesterday
# Read metadata files and day.csv (subset of data.csv with only data from yesterday)

dir <- paste0('Data/AoT/AoT_Chicago.complete.',Sys.Date(),'/') #Replace Sys.Date() by a valid date time if the data are not download today

start.time <- Sys.time()
#Metadata files : nodes.csv, sensors.csv (provenance.csv is the description of the project but since only one exist in Chicago for now, this file doesn't bring useful information)
nodes <- read_csv(paste0(dir,'nodes.csv'))
sensors <- read_csv(paste0(dir,'sensors.csv'))

#Data file : just read data from yesterday (day.csv) as data.csv is too big to be read in R memory (entire dataset since the launch of the project).
#Need to add column names as the header is not kept with the cut command
data <- read_csv(paste0(dir,'day.csv'),col_names = c('node_id','timestamp','plugin','sensor','parameter','value'))
end.time <- Sys.time()
print(end.time-start.time)

str(nodes) #Structure of nodes.csv
str(sensors) #Structure of sensors.csv

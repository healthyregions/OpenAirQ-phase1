#Script to extract the EPA's data for a given time period.
# Only the data for the criteria pollutants are downloaded for now but possible to extend with other dataset with parameter class variable (pc). Check the different options in the parameter_class.csv file.

#LIBRARIES

library(tidyverse) # (ggplot2, dplyr, readr, etc)
library(sf) # Simple Features
library(USAboundaries) # Boundaries for geographical units in the United States of America (U.S. Census Bureau)

#Set working directory to project directory
setwd("~/Geoda_airquality/workflows/sensors")

#Stard date and end date for which we want to download EPA's data (yyyymmdd)
bdate <- "20180301"
edate <- "20180401"

#Function to extract data for a specific time period and for a specific county.
aqs_dat <- function(bdate,edate,state,county) {
  return (read_csv(paste0("https://aqs.epa.gov/api/rawData?user=anais.ladoy@epfl.ch&pw=saffrongoose78&format=AQCSV&pc=CRITERIA&param=&bdate=",bdate,"&edate=",edate,"&state=",state,"&county=",county,"&dur=1")))
}

#Download EPA's data for the Chicago's surrounding counties 
data <- data.frame()

#IL counties
counties.IL <- c('031','097','043','197','091','111','089','093','063')
for (i in 1:length(counties.IL)) {
  r.IL <- aqs_dat(bdate,edate,"17",counties.IL[i])
  data <- rbind(data,r.IL) 
}

#IN counties ('LAKE','PORTER','NEWTON','JASPER','LA PORTE','STARKE','PULASKI')
counties.IN <- c('089','127','111','073','091','149','131')
for (i in 1:length(counties.IN)) {
  r.IN <- aqs_dat(bdate,edate,"92",counties.IN[i])
  data <- rbind(data,r.IN) 
}

#WI counties ('RACINE','WALWORTH','KENOSHA','MILWAUKEE','WAUKESHA')
counties.WI <- c('101','127','059','079','133')
for (i in 1:length(counties.WI)) {
  r.WI <- aqs_dat(bdate,edate,"55",counties.WI[i])
  data <- rbind(data,r.WI) 
}

data.bk <- data
data <- data.bk

#Remove empty lines (NA lines), must be equal to 21 (nb of requests=nb of counties)
if(nrow(data %>% filter(is.na(site) | site=='END OF FILE')) == 21){
  data <- data %>% filter(!is.na(site) & site!='END OF FILE')
} else{
  print('There are more NA lines than expected. You may check again.')
}

#Save data
write_csv(data,paste0('Data/AQS/data_',bdate,'_',edate,'.csv'))

#Read metadata files (for EPA sites and monitors)
meta_path <- 'Data/AQS/metadata/'
sites <- read_csv(paste0(meta_path,'aqs_sites.csv'))
monitors <- read_csv(paste0(meta_path,'aqs_monitors.csv'))

#Map the Chicago's surrounding counties we consider in the project
limits.counties <- us_counties() %>% filter((state_name=='Illinois' & name %in% c('Cook','Lake','DuPage','Will','Kankakee','McHenry','Kane','Kendall','Grundy')) | (state_name=="Wisconsin" & name %in% c('Racine','Walworth','Kenosha','Milwaukee','Waukesha')) | (state_name=="Indiana" & name %in% c('Lake','Porter','Newton','Jasper','LaPorte','Starke','Pulaski'))) %>% st_transform(counties, crs=32616) 
ggplot() + geom_sf(data=limits.counties,mapping=aes(fill = state_name)) 

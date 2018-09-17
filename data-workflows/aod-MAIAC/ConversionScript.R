library(RCurl)
library(gdalUtils)
library(rgdal)
library(rts)
library(raster)
library(dplyr)
library(MODIStsp)

#Function to convert HDF to Geotiff (Run in Terminal or Jupyter Notebook, NOT RStudio)
HFDtoGEOTIFF <- function (h) {
  hdf.file <- list.files(pattern = h, full.names = TRUE)
  
  sds <- get_subdatasets(hdf.file)
  
  #Choose the file you want to work with (Optical Depth 47)
  hdf.sub <- sds[1]
  
  #Creates a temporary outfile name
  tempname <- rasterTmpFile()  
  
  #Converts the hdf subdataset to a tif file. Returns NULL which is ok
  gdal_translate(hdf.sub, dst_dataset = tempname) 
  modis.r <- raster(tempname)
  
  wd <- getwd()
  
  filename = paste(substr(hdf.sub,19,59),"tif", sep = "")
  outname = paste(wd,filename, sep = "/") #Creates the path and file name for the output geotif. Change "wd" to path of output folder if different than working directory"
  writeRaster(modis.r, filename = outname, format = "GTiff", overwrite = TRUE)
}


#Get list of hdfs to convert
files <- list.files(path = "2016 Chicago MAIAC Data", pattern = "hdf")

for (i in 1:length(files)) {
  HDF_to_GEOTIFF(files[i])
}




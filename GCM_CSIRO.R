#my script
library(rgdal)
library(raster)
library(ncdf4)

mypath<-""#path to ncdf files

csro_mod1_hist<-stack(paste(mypath,filename))


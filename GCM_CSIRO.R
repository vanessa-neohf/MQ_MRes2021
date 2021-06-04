#my script
library(rgdal)
library(raster)
library(ncdf4)

#path to ncdf files
mypath<-"D:/GCM_Data"

csro_mod1_hist<- stack(paste(mypath,
                            "tos_day_ACCESS1-0_historical_r3i1p1_19700101-19791231.nc", 
                            sep = "/"), varname = "tos")

csro_mod1_hist

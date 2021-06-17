#my script
library(rgdal)
library(raster)
library(ncdf4)
library(tidyverse)

#read coral core / GCM coordinate
setwd("D:/Coral_Proxy_Data")
Coral_Core <- data.frame(read_csv("Coral_Core_Coordinates.csv"))

#path to ncdf files of tos, daily data
mypath<-"D:/GCM_Data"

gcmdata<-list.files(mypath)
setwd(mypath)
access1r3<-stack(gcmdata[13:18])


#ACCESS1_0 Historical - r1i1p1 ensemble

ACCESS1_0_r1i1p1_hist<- brick(paste(mypath,
                            "ACCESS1-0_r1i1p1_tos_day_historical_1970_2005_combined.nc", 
                            sep = "/"), varname = "tos", stopIfNotEqualSpaced = F)

##extract test
a<-extract(Coral_Core[,5:4],ACCESS1_0_r1i1p1_hist)

ACCESS1_0_r1i1p1_hist

#ACCESS1_0 Historical - r2i1p1 ensemble

#ACCESS1_0 Historical - r3i1p1 ensemble

#ACCESS1_0 RCP85 - r1i1p1 ensemble

#ACCESS1_3 Historical - r1i1p1 ensemble

#ACCESS1_3 Historical - r2i1p1 ensemble

#ACCESS1_3 Historical - r3i1p1 ensemble

#ACCESS1_3 RCP85 - r1i1p1 ensemble

#CSIRO_MK3_6_0 Historical - r1i1p1 ensemble

#CSIRO_MK3_6_0 Historical - r2i1p1 ensemble

#CSIRO_MK3_6_0 Historical - r3i1p1 ensemble

#CSIRO_MK3_6_0 Historical - r4i1p1 ensemble

#CSIRO_MK3_6_0 Historical - r5i1p1 ensemble

#CSIRO_MK3_6_0 Historical - r6i1p1 ensemble

#CSIRO_MK3_6_0 Historical - r7i1p1 ensemble

#CSIRO_MK3_6_0 Historical - r8i1p1 ensemble

#CSIRO_MK3_6_0 Historical - r9i1p1 ensemble

#CSIRO_MK3_6_0 Historical - r10i1p1 ensemble

#CSIRO_MK3_6_0 RCP26 - r1i1p1 ensemble

#CSIRO_MK3_6_0 RCP26 - r2i1p1 ensemble

#CSIRO_MK3_6_0 RCP26 - r3i1p1 ensemble

#CSIRO_MK3_6_0 RCP26 - r4i1p1 ensemble

#CSIRO_MK3_6_0 RCP26 - r5i1p1 ensemble

#CSIRO_MK3_6_0 RCP26 - r6i1p1 ensemble

#CSIRO_MK3_6_0 RCP26 - r7i1p1 ensemble

#CSIRO_MK3_6_0 RCP26 - r8i1p1 ensemble

#CSIRO_MK3_6_0 RCP26 - r9i1p1 ensemble

#CSIRO_MK3_6_0 RCP26 - r10i1p1 ensemble

#CSIRO_MK3_6_0 RCP85 - r1i1p1 ensemble

#CSIRO_MK3_6_0 RCP85 - r2i1p1 ensemble

#CSIRO_MK3_6_0 RCP85 - r3i1p1 ensemble

#CSIRO_MK3_6_0 RCP85 - r4i1p1 ensemble

#CSIRO_MK3_6_0 RCP85 - r5i1p1 ensemble

#CSIRO_MK3_6_0 RCP85 - r6i1p1 ensemble

#CSIRO_MK3_6_0 RCP85 - r7i1p1 ensemble

#CSIRO_MK3_6_0 RCP85 - r8i1p1 ensemble

#CSIRO_MK3_6_0 RCP85 - r9i1p1 ensemble

#CSIRO_MK3_6_0 RCP85 - r10i1p1 ensemble
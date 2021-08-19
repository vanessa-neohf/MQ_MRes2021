library(tidyverse)

#read NOAA SST CSV file
#Browse Island, Cocos Keeling, Ningaloo Reef, Houtman Abrolhos
NOAA <- read_csv(here::here("data_raw", "NOAA_SST.csv"))

#read NOAA SST Scott Reef CSV file
#Scott Reef
NOAA_Scott <- read_csv(here::here("data_raw", "NOAA_SST_Scott_Reef.csv"))

unique(NOAA$Reef_Site)
unique(NOAA_Scott$subsite)

#filter SST time series for individual reef sites and convert date to ym format
#BRS05
NOAA_BRS05 <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "BRS05")
  
write_csv(NOAA_BRS05, "D:/NOAA_Data/NOAA_BRS05_SST.csv")

#BRS07
NOAA_BRS07 <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "BRS07")

write_csv(NOAA_BRS07, "D:/NOAA_Data/NOAA_BRS07_SST.csv")

#DAR Long
NOAA_DAR_Long <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "DAR Long")

write_csv(NOAA_DAR_Long, "D:/NOAA_Data/NOAA_DAR_Long_SST.csv")

#DAR3
NOAA_DAR3 <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "DAR3")

write_csv(NOAA_DAR3, "D:/NOAA_Data/NOAA_DAR3_SST.csv")

#Tantabiddi(13TNT)
NOAA_Tantabiddi_13TNT <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "Tantabiddi (13TNT)")

write_csv(NOAA_Tantabiddi_13TNT, "D:/NOAA_Data/NOAA_Tantabiddi_13TNT_SST.csv")

#Tantabiddi(08TNT)
NOAA_Tantabiddi_08TNT <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "Tantabiddi (08TNT)")

write_csv(NOAA_Tantabiddi_08TNT, "D:/NOAA_Data/NOAA_Tantabiddi_08TNT_SST.csv")

#Bundegi (13BND)
NOAA_Bundegi_13BND <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "Bundegi (13BND)")

write_csv(NOAA_Bundegi_13BND, "D:/NOAA_Data/NOAA_Bundegi_13BND_SST.csv")

#Bundegi (08BND)
NOAA_Bundegi_08BND <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "Bundegi (08BND)")

write_csv(NOAA_Bundegi_08BND, "D:/NOAA_Data/NOAA_Bundegi_08BND_SST.csv")

#TNT07C
NOAA_TNT07C <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "TNT07C")

write_csv(NOAA_TNT07C, "D:/NOAA_Data/NOAA_TNT07C_SST.csv")

#BUN05A
NOAA_BUN05A <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "BUN05A")

write_csv(NOAA_BUN05A, "D:/NOAA_Data/NOAA_BUN05A_SST.csv")

#TNT
NOAA_TNT <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "TNT")

write_csv(NOAA_TNT, "D:/NOAA_Data/NOAA_TNT_NSST.csv")

#Wallabi Island
NOAA_Wallabi_Island <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "Wallabi Island")

write_csv(NOAA_Wallabi_Island, "D:/NOAA_Data/NOAA_Wallabi_Island_SST.csv")

#HAB10A (contains both Sr/Ca and d180 proxies)
NOAA_HAB10A <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "HAB10A")

#HAB10A (Sr/Ca Proxy)
NOAA_HAB10A_SrCa <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "HAB10A" & Data_Type == "Coral Core Sr/Ca Proxy")

write_csv(NOAA_HAB10A_SrCa, "D:/NOAA_Data/NOAA_HAB10A_SrCa_Proxy_SST.csv")

#HAB10A (d18O Proxy)
NOAA_HAB10A_d18O <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "HAB10A" & Data_Type == "Coral Core d18O Proxy")

write_csv(NOAA_HAB10A_d18O, "D:/NOAA_Data/NOAA_HAB10A_d18O_Proxy_SST.csv")

#HAB05B
NOAA_HAB05B <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "HAB05B")

#HAB05B (Sr/Ca Proxy)
NOAA_HAB05B_SrCa <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "HAB05B" & Data_Type == "Coral Core Sr/Ca Proxy")

write_csv(NOAA_HAB05B_SrCa, "D:/NOAA_Data/NOAA_HAB05B_SrCa_Proxy_SST.csv")

#HAB05B (d18O Proxy)
NOAA_HAB05B_d18O <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "HAB05B" & Data_Type == "Coral Core d18O Proxy")

write_csv(NOAA_HAB05B_d18O, "D:/NOAA_Data/NOAA_HAB05B_d18O_Proxy_SST.csv")

#SCOTT_RPO_1
NOAA_SCOTT_RPO_1 <- NOAA_Scott %>% 
  mutate(date = format(Date, "%Y-%b-%d")) %>% 
  filter(subsite == "SCOTT_RPO_1")

write_csv(NOAA_SCOTT_RPO_1, "C:/Users/Sam/Documents/Git/MQ_MRes2021/data_raw/NOAA_SCOTT_RPO_1_SST.csv")

#SCOTT_SL4
NOAA_SCOTT_SL4 <- NOAA_Scott %>% 
  mutate(date = format(Date, "%Y-%b-%d")) %>% 
  filter(subsite == "SCOTT_SL4")

write_csv(NOAA_SCOTT_SL4, "C:/Users/Sam/Documents/Git/MQ_MRes2021/data_raw/NOAA_SCOTT_SL4_SST.csv")

#SCOTT_SS3
NOAA_SCOTT_SS3 <- NOAA_Scott %>% 
  mutate(date = format(Date, "%Y-%b-%d")) %>% 
  filter(subsite == "SCOTT_SS3")

write_csv(NOAA_SCOTT_SS3, "C:/Users/Sam/Documents/Git/MQ_MRes2021/data_raw/NOAA_SCOTT_SS3_SST.csv")

#SCOTTNR1
NOAA_SCOTTNR1 <- NOAA_Scott %>% 
  mutate(date = format(Date, "%Y-%b-%d")) %>% 
  filter(subsite == "SCOTTNR1")

write_csv(NOAA_SCOTTNR1, "C:/Users/Sam/Documents/Git/MQ_MRes2021/data_raw/NOAA_SCOTTNR1_SST.csv")

#SCOTTSL1
NOAA_SCOTTSL1 <- NOAA_Scott %>% 
  mutate(date = format(Date, "%Y-%b-%d")) %>% 
  filter(subsite == "SCOTTSL1")

write_csv(NOAA_SCOTTSL1, "C:/Users/Sam/Documents/Git/MQ_MRes2021/data_raw/NOAA_SCOTTSL1_SST.csv")

#SCOTTSL2
NOAA_SCOTTSL2 <- NOAA_Scott %>% 
  mutate(date = format(Date, "%Y-%b-%d")) %>% 
  filter(subsite == "SCOTTSL2")

write_csv(NOAA_SCOTTSL2, "C:/Users/Sam/Documents/Git/MQ_MRes2021/data_raw/NOAA_SCOTTSL2_SST.csv")

#SCOTTSL3
NOAA_SCOTTSL3 <- NOAA_Scott %>% 
  mutate(date = format(Date, "%Y-%b-%d")) %>% 
  filter(subsite == "SCOTTSL3")

write_csv(NOAA_SCOTTSL3, "C:/Users/Sam/Documents/Git/MQ_MRes2021/data_raw/NOAA_SCOTTSL3_SST.csv")

#SCOTTSS1
NOAA_SCOTTSS1 <- NOAA_Scott %>% 
  mutate(date = format(Date, "%Y-%b-%d")) %>% 
  filter(subsite == "SCOTTSS1")

write_csv(NOAA_SCOTTSS1, "C:/Users/Sam/Documents/Git/MQ_MRes2021/data_raw/NOAA_SCOTTSS1_SST.csv")

#SCOTTSS2
NOAA_SCOTTSS2 <- NOAA_Scott %>% 
  mutate(date = format(Date, "%Y-%b-%d")) %>% 
  filter(subsite == "SCOTTSS2")

write_csv(NOAA_SCOTTSS2, "C:/Users/Sam/Documents/Git/MQ_MRes2021/data_raw/NOAA_SCOTTSS2_SST.csv")

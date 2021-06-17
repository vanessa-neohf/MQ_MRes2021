library(tidyverse)

#read NOAA SST CSV file
NOAA <- read_csv("NOAA_SST.csv")

unique(NOAA$Reef_Site)

#filter SST time series for individual reef sites and convert date to ym format
#BRS05
NOAA_BRS05 <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "BRS05")
  
write_csv(NOAA_BRS05, "D:/NOAA_Data/BRS05_NOAA_SST.csv")

#BRS07
NOAA_BRS07 <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "BRS07")

write_csv(NOAA_BRS07, "D:/NOAA_Data/BRS07_NOAA_SST.csv")

#DAR Long
NOAA_DAR_Long <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "DAR Long")

write_csv(NOAA_DAR_Long, "D:/NOAA_Data/DAR_Long_NOAA_SST.csv")

#DAR3
NOAA_DAR3 <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "DAR3")

write_csv(NOAA_DAR3, "D:/NOAA_Data/DAR3_NOAA_SST.csv")

#Tantabiddi(13TNT)
NOAA_Tantabiddi_13TNT <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "Tantabiddi (13TNT)")

write_csv(NOAA_Tantabiddi_13TNT, "D:/NOAA_Data/Tantabiddi_13TNT_NOAA_SST.csv")

#Tantabiddi(08TNT)
NOAA_Tantabiddi_08TNT <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "Tantabiddi (08TNT)")

write_csv(NOAA_Tantabiddi_08TNT, "D:/NOAA_Data/Tantabiddi_08TNT_NOAA_SST.csv")

#Bundegi (13BND)
NOAA_Bundegi_13BND <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "Bundegi (13BND)")

write_csv(NOAA_Bundegi_13BND, "D:/NOAA_Data/Bundegi_13BND_NOAA_SST.csv")

#Bundegi (08BND)
NOAA_Bundegi_08BND <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "Bundegi (08BND)")

write_csv(NOAA_Bundegi_08BND, "D:/NOAA_Data/Bundegi_08BND_NOAA_SST.csv")

#TNT07C
NOAA_TNT07C <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "TNT07C")

write_csv(NOAA_TNT07C, "D:/NOAA_Data/TNT07C_NOAA_SST.csv")

#BUN05A
NOAA_BUN05A <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "BUN05A")

write_csv(NOAA_BUN05A, "D:/NOAA_Data/BUN05A_NOAA_SST.csv")

#TNT
NOAA_TNT <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "TNT")

write_csv(NOAA_TNT, "D:/NOAA_Data/TNT_NOAA_SST.csv")

#Wallabi Island
NOAA_Wallabi_Island <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "Wallabi Island")

write_csv(NOAA_Wallabi_Island, "D:/NOAA_Data/Wallabi_Island_NOAA_SST.csv")

#HAB10A (contains both Sr/Ca and d180 proxies)
NOAA_HAB10A <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "HAB10A")

#HAB10A (Sr/Ca Proxy)
NOAA_HAB10A_SrCa <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "HAB10A" & Data_Type == "Coral Core Sr/Ca Proxy")

write_csv(NOAA_HAB10A_SrCa, "D:/NOAA_Data/HAB10A_SrCa_Proxy_NOAA_SST.csv")

#HAB10A (d18O Proxy)
NOAA_HAB10A_d18O <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "HAB10A" & Data_Type == "Coral Core d18O Proxy")

write_csv(NOAA_HAB10A_d18O, "D:/NOAA_Data/HAB10A_d18O_Proxy_NOAA_SST.csv")

#HAB05B
NOAA_HAB05B <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "HAB05B")

#HAB05B (Sr/Ca Proxy)
NOAA_HAB05B_SrCa <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "HAB05B" & Data_Type == "Coral Core Sr/Ca Proxy")

write_csv(NOAA_HAB05B_SrCa, "D:/NOAA_Data/HAB05B_SrCa_Proxy_NOAA_SST.csv")

#HAB05B (d18O Proxy)
NOAA_HAB05B_d18O <- NOAA %>% 
  mutate(date = format(Date, "%Y-%b")) %>% 
  filter(Reef_Site == "HAB05B" & Data_Type == "Coral Core d18O Proxy")

write_csv(NOAA_HAB05B_d18O, "D:/NOAA_Data/HAB05B_d18O_Proxy_NOAA_SST.csv")

library(tidyverse)
library(lubridate)

#read CCI SST data of individual sites
BRS05 <- read_csv("CCI_BRS05.csv")
BRS07 <- read_csv("CCI_BRS07.csv")
BUN05A <- read_csv("CCI_BUN05A.csv")
Bundegi <- read_csv("CCI_DAR3.csv")
DAR3 <- read_csv("CCI_DAR3.csv")
DARL <- read_csv("CCI_DARL.csv")
HAB05B <- read_csv("CCI_HAB05B.csv")
HAB10A <- read_csv("CCI_HAB10A.csv")
SCOTT_RPO_1 <- read_csv("CCI_SCOTT_RPO_1.csv", skip = 2)
SCOTT_SL4 <- read_csv("CCI_SCOTT_SL4.csv", skip = 2)
SCOTT_SS3 <- read_csv("CCI_SCOTT_SS3.csv", skip = 2)
SCOTTNR1 <- read_csv("CCI_SCOTTNR1.csv", skip = 2)
SCOTTSL1 <- read_csv("CCI_SCOTTSL1.csv", skip = 2)
SCOTTSL2 <- read_csv("CCI_SCOTTSL2.csv", skip = 2)
SCOTTSL3 <- read_csv("CCI_SCOTTSL3.csv", skip = 2)
SCOTTSS1 <- read_csv("CCI_SCOTTSS1.csv", skip = 2)
SCOTTSS2 <- read_csv("CCI_SCOTTSS2.csv", skip = 2)
Tantabiddi <- read_csv("CCI_Tantabiddi.csv")
TNT <- read_csv("CCI_TNT.csv")
TNT07C <- read_csv("CCI_TNT07C.csv")
Wallabi_Island <- read_csv("CCI_Wallabi_Island.csv")

#combine year, month, and day into a single variable
#derive new columns of daily and monthly dates

#BRS05
BRS05 <- BRS05 %>% 
  mutate(daily_date = str_c(year, month, day, sep = "-")) %>% 
  mutate(daily_date = ymd(daily_date)) %>% 
  mutate(monthly_date = format(daily_date, "%Y-%b"))

#BRS07
BRS07 <- BRS07 %>% 
  mutate(daily_date = str_c(year, month, day, sep = "-")) %>% 
  mutate(daily_date = ymd(daily_date)) %>% 
  mutate(monthly_date = format(daily_date, "%Y-%b"))

#BUN05A
BUN05A <- BUN05A %>% 
  mutate(daily_date = str_c(year, month, day, sep = "-")) %>% 
  mutate(daily_date = ymd(daily_date)) %>% 
  mutate(monthly_date = format(daily_date, "%Y-%b"))

#Bundegi
Bundegi <- Bundegi %>% 
  mutate(daily_date = str_c(year, month, day, sep = "-")) %>% 
  mutate(daily_date = ymd(daily_date)) %>% 
  mutate(monthly_date = format(daily_date, "%Y-%b"))

#DAR3
DAR3 <- DAR3 %>% 
  mutate(daily_date = str_c(year, month, day, sep = "-")) %>% 
  mutate(daily_date = ymd(daily_date)) %>% 
  mutate(monthly_date = format(daily_date, "%Y-%b"))

#DARL
DARL <- DARL %>% 
  mutate(daily_date = str_c(year, month, day, sep = "-")) %>% 
  mutate(daily_date = ymd(daily_date)) %>% 
  mutate(monthly_date = format(daily_date, "%Y-%b"))

#HAB05B
HAB05B <- HAB05B %>% 
  mutate(daily_date = str_c(year, month, day, sep = "-")) %>% 
  mutate(daily_date = ymd(daily_date)) %>% 
  mutate(monthly_date = format(daily_date, "%Y-%b"))

#HAB10A
HAB10A <- HAB10A %>% 
  mutate(daily_date = str_c(year, month, day, sep = "-")) %>% 
  mutate(daily_date = ymd(daily_date)) %>% 
  mutate(monthly_date = format(daily_date, "%Y-%b"))

#SCOTT_RPO_1
SCOTT_RPO_1 <- SCOTT_RPO_1 %>% 
  mutate(daily_date = str_c(year, month, day, sep = "-")) %>% 
  mutate(daily_date = ymd(daily_date)) %>% 
  mutate(monthly_date = format(daily_date, "%Y-%b"))

#SCOTT_SL4
SCOTT_SL4 <- SCOTT_SL4 %>% 
  mutate(daily_date = str_c(year, month, day, sep = "-")) %>% 
  mutate(daily_date = ymd(daily_date)) %>% 
  mutate(monthly_date = format(daily_date, "%Y-%b"))

#SCOTT_SS3
SCOTT_SS3 <- SCOTT_SS3 %>% 
  mutate(daily_date = str_c(year, month, day, sep = "-")) %>% 
  mutate(daily_date = ymd(daily_date)) %>% 
  mutate(monthly_date = format(daily_date, "%Y-%b"))

#SCOTTNR1
SCOTTNR1 <- SCOTTNR1 %>% 
  mutate(daily_date = str_c(year, month, day, sep = "-")) %>% 
  mutate(daily_date = ymd(daily_date)) %>% 
  mutate(monthly_date = format(daily_date, "%Y-%b"))

#SCOTTSL1
SCOTTSL1 <- SCOTTSL1 %>% 
  mutate(daily_date = str_c(year, month, day, sep = "-")) %>% 
  mutate(daily_date = ymd(daily_date)) %>% 
  mutate(monthly_date = format(daily_date, "%Y-%b"))

#SCOTTSL2
SCOTTSL2 <- SCOTTSL2 %>% 
  mutate(daily_date = str_c(year, month, day, sep = "-")) %>% 
  mutate(daily_date = ymd(daily_date)) %>% 
  mutate(monthly_date = format(daily_date, "%Y-%b"))

#SCOTTSL3
SCOTTSL3 <- SCOTTSL3 %>% 
  mutate(daily_date = str_c(year, month, day, sep = "-")) %>% 
  mutate(daily_date = ymd(daily_date)) %>% 
  mutate(monthly_date = format(daily_date, "%Y-%b"))

#SCOTTSS1
SCOTTSS1 <- SCOTTSS1 %>% 
  mutate(daily_date = str_c(year, month, day, sep = "-")) %>% 
  mutate(daily_date = ymd(daily_date)) %>% 
  mutate(monthly_date = format(daily_date, "%Y-%b"))

#SCOTTSS2
SCOTTSS2 <- SCOTTSS2 %>% 
  mutate(daily_date = str_c(year, month, day, sep = "-")) %>% 
  mutate(daily_date = ymd(daily_date)) %>% 
  mutate(monthly_date = format(daily_date, "%Y-%b"))

#Tantabiddi
Tantabiddi <- Tantabiddi %>% 
  mutate(daily_date = str_c(year, month, day, sep = "-")) %>% 
  mutate(daily_date = ymd(daily_date)) %>% 
  mutate(monthly_date = format(daily_date, "%Y-%b"))

#TNT
TNT <- TNT %>% 
  mutate(daily_date = str_c(year, month, day, sep = "-")) %>% 
  mutate(daily_date = ymd(daily_date)) %>% 
  mutate(monthly_date = format(daily_date, "%Y-%b"))

#TNT07C
TNT07C <- TNT07C %>% 
  mutate(daily_date = str_c(year, month, day, sep = "-")) %>% 
  mutate(daily_date = ymd(daily_date)) %>% 
  mutate(monthly_date = format(daily_date, "%Y-%b"))

#Wallabi Island
Wallabi_Island <- Wallabi_Island %>% 
  mutate(daily_date = str_c(year, month, day, sep = "-")) %>% 
  mutate(daily_date = ymd(daily_date)) %>% 
  mutate(monthly_date = format(daily_date, "%Y-%b"))

#update CSV file in Git
write_csv(BRS05, "C:/Users/46217029/Documents/Git/MQ_MRes2021/CCI_BRS05.csv")
write_csv(BRS07, "C:/Users/46217029/Documents/Git/MQ_MRes2021/CCI_BRS07.csv")
write_csv(BUN05A, "C:/Users/46217029/Documents/Git/MQ_MRes2021/CCI_BUN05A.csv")
write_csv(Bundegi, "C:/Users/46217029/Documents/Git/MQ_MRes2021/CCI_Bundegi.csv")
write_csv(DAR3, "C:/Users/46217029/Documents/Git/MQ_MRes2021/CCI_DAR3.csv")
write_csv(DARL, "C:/Users/46217029/Documents/Git/MQ_MRes2021/CCI_DARL.csv")
write_csv(HAB05B, "C:/Users/46217029/Documents/Git/MQ_MRes2021/CCI_HAB05B.csv")
write_csv(HAB10A, "C:/Users/46217029/Documents/Git/MQ_MRes2021/CCI_HAB10A.csv")
write_csv(SCOTT_RPO_1, "C:/Users/Sam/Documents/Git/MQ_MRes2021/data_raw/CCI_SCOTT_RPO_1.csv")
write_csv(SCOTT_SL4, "C:/Users/Sam/Documents/Git/MQ_MRes2021/data_raw/CCI_SCOTT_SL4.csv")
write_csv(SCOTT_SS3, "C:/Users/Sam/Documents/Git/MQ_MRes2021/data_raw/CCI_SCOTT_SS3.csv")
write_csv(SCOTTNR1, "C:/Users/Sam/Documents/Git/MQ_MRes2021/data_raw/CCI_SCOTTNR1.csv")
write_csv(SCOTTSL1, "C:/Users/Sam/Documents/Git/MQ_MRes2021/data_raw/CCI_SCOTTSL1.csv")
write_csv(SCOTTSL2, "C:/Users/Sam/Documents/Git/MQ_MRes2021/data_raw/CCI_SCOTTSL2.csv")
write_csv(SCOTTSL3, "C:/Users/Sam/Documents/Git/MQ_MRes2021/data_raw/CCI_SCOTTSL3.csv")
write_csv(SCOTTSS1, "C:/Users/Sam/Documents/Git/MQ_MRes2021/data_raw/CCI_SCOTTSS1.csv")
write_csv(SCOTTSS2, "C:/Users/Sam/Documents/Git/MQ_MRes2021/data_raw/CCI_SCOTTSS2.csv")
write_csv(Tantabiddi, "C:/Users/46217029/Documents/Git/MQ_MRes2021/CCI_Tantabiddi.csv")
write_csv(TNT, "C:/Users/46217029/Documents/Git/MQ_MRes2021/CCI_TNT.csv")
write_csv(TNT07C, "C:/Users/46217029/Documents/Git/MQ_MRes2021/CCI_TNT07C.csv")
write_csv(Wallabi_Island, "C:/Users/46217029/Documents/Git/MQ_MRes2021/CCI_Wallabi_Island.csv")

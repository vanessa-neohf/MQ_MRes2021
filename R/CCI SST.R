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
write_csv(Tantabiddi, "C:/Users/46217029/Documents/Git/MQ_MRes2021/CCI_Tantabiddi.csv")
write_csv(TNT, "C:/Users/46217029/Documents/Git/MQ_MRes2021/CCI_TNT.csv")
write_csv(TNT07C, "C:/Users/46217029/Documents/Git/MQ_MRes2021/CCI_TNT07C.csv")
write_csv(Wallabi_Island, "C:/Users/46217029/Documents/Git/MQ_MRes2021/CCI_Wallabi_Island.csv")

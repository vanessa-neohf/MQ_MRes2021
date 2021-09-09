library(tidyverse)
library(lubridate)
library(tsibble)

#reading raw data for tsclust analysis

##BRS05
Browse_05_CCI <- read_csv(here::here("data_raw", "CCI_BRS05.csv")) 
Browse_05_CCI <- Browse_05_CCI %>% 
  mutate(sst = as.numeric(str_sub(Browse_05_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>% #select only useful columns 
  as_tsibble(index = date) # convert them to tsibble

Browse_05_CCore <- read_csv(here::here("data_raw", "CCore_BRS05_BRS07_Year_Month.csv"), n_max = 740) %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = yearmonth(date)) %>% #converting date column fr chr to date format
  select(data_type, date, `BRS05 Sr/Ca [mmol/mol]`) %>%  #select only useful columns 
  as_tsibble(index = date) # convert them to tsibble 

Browse_05_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_SCOTTSS1.csv")) %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst)  %>% #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Browse_05_NOAA <- read_csv(here::here("data_raw", "NOAA_BRS05_SST.csv")) %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

##BRS07
Browse_07_CCI <- read_csv(here::here("data_raw", "CCI_BRS07.csv"))
Browse_07_CCI <- Browse_07_CCI %>% 
  mutate(sst = as.numeric(str_sub(Browse_07_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>% #select only useful columns 
  as_tsibble(index = date) # convert them to tsibble

Browse_07_CCore <- read_csv(here::here("data_raw", "CCore_BRS05_BRS07_Year_Month.csv"), n_max = 740) %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = yearmonth(date)) %>% #converting date column fr chr to date format
  select(data_type, date, `BRS07 Sr/Ca [mmol/mol]`) %>%  #select only useful columns 
  as_tsibble(index = date) # convert them to tsibble 

Browse_07_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_SCOTTSS1.csv")) %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst)  %>% #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Browse_07_NOAA <- read_csv(here::here("data_raw", "NOAA_BRS07_SST.csv")) %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

##DAR Long
DARL_CCI <- read_csv(here::here("data_raw", "CCI_DARL.csv"))
DARL_CCI <- DARL_CCI %>% 
  mutate(sst = as.numeric(str_sub(DARL_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

DARL_CCore <- read_csv(here::here("data_raw", "CCore_DAR_Long_Year_Month.csv"))
DARL_CCore <- DARL_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% 
  mutate(date = ym(date)) %>% 
  select(data_type, date, `Sr/Ca`) %>%     #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

DARL_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_100THSITE.csv"))
DARL_Logger <- DARL_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst) %>%   #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

DARL_NOAA <- read_csv(here::here("data_raw", "NOAA_DAR_Long_SST.csv"))
DARL_NOAA <- DARL_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

##DAR3
DAR3_CCI <- read_csv(here::here("data_raw", "CCI_DAR3.csv"))
DAR3_CCI <- DAR3_CCI %>% 
  mutate(sst = as.numeric(str_sub(DAR3_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

DAR3_CCore <- read_csv(here::here("data_raw", "CCore_DAR3_Year_Month.csv"))
DAR3_CCore <- DAR3_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% 
  mutate(date = ym(date)) %>% 
  select(data_type, date, `Sr/Ca`) %>%     #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

DAR3_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_100THSITE.csv"))
DAR3_Logger <- DAR3_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst) %>%   #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

DAR3_NOAA <- read_csv(here::here("data_raw", "NOAA_DAR3_SST.csv"))
DAR3_NOAA <- DAR3_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

##Tantabiddi 13TNT
Ningaloo_13TNT_CCI <- read_csv(here::here("data_raw", "CCI_Tantabiddi.csv"))
Ningaloo_13TNT_CCI <- Ningaloo_13TNT_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_13TNT_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_13TNT_CCore <- read_csv(here::here("data_raw", "CCore_Ningaloo_13TNT_Year_Month.csv"))
Ningaloo_13TNT_CCore <- Ningaloo_13TNT_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = as_date(date)) %>% 
  select(data_type, date, `Sr/Ca`) %>%     #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_13TNT_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_TANDFL1.csv"))
Ningaloo_13TNT_Logger <- Ningaloo_13TNT_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst) %>%   #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_13TNT_NOAA <- read_csv(here::here("data_raw", "NOAA_Tantabiddi_13TNT_SST.csv"))
Ningaloo_13TNT_NOAA <- Ningaloo_13TNT_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

##Tantabiddi 08TNT
Ningaloo_08TNT_CCI <- read_csv(here::here("data_raw", "CCI_Tantabiddi.csv"))
Ningaloo_08TNT_CCI <- Ningaloo_08TNT_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_08TNT_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_08TNT_CCore <- read_csv(here::here("data_raw", "CCore_Ningaloo_08TNT_Year_Month.csv"))
Ningaloo_08TNT_CCore <- Ningaloo_08TNT_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = as_date(date)) %>% 
  select(data_type, date, `Sr/Ca`) %>%     #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_08TNT_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_TANDFL1.csv"))
Ningaloo_08TNT_Logger <- Ningaloo_08TNT_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst) %>%   #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_08TNT_NOAA <- read_csv(here::here("data_raw", "NOAA_Tantabiddi_08TNT_SST.csv"))
Ningaloo_08TNT_NOAA <- Ningaloo_08TNT_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

##Bundegi 13BND
Ningaloo_13BND_CCI <- read_csv(here::here("data_raw", "CCI_Bundegi.csv"))
Ningaloo_13BND_CCI <- Ningaloo_13BND_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_13BND_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_13BND_CCore <- read_csv(here::here("data_raw", "CCore_Ningaloo_13BND_Year_Month.csv"))
Ningaloo_13BND_CCore <- Ningaloo_13BND_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = as_date(date)) %>% 
  select(data_type, date, `Sr/Ca`) %>%     #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_13BND_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_BUNDFL1.csv"))
Ningaloo_13BND_Logger <- Ningaloo_13BND_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst) %>%   #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_13BND_NOAA <- read_csv(here::here("data_raw", "NOAA_Bundegi_13BND_SST.csv"))
Ningaloo_13BND_NOAA <- Ningaloo_13BND_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

##Bundegi 08BND
Ningaloo_08BND_CCI <- read_csv(here::here("data_raw", "CCI_Bundegi.csv"))
Ningaloo_08BND_CCI <- Ningaloo_08BND_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_08BND_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_08BND_CCore <- read_csv(here::here("data_raw", "CCore_Ningaloo_08BND_Year_Month.csv"))
Ningaloo_08BND_CCore <- Ningaloo_08BND_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = as_date(date)) %>% 
  select(data_type, date, `Sr/Ca`) %>%     #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_08BND_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_BUNDFL1.csv"))
Ningaloo_08BND_Logger <- Ningaloo_08BND_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst) %>%   #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_08BND_NOAA <- read_csv(here::here("data_raw", "NOAA_Bundegi_08BND_SST.csv"))
Ningaloo_08BND_NOAA <- Ningaloo_08BND_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

##TNT

Ningaloo_TNT_CCI <- read_csv(here::here("data_raw", "CCI_TNT.csv"))
Ningaloo_TNT_CCI <- Ningaloo_TNT_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_TNT_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_TNT_CCore <- read_csv(here::here("data_raw", "CCore_Ningaloo_TNT_Year_Month.csv"))
Ningaloo_TNT_CCore <- Ningaloo_TNT_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = as_date(date)) %>% 
  select(data_type, date, `d18O`) %>%     #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_TNT_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_TANDFL1.csv"))
Ningaloo_TNT_Logger <- Ningaloo_TNT_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst) %>%   #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_TNT_NOAA <- read_csv(here::here("data_raw", "NOAA_TNT_SST.csv"))
Ningaloo_TNT_NOAA <- Ningaloo_TNT_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

##TNT07C
##Only annual coral core data

Ningaloo_TNT07C_CCI <- read_csv(here::here("data_raw", "CCI_TNT07C.csv"))
Ningaloo_TNT07C_CCI <- Ningaloo_TNT07C_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_TNT07C_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_TNT07C_CCore <- read_csv(here::here("data_raw", "CCore_Ningaloo_TNT07C_year.csv"))

Ningaloo_TNT07C_CCore <- Ningaloo_TNT07C_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(month = "01") %>% 
  mutate(date = str_c(year, month, sep = "-")) %>% 
  mutate(date = ym(date)) %>% 
  select(data_type, date, `Tantabiddi Sr/Ca`) %>% 
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_TNT07C_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_TANTABIDDI_SL1.csv"))
Ningaloo_TNT07C_Logger <- Ningaloo_TNT07C_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst) %>%   #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_TNT07C_NOAA <- read_csv(here::here("data_raw", "NOAA_TNT07C_SST.csv"))
Ningaloo_TNT07C_NOAA <- Ningaloo_TNT07C_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

##BUN05A
##Only annual coral core data

Ningaloo_BUN05A_CCI <- read_csv(here::here("data_raw", "CCI_BUN05A.csv"))
Ningaloo_BUN05A_CCI <- Ningaloo_BUN05A_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_BUN05A_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_BUN05A_CCore <- read_csv(here::here("data_raw", "CCore_Ningaloo_BUN05A_year.csv"))
Ningaloo_BUN05A_CCore <- Ningaloo_BUN05A_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(month = "01") %>% 
  mutate(date = str_c(year, month, sep = "-")) %>% 
  mutate(date = ym(date)) %>% 
  select(data_type, date, `Bundegi Sr/Ca`) %>% 
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_BUN05A_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_BUNDEGI_BR.csv"))
Ningaloo_BUN05A_Logger <- Ningaloo_BUN05A_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst) %>%   #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_BUN05A_NOAA <- read_csv(here::here("data_raw", "NOAA_BUN05A_SST.csv"))
Ningaloo_BUN05A_NOAA <- Ningaloo_BUN05A_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

##Wallabi Island
Wallabi_Island_CCI <- read_csv(here::here("data_raw", "CCI_Wallabi_Island.csv"))
Wallabi_Island_CCI <- Wallabi_Island_CCI %>% 
  mutate(sst = as.numeric(str_sub(Wallabi_Island_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Wallabi_Island_CCore <- read_csv(here::here("data_raw", "CCore_H_Abrol_Wallabi_Year_Month.csv"))
Wallabi_Island_CCore <- Wallabi_Island_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% 
  mutate(date = ym(date)) %>% 
  select(data_type, date, `d18O`) %>%     #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Wallabi_Island_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_NTHFISHFL1.csv"))
Wallabi_Island_Logger <- Wallabi_Island_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst) %>%   #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Wallabi_Island_NOAA <- read_csv(here::here("data_raw", "NOAA_Wallabi_Island_SST.csv"))
Wallabi_Island_NOAA <- Wallabi_Island_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

##HAB10A d18O Proxy
##Only annual coral core data

HAB10A_d18O_CCI <- read_csv(here::here("data_raw", "CCI_HAB10A.csv"))
HAB10A_d18O_CCI <- HAB10A_d18O_CCI %>% 
  mutate(sst = as.numeric(str_sub(HAB10A_d18O_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

HAB10A_d18O_CCore <- read_csv(here::here("data_raw", "CCore_H_Abrol_HAB_year.csv"))

HAB10A_d18O_CCore <- HAB10A_d18O_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(month = "01") %>% 
  mutate(date = str_c(year, month, sep = "-")) %>% 
  mutate(date = ym(date)) %>% 
  select(data_type, date, `HAB10A d18O`) %>% 
  as_tsibble(index = date) # convert them to tsibble

HAB10A_d18O_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_NTHFISHFL1.csv"))
HAB10A_d18O_Logger <- HAB10A_d18O_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst) %>%   #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

HAB10A_d18O_NOAA <- read_csv(here::here("data_raw", "NOAA_HAB10A_d18O_Proxy_SST.csv"))
HAB10A_d18O_NOAA <- HAB10A_d18O_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

##HAB05B d18O Proxy
##Only annual coral core data

HAB05B_d18O_CCI <- read_csv(here::here("data_raw", "CCI_HAB05B.csv"))
HAB05B_d18O_CCI <- HAB05B_d18O_CCI %>% 
  mutate(sst = as.numeric(str_sub(HAB05B_d18O_CCI$`mean temperature deg C`, 3, 7))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

HAB05B_d18O_CCore <- read_csv(here::here("data_raw", "CCore_H_Abrol_HAB_year.csv"))

HAB05B_d18O_CCore <- HAB05B_d18O_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(month = "01") %>% 
  mutate(date = str_c(year, month, sep = "-")) %>% 
  mutate(date = ym(date)) %>% 
  select(data_type, date, `HAB05B d18O`) %>% 
  as_tsibble(index = date) # convert them to tsibble

HAB05B_d18O_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_NTHFISHFL1.csv"))
HAB05B_d18O_Logger <- HAB05B_d18O_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst) %>%   #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

HAB05B_d18O_NOAA <- read_csv(here::here("data_raw", "NOAA_HAB05B_d18O_Proxy_SST.csv"))
HAB05B_d18O_NOAA <- HAB05B_d18O_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

##HAB10A Sr/Ca Proxy
##Only annual coral core data

HAB10A_SrCa_CCI <- read_csv(here::here("data_raw", "CCI_HAB10A.csv"))
HAB10A_SrCa_CCI <- HAB10A_SrCa_CCI %>% 
  mutate(sst = as.numeric(str_sub(HAB10A_SrCa_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

HAB10A_SrCa_CCore <- read_csv(here::here("data_raw", "CCore_H_Abrol_HAB_year.csv"))

HAB10A_SrCa_CCore <- HAB10A_SrCa_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(month = "01") %>% 
  mutate(date = str_c(year, month, sep = "-")) %>% 
  mutate(date = ym(date)) %>% 
  select(data_type, date, `HAB10A Sr/Ca`) %>% 
  as_tsibble(index = date) # convert them to tsibble

HAB10A_SrCa_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_NTHFISHFL1.csv"))
HAB10A_SrCa_Logger <- HAB10A_SrCa_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst) %>%   #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

HAB10A_SrCa_NOAA <- read_csv(here::here("data_raw", "NOAA_HAB10A_SrCa_Proxy_SST.csv"))
HAB10A_SrCa_NOAA <- HAB10A_SrCa_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

##HAB05B SrCa Proxy
##Only annual coral core data

HAB05B_SrCa_CCI <- read_csv(here::here("data_raw", "CCI_HAB05B.csv"))
HAB05B_SrCa_CCI <- HAB05B_SrCa_CCI %>% 
  mutate(sst = as.numeric(str_sub(HAB05B_SrCa_CCI$`mean temperature deg C`, 3, 7))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

HAB05B_SrCa_CCore <- read_csv(here::here("data_raw", "CCore_H_Abrol_HAB_year.csv"))

HAB05B_SrCa_CCore <- HAB05B_SrCa_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(month = "01") %>% 
  mutate(date = str_c(year, month, sep = "-")) %>% 
  mutate(date = ym(date)) %>% 
  select(data_type, date, `HAB05B Sr/Ca`) %>% 
  as_tsibble(index = date) # convert them to tsibble

HAB05B_SrCa_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_NTHFISHFL1.csv"))
HAB05B_SrCa_Logger <- HAB05B_SrCa_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst) %>%   #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

HAB05B_SrCa_NOAA <- read_csv(here::here("data_raw", "NOAA_HAB05B_SrCa_Proxy_SST.csv"))
HAB05B_SrCa_NOAA <- HAB05B_SrCa_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble
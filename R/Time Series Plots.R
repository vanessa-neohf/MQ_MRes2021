#Data visualisation for each coral core site 
#Plotting NOAA, CCI, Coral Core, Data Logger, and GCM Sea Surface Temp time series
#Sites DAR Long, SCOTT_RPO_1, SCOTTSL1, BRS07, 08BND, 13BND, 08TNT. 13TNT, HAB10A, HAB05B

#load packages
library(tidyverse)
library(lubridate)
library(patchwork)

##Browse Island##
##BRS05
Browse_05_CCI <- read_csv(here::here("data_raw", "CCI_BRS05.csv"))
Browse_05_CCI <- Browse_05_CCI %>% 
  mutate(sst = as.numeric(str_sub(Browse_05_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

Browse_05_CCore <- read_csv(here::here("data_raw", "CCore_BRS05_BRS07_Year_Month.csv"))
Browse_05_CCore <- Browse_05_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = ym(date)) %>% #converting date column fr chr to date format
  select(data_type, date, `BRS05 Sr/Ca [mmol/mol]`)  #select only useful columns

Browse_05_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_SCOTTSS1.csv"))
Browse_05_Logger <- Browse_05_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(subsite, data_type, date, sst)  #select only useful columns

Browse_05_NOAA <- read_csv(here::here("data_raw", "NOAA_BRS05_SST.csv"))
Browse_05_NOAA <- Browse_05_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

Browse_05_comb <- bind_rows(Browse_05_CCI, Browse_05_Logger, Browse_05_NOAA, Browse_05_CCore)

Plot_1_BRS05 <- Browse_05_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  xlim(as.Date(c("1980-01-01", "2021-01-01"))) +
  geom_smooth(aes(color = data_type))

Plot_2_BRS05 <- Browse_05_CCore %>% 
  ggplot(aes(x=date, y = `BRS05 Sr/Ca [mmol/mol]`)) +
  geom_line(aes(color = data_type)) +
  xlim(as.Date(c("1980-01-01", "2015-01-01"))) +
  geom_smooth(aes(color = data_type))

Plot_1_BRS05 + Plot_2_BRS05  #using Patchwork to bring plots together

##Browse Island##
##BRS07
Browse_07_CCI <- read_csv(here::here("data_raw", "CCI_BRS07.csv"))
Browse_07_CCI <- Browse_07_CCI %>% 
  mutate(sst = as.numeric(str_sub(Browse_07_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

Browse_07_CCore <- read_csv(here::here("data_raw", "CCore_BRS05_BRS07_Year_Month.csv"))
Browse_07_CCore <- Browse_07_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = ym(date)) %>% #converting date column fr chr to date format
  select(data_type, date, `BRS07 Sr/Ca [mmol/mol]`)  #select only useful columns

Browse_07_CCore_CCI <- read_csv(here::here("data_raw", "CCore_SST_BRS07_CCI.csv")) %>% 
  mutate(data_type = "Coral Core CCI calibrated SST", .before = "date") %>% 
  select(data_type, date, sst)

Browse_07_CCore_NOAA <- read_csv(here::here("data_raw", "CCore_SST_BRS07_NOAA.csv")) %>% 
  mutate(data_type = "Coral Core NOAA calibrated SST", .before = "date") %>% 
  select(data_type, date, sst)

Browse_07_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_SCOTTSS1.csv"))
Browse_07_Logger <- Browse_07_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(subsite, data_type, date, sst)  #select only useful columns

Browse_07_NOAA <- read_csv(here::here("data_raw", "NOAA_BRS07_SST.csv"))
Browse_07_NOAA <- Browse_07_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

Browse_07_GCM <- read_csv(here::here("data_raw/GCM", "GCM_BRS07_with_mmm_and_dhw.csv")) %>% 
  select(Date, sst) %>% 
  rename(date = Date) %>% 
  mutate(data_type = "GCM", .before = date)

Browse_07_comb <- bind_rows(Browse_07_CCI, Browse_07_Logger, Browse_07_NOAA, 
                            Browse_07_GCM, Browse_07_CCore_CCI, Browse_07_CCore_NOAA)

Plot_1_BRS07 <- Browse_07_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  xlim(as.Date(c("1980-01-01", "2021-01-01"))) +
  geom_smooth(aes(color = data_type))

Plot_2_BRS07 <- Browse_07_CCore %>% 
  ggplot(aes(x=date, y = `BRS07 Sr/Ca [mmol/mol]`)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type))

Plot_1_BRS07 + Plot_2_BRS07  #using Patchwork to bring plots together


##Cocos(Keeling) Islands##
##DAR Long
Cocos_DARL_CCI <- read_csv(here::here("data_raw", "CCI_DARL.csv"))
Cocos_DARL_CCI <- Cocos_DARL_CCI %>% 
  mutate(sst = as.numeric(str_sub(Cocos_DARL_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

Cocos_DARL_CCore <- read_csv(here::here("data_raw", "CCore_DAR_Long_Year_Month.csv"))
Cocos_DARL_CCore <- Cocos_DARL_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = ym(date)) %>% #converting date column fr chr to date format
  select(data_type, date, `Sr/Ca`)  #select only useful columns

Cocos_DARL_CCore_CCI <- read_csv(here::here("data_raw", "CCore_SST_DARL_CCI.csv")) %>% 
  mutate(data_type = "Coral Core CCI calibrated SST", .before = "date") %>% 
  select(data_type, date, sst)

Cocos_DARL_CCore_NOAA <- read_csv(here::here("data_raw", "CCore_SST_DARL_NOAA.csv")) %>% 
  mutate(data_type = "Coral Core NOAA calibrated SST", .before = "date") %>% 
  select(data_type, date, sst)

Cocos_DARL_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_100THSITE.csv"))
Cocos_DARL_Logger <- Cocos_DARL_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(subsite, data_type, date, sst)  #select only useful columns

Cocos_DARL_NOAA <- read_csv(here::here("data_raw", "NOAA_DAR_Long_SST.csv"))
Cocos_DARL_NOAA <- Cocos_DARL_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

Cocos_DARL_GCM <- read_csv(here::here("data_raw/GCM", "GCM_DAR_Long_with_mmm_and_dhw.csv")) %>% 
  select(Date, sst) %>% 
  rename(date = Date) %>% 
  mutate(data_type = "GCM", .before = date)

Cocos_DARL_comb <- bind_rows(Cocos_DARL_CCI, Cocos_DARL_Logger, Cocos_DARL_NOAA, 
                             Cocos_DARL_GCM, Cocos_DARL_CCore_CCI, Cocos_DARL_CCore_NOAA)

Plot_1_DARL <- Cocos_DARL_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  xlim(as.Date(c("1980-01-01", "2021-01-01"))) +
  geom_smooth(aes(color = data_type))

Plot_2_DARL <- Cocos_DARL_CCore %>% 
  ggplot(aes(x=date, y = `Sr/Ca`)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type))

Plot_1_DARL + Plot_2_DARL  #using Patchwork to bring plots together

##Cocos(Keeling) Islands##
##DAR3
Cocos_DAR3_CCI <- read_csv(here::here("data_raw", "CCI_DAR3.csv"))
Cocos_DAR3_CCI <- Cocos_DAR3_CCI %>% 
  mutate(sst = as.numeric(str_sub(Cocos_DAR3_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

Cocos_DAR3_CCore <- read_csv(here::here("data_raw", "CCore_DAR3_Year_Month.csv"))
Cocos_DAR3_CCore <- Cocos_DAR3_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = ym(date)) %>% #converting date column fr chr to date format
  select(data_type, date, `Sr/Ca`)  #select only useful columns

Cocos_DAR3_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_100THSITE.csv"))
Cocos_DAR3_Logger <- Cocos_DAR3_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(subsite, data_type, date, sst)  #select only useful columns

Cocos_DAR3_NOAA <- read_csv(here::here("data_raw", "NOAA_DAR3_SST.csv"))
Cocos_DAR3_NOAA <- Cocos_DAR3_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

Cocos_DAR3_comb <- bind_rows(Cocos_DAR3_CCI, Cocos_DAR3_CCore, Cocos_DAR3_Logger, Cocos_DAR3_NOAA)

Plot_1_DAR3 <- Cocos_DAR3_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  xlim(as.Date(c("1980-01-01", "2021-01-01"))) +
  geom_smooth(aes(color = data_type))

Plot_2_DAR3 <- Cocos_DAR3_CCore %>% 
  ggplot(aes(x=date, y = `Sr/Ca`)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type))

Plot_1_DAR3 + Plot_2_DAR3  #using Patchwork to bring plots together

##Ningaloo Reef##
##Tantabiddi 13TNT
Ningaloo_13TNT_CCI <- read_csv(here::here("data_raw", "CCI_Tantabiddi.csv"))
Ningaloo_13TNT_CCI <- Ningaloo_13TNT_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_13TNT_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_13TNT_CCore <- read_csv(here::here("data_raw", "CCore_Ningaloo_13TNT_Year_Month.csv"))
Ningaloo_13TNT_CCore <- Ningaloo_13TNT_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = ym(date)) %>% #converting date column fr chr to date format
  select(data_type, date, `Sr/Ca`)  #select only useful columns

Ningaloo_13TNT_CCore_CCI <- read_csv(here::here("data_raw", "CCore_SST_Ningaloo_13TNT_CCI.csv")) %>% 
  mutate(data_type = "Coral Core CCI calibrated SST", .before = "date") %>% 
  select(data_type, date, sst)

Ningaloo_13TNT_CCore_NOAA <- read_csv(here::here("data_raw", "CCore_SST_Ningaloo_13TNT_NOAA.csv")) %>% 
  mutate(data_type = "Coral Core NOAA calibrated SST", .before = "date") %>% 
  select(data_type, date, sst)

Ningaloo_13TNT_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_TANDFL1.csv"))
Ningaloo_13TNT_Logger <- Ningaloo_13TNT_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(subsite, data_type, date, sst)  #select only useful columns

Ningaloo_13TNT_NOAA <- read_csv(here::here("data_raw", "NOAA_Tantabiddi_13TNT_SST.csv"))
Ningaloo_13TNT_NOAA <- Ningaloo_13TNT_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_13TNT_GCM <- read_csv(here::here("data_raw/GCM", "GCM_Tantabiddi_with_mmm_and_dhw.csv")) %>% 
  select(Date, sst) %>% 
  rename(date = Date) %>% 
  mutate(data_type = "GCM", .before = date)

Ningaloo_13TNT_comb <- bind_rows(Ningaloo_13TNT_CCI, Ningaloo_13TNT_Logger, Ningaloo_13TNT_NOAA,
                                 Ningaloo_13TNT_GCM, Ningaloo_13TNT_CCore_CCI, Ningaloo_13TNT_CCore_NOAA)

Plot_1_13TNT <- Ningaloo_13TNT_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  xlim(as.Date(c("1980-01-01", "2021-01-01"))) +
  geom_smooth(aes(color = data_type))

Plot_2_13TNT <- Ningaloo_13TNT_CCore %>% 
  ggplot(aes(x=date, y = `Sr/Ca`)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type))

Plot_1_13TNT + Plot_2_13TNT  #using Patchwork to bring plots together


##Ningaloo Reef##
##Tantabiddi 08TNT
Ningaloo_08TNT_CCI <- read_csv(here::here("data_raw", "CCI_Tantabiddi.csv"))
Ningaloo_08TNT_CCI <- Ningaloo_08TNT_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_08TNT_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_08TNT_CCore <- read_csv(here::here("data_raw", "CCore_Ningaloo_08TNT_Year_Month.csv"))
Ningaloo_08TNT_CCore <- Ningaloo_08TNT_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = ym(date)) %>% #converting date column fr chr to date format
  select(data_type, date, `Sr/Ca`)  #select only useful columns

Ningaloo_08TNT_CCore_CCI <- read_csv(here::here("data_raw", "CCore_SST_Ningaloo_08TNT_CCI.csv")) %>% 
  mutate(data_type = "Coral Core CCI calibrated SST", .before = "date") %>% 
  select(data_type, date, sst)

Ningaloo_08TNT_CCore_NOAA <- read_csv(here::here("data_raw", "CCore_SST_Ningaloo_08TNT_NOAA.csv")) %>% 
  mutate(data_type = "Coral Core NOAA calibrated SST", .before = "date") %>% 
  select(data_type, date, sst)

Ningaloo_08TNT_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_TANDFL1.csv"))
Ningaloo_08TNT_Logger <- Ningaloo_08TNT_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(subsite, data_type, date, sst)  #select only useful columns

Ningaloo_08TNT_NOAA <- read_csv(here::here("data_raw", "NOAA_Tantabiddi_08TNT_SST.csv"))
Ningaloo_08TNT_NOAA <- Ningaloo_08TNT_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_08TNT_GCM <- read_csv(here::here("data_raw/GCM", "GCM_Tantabiddi_with_mmm_and_dhw.csv")) %>% 
  select(Date, sst) %>% 
  rename(date = Date) %>% 
  mutate(data_type = "GCM", .before = date)

Ningaloo_08TNT_comb <- bind_rows(Ningaloo_08TNT_CCI, Ningaloo_08TNT_Logger, Ningaloo_08TNT_NOAA,
                                 Ningaloo_08TNT_GCM, Ningaloo_08TNT_CCore_CCI, Ningaloo_08TNT_CCore_NOAA)

Plot_1_08TNT <- Ningaloo_08TNT_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  xlim(as.Date(c("1980-01-01", "2021-01-01"))) +
  geom_smooth(aes(color = data_type))

Plot_2_08TNT <- Ningaloo_08TNT_CCore %>% 
  ggplot(aes(x=date, y = `Sr/Ca`)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type))

Plot_1_08TNT + Plot_2_08TNT  #using Patchwork to bring plots together


##Ningaloo Reef##
##Bundegi 13BND
Ningaloo_13BND_CCI <- read_csv(here::here("data_raw", "CCI_Bundegi.csv"))
Ningaloo_13BND_CCI <- Ningaloo_13BND_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_13BND_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_13BND_CCore <- read_csv(here::here("data_raw", "CCore_Ningaloo_13BND_Year_Month.csv"))
Ningaloo_13BND_CCore <- Ningaloo_13BND_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = ym(date)) %>% #converting date column fr chr to date format
  select(data_type, date, `Sr/Ca`)  #select only useful columns

Ningaloo_13BND_CCore_CCI <- read_csv(here::here("data_raw", "CCore_SST_Ningaloo_13BND_CCI.csv")) %>% 
  mutate(data_type = "Coral Core CCI calibrated SST", .before = "date") %>% 
  select(data_type, date, sst)

Ningaloo_13BND_CCore_NOAA <- read_csv(here::here("data_raw", "CCore_SST_Ningaloo_13BND_NOAA.csv")) %>% 
  mutate(data_type = "Coral Core NOAA calibrated SST", .before = "date") %>% 
  select(data_type, date, sst)

Ningaloo_13BND_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_BUNDFL1.csv"))
Ningaloo_13BND_Logger <- Ningaloo_13BND_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(subsite, data_type, date, sst)  #select only useful columns

Ningaloo_13BND_NOAA <- read_csv(here::here("data_raw", "NOAA_Bundegi_13BND_SST.csv"))
Ningaloo_13BND_NOAA <- Ningaloo_13BND_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_13BND_GCM <- read_csv(here::here("data_raw/GCM", "GCM_Bundegi_with_mmm_and_dhw.csv")) %>% 
  select(Date, sst) %>% 
  rename(date = Date) %>% 
  mutate(data_type = "GCM", .before = date)

Ningaloo_13BND_comb <- bind_rows(Ningaloo_13BND_CCI, Ningaloo_13BND_Logger, Ningaloo_13BND_NOAA,
                                 Ningaloo_13BND_GCM, Ningaloo_13BND_CCore_CCI, Ningaloo_13BND_CCore_NOAA)

Plot_1_13BND <- Ningaloo_13BND_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  xlim(as.Date(c("1980-01-01", "2021-01-01"))) +
  geom_smooth(aes(color = data_type))

Plot_2_13BND <- Ningaloo_13BND_CCore %>% 
  ggplot(aes(x=date, y = `Sr/Ca`)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type))

Plot_1_13BND + Plot_2_13BND  #using Patchwork to bring plots together


##Ningaloo Reef##
##Bundegi 08BND
Ningaloo_08BND_CCI <- read_csv(here::here("data_raw", "CCI_Bundegi.csv"))
Ningaloo_08BND_CCI <- Ningaloo_08BND_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_08BND_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_08BND_CCore <- read_csv(here::here("data_raw", "CCore_Ningaloo_08BND_Year_Month.csv"))
Ningaloo_08BND_CCore <- Ningaloo_08BND_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = ym(date)) %>% #converting date column fr chr to date format
  select(data_type, date, `Sr/Ca`)  #select only useful columns

Ningaloo_08BND_CCore_CCI <- read_csv(here::here("data_raw", "CCore_SST_Ningaloo_08BND_CCI.csv")) %>% 
  mutate(data_type = "Coral Core CCI calibrated SST", .before = "date") %>% 
  select(data_type, date, sst)

Ningaloo_08BND_CCore_NOAA <- read_csv(here::here("data_raw", "CCore_SST_Ningaloo_08BND_NOAA.csv")) %>% 
  mutate(data_type = "Coral Core NOAA calibrated SST", .before = "date") %>% 
  select(data_type, date, sst)

Ningaloo_08BND_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_BUNDFL1.csv"))
Ningaloo_08BND_Logger <- Ningaloo_08BND_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(subsite, data_type, date, sst)  #select only useful columns

Ningaloo_08BND_NOAA <- read_csv(here::here("data_raw", "NOAA_Bundegi_08BND_SST.csv"))
Ningaloo_08BND_NOAA <- Ningaloo_08BND_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_08BND_GCM <- read_csv(here::here("data_raw/GCM", "GCM_Bundegi_with_mmm_and_dhw.csv")) %>% 
  select(Date, sst) %>% 
  rename(date = Date) %>% 
  mutate(data_type = "GCM", .before = date)

Ningaloo_08BND_comb <- bind_rows(Ningaloo_08BND_CCI, Ningaloo_08BND_Logger, Ningaloo_08BND_NOAA,
                                 Ningaloo_08BND_GCM, Ningaloo_08BND_CCore_CCI, Ningaloo_08BND_CCore_NOAA)

Plot_1_08BND <- Ningaloo_08BND_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  xlim(as.Date(c("1980-01-01", "2021-01-01"))) +
  geom_smooth(aes(color = data_type))

Plot_2_08BND <- Ningaloo_08BND_CCore %>% 
  ggplot(aes(x=date, y = `Sr/Ca`)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type))

Plot_1_08BND + Plot_2_08BND  #using Patchwork to bring plots together


##Ningaloo Reef##
##TNT07C
Ningaloo_TNT07C_CCI <- read_csv(here::here("data_raw", "CCI_TNT07C.csv"))
Ningaloo_TNT07C_CCI <- Ningaloo_TNT07C_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_TNT07C_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_TNT07C_CCore <- read_csv(here::here("data_raw", "CCore_Ningaloo_TNT07C_year.csv"))
Ningaloo_TNT07C_CCore <- Ningaloo_TNT07C_CCore %>% 
  mutate(data_type = "Coral Core") %>%  #prepare data for binding with other data types
  mutate(month = "01") %>% #add month column
  mutate(date = str_c(year, month, sep = "-")) %>% #join year-month columns
  mutate(date = ym(date)) %>%  #convert chr to date format
  select(data_type, date, `Tantabiddi Sr/Ca`)


Ningaloo_TNT07C_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_TANTABIDDI_SL1.csv"))
Ningaloo_TNT07C_Logger <- Ningaloo_TNT07C_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(subsite, data_type, date, sst)  #select only useful columns

Ningaloo_TNT07C_NOAA <- read_csv(here::here("data_raw", "NOAA_TNT07C_SST.csv"))
Ningaloo_TNT07C_NOAA <- Ningaloo_TNT07C_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_TNT07C_comb <- bind_rows(Ningaloo_TNT07C_CCI, Ningaloo_TNT07C_CCore, Ningaloo_TNT07C_Logger, Ningaloo_TNT07C_NOAA)

Plot_1_TNT07C <- Ningaloo_TNT07C_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type)) +
  xlim(as.Date(c("1980-01-01", "2021-01-01")))

Plot_2_TNT07C <- Ningaloo_TNT07C_CCore %>% 
  ggplot(aes(x=date, y = `Tantabiddi Sr/Ca`)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type))

Plot_1_TNT07C + Plot_2_TNT07C  #using Patchwork to bring plots together


##Ningaloo Reef##
##BUN05A
Ningaloo_BUN05A_CCI <- read_csv(here::here("data_raw", "CCI_BUN05A.csv"))
Ningaloo_BUN05A_CCI <- Ningaloo_BUN05A_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_BUN05A_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_BUN05A_CCore <- read_csv(here::here("data_raw", "CCore_Ningaloo_BUN05A_year.csv"))
Ningaloo_BUN05A_CCore <- Ningaloo_BUN05A_CCore %>% 
  mutate(data_type = "Coral Core") %>%  #prepare data for binding with other data types
  mutate(month = "01") %>% #add month column
  mutate(date = str_c(year, month, sep = "-")) %>% #join year-month columns
  mutate(date = ym(date)) %>%  #convert chr to date format
  select(data_type, date, `Bundegi Sr/Ca`)


Ningaloo_BUN05A_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_BUNDEGI_BR.csv"))
Ningaloo_BUN05A_Logger <- Ningaloo_BUN05A_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(subsite, data_type, date, sst)  #select only useful columns

Ningaloo_BUN05A_NOAA <- read_csv(here::here("data_raw", "NOAA_BUN05A_SST.csv"))
Ningaloo_BUN05A_NOAA <- Ningaloo_BUN05A_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_BUN05A_comb <- bind_rows(Ningaloo_BUN05A_CCI, Ningaloo_BUN05A_CCore, Ningaloo_BUN05A_Logger, Ningaloo_BUN05A_NOAA)

Plot_1_BUN05A <- Ningaloo_BUN05A_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type)) +
  xlim(as.Date(c("1980-01-01", "2021-01-01")))

Plot_2_BUN05A <- Ningaloo_BUN05A_CCore %>% 
  ggplot(aes(x=date, y = `Bundegi Sr/Ca`)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type))

Plot_1_BUN05A + Plot_2_BUN05A  #using Patchwork to bring plots together


##Ningaloo Reef##
##TNT
Ningaloo_TNT_CCI <- read_csv(here::here("data_raw", "CCI_TNT.csv"))
Ningaloo_TNT_CCI <- Ningaloo_TNT_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_TNT_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_TNT_CCore <- read_csv(here::here("data_raw", "CCore_Ningaloo_TNT_Year_Month.csv"))
Ningaloo_TNT_CCore <- Ningaloo_TNT_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = ym(date)) %>% #converting date column fr chr to date format
  select(data_type, date, d18O)  #select only useful columns

Ningaloo_TNT_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_TANDFL1.csv"))
Ningaloo_TNT_Logger <- Ningaloo_TNT_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(subsite, data_type, date, sst)  #select only useful columns

Ningaloo_TNT_NOAA <- read_csv(here::here("data_raw", "NOAA_TNT_SST.csv"))
Ningaloo_TNT_NOAA <- Ningaloo_TNT_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_TNT_comb <- bind_rows(Ningaloo_TNT_CCI, Ningaloo_TNT_CCore, Ningaloo_TNT_Logger, Ningaloo_TNT_NOAA)

Plot_1_TNT <- Ningaloo_TNT_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type)) +
  xlim(as.Date(c("1980-01-01", "2021-01-01")))

Plot_2_TNT <- Ningaloo_TNT_CCore %>% 
  ggplot(aes(x=date, y = d18O)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type))

Plot_1_TNT + Plot_2_TNT  #using Patchwork to bring plots together


##Houtman Abrolhos##
##Wallabi Island
HAbrol_Wallabi_CCI <- read_csv(here::here("data_raw", "CCI_Wallabi_Island.csv"))
HAbrol_Wallabi_CCI <- HAbrol_Wallabi_CCI %>% 
  mutate(sst = as.numeric(str_sub(HAbrol_Wallabi_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

HAbrol_Wallabi_CCore <- read_csv(here::here("data_raw", "CCore_H_Abrol_Wallabi_Year_Month.csv"))
HAbrol_Wallabi_CCore <- HAbrol_Wallabi_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = ym(date)) %>% #converting date column fr chr to date format
  select(data_type, date, d18O)  #select only useful columns

HAbrol_Wallabi_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_NTHFISHFL1.csv"))
HAbrol_Wallabi_Logger <- HAbrol_Wallabi_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(subsite, data_type, date, sst)  #select only useful columns

HAbrol_Wallabi_NOAA <- read_csv(here::here("data_raw", "NOAA_Wallabi_Island_SST.csv"))
HAbrol_Wallabi_NOAA <- HAbrol_Wallabi_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

HAbrol_Wallabi_comb <- bind_rows(HAbrol_Wallabi_CCI, HAbrol_Wallabi_CCore, HAbrol_Wallabi_Logger, HAbrol_Wallabi_NOAA)

Plot_1_Wallabi <- HAbrol_Wallabi_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type)) +
  xlim(as.Date(c("1980-01-01", "2021-01-01")))

Plot_2_Wallabi <- HAbrol_Wallabi_CCore %>% 
  ggplot(aes(x=date, y = d18O)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type))

Plot_1_Wallabi + Plot_2_Wallabi  #using Patchwork to bring plots together


##Houtman Abrolhos##
#HAB10A d18O
HAbrol_HAB10A_d18O_CCI <- read_csv(here::here("data_raw", "CCI_HAB10A.csv"))
HAbrol_HAB10A_d18O_CCI <- HAbrol_HAB10A_d18O_CCI %>% 
  mutate(sst = as.numeric(str_sub(HAbrol_HAB10A_d18O_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

HAbrol_HAB10A_d18O_CCore <- read_csv(here::here("data_raw", "CCore_H_Abrol_HAB_year.csv"))
HAbrol_HAB10A_d18O_CCore <- HAbrol_HAB10A_d18O_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(month = "01") %>% #add month column
  mutate(date = str_c(year, month, sep = "-")) %>% #join year-month columns
  mutate(date = ym(date)) %>%  #convert chr to date format
  select(data_type, date, `HAB10A d18O`)  #select only useful columns

HAbrol_HAB10A_d18O_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_NTHFISHFL1.csv"))
HAbrol_HAB10A_d18O_Logger <- HAbrol_HAB10A_d18O_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(subsite, data_type, date, sst)  #select only useful columns

HAbrol_HAB10A_d18O_NOAA <- read_csv(here::here("data_raw", "NOAA_HAB10A_d18O_Proxy_SST.csv"))
HAbrol_HAB10A_d18O_NOAA <- HAbrol_HAB10A_d18O_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

HAbrol_HAB10A_d18O_comb <- bind_rows(HAbrol_HAB10A_d18O_CCI, HAbrol_HAB10A_d18O_CCore, HAbrol_HAB10A_d18O_Logger, HAbrol_HAB10A_d18O_NOAA)

Plot_1_HAB10A_d18O <- HAbrol_HAB10A_d18O_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type)) +
  xlim(as.Date(c("1980-01-01", "2021-01-01")))

Plot_2_HAB10A_d18O <- HAbrol_HAB10A_d18O_CCore %>% 
  ggplot(aes(x=date, y = `HAB10A d18O`)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type)) +
  xlim(as.Date(c("1845-01-01", "2010-01-01")))

Plot_1_HAB10A_d18O + Plot_2_HAB10A_d18O  #using Patchwork to bring plots together



##Houtman Abrolhos##
#HAB10A Sr/Ca
HAbrol_HAB10A_SrCa_CCI <- read_csv(here::here("data_raw", "CCI_HAB10A.csv"))
HAbrol_HAB10A_SrCa_CCI <- HAbrol_HAB10A_SrCa_CCI %>% 
  mutate(sst = as.numeric(str_sub(HAbrol_HAB10A_SrCa_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

HAbrol_HAB10A_SrCa_CCore <- read_csv(here::here("data_raw", "CCore_H_Abrol_HAB_year.csv"))
HAbrol_HAB10A_SrCa_CCore <- HAbrol_HAB10A_SrCa_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(month = "01") %>% #add month column
  mutate(date = str_c(year, month, sep = "-")) %>% #join year-month columns
  mutate(date = ym(date)) %>%  #convert chr to date format
  select(data_type, date, `HAB10A Sr/Ca`)  #select only useful columns

HAbrol_HAB10A_SrCa_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_NTHFISHFL1.csv"))
HAbrol_HAB10A_SrCa_Logger <- HAbrol_HAB10A_SrCa_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(subsite, data_type, date, sst)  #select only useful columns

HAbrol_HAB10A_SrCa_NOAA <- read_csv(here::here("data_raw", "NOAA_HAB10A_SrCa_Proxy_SST.csv"))
HAbrol_HAB10A_SrCa_NOAA <- HAbrol_HAB10A_SrCa_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

HAbrol_HAB10A_GCM <- read_csv(here::here("data_raw/GCM", "GCM_HAB10A_with_mmm_and_dhw.csv")) %>% 
  select(Date, sst) %>% 
  rename(date = Date) %>% 
  mutate(data_type = "GCM", .before = date)

HAbrol_HAB10A_SrCa_comb <- bind_rows(HAbrol_HAB10A_SrCa_CCI, HAbrol_HAB10A_SrCa_Logger, HAbrol_HAB10A_SrCa_NOAA,
                                     HAbrol_HAB10A_GCM)

Plot_1_HAB10A_SrCa <- HAbrol_HAB10A_SrCa_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type)) +
  xlim(as.Date(c("1980-01-01", "2021-01-01")))

Plot_2_HAB10A_SrCa <- HAbrol_HAB10A_SrCa_CCore %>% 
  ggplot(aes(x=date, y = `HAB10A Sr/Ca`)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type)) +
  xlim(as.Date(c("1845-01-01", "2010-01-01")))

Plot_1_HAB10A_SrCa + Plot_2_HAB10A_SrCa  #using Patchwork to bring plots together


##Houtman Abrolhos##
#HAB05B d18O
HAbrol_HAB05B_d18O_CCI <- read_csv(here::here("data_raw", "CCI_HAB05B.csv"))
HAbrol_HAB05B_d18O_CCI <- HAbrol_HAB05B_d18O_CCI %>% 
  mutate(sst = as.numeric(str_sub(HAbrol_HAB05B_d18O_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

HAbrol_HAB05B_d18O_CCore <- read_csv(here::here("data_raw", "CCore_H_Abrol_HAB_year.csv"))
HAbrol_HAB05B_d18O_CCore <- HAbrol_HAB05B_d18O_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(month = "01") %>% #add month column
  mutate(date = str_c(year, month, sep = "-")) %>% #join year-month columns
  mutate(date = ym(date)) %>%  #convert chr to date format
  select(data_type, date, `HAB05B d18O`)  #select only useful columns

HAbrol_HAB05B_d18O_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_NTHFISHFL1.csv"))
HAbrol_HAB05B_d18O_Logger <- HAbrol_HAB05B_d18O_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(subsite, data_type, date, sst)  #select only useful columns

HAbrol_HAB05B_d18O_NOAA <- read_csv(here::here("data_raw", "NOAA_HAB05B_d18O_Proxy_SST.csv"))
HAbrol_HAB05B_d18O_NOAA <- HAbrol_HAB05B_d18O_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

HAbrol_HAB05B_d18O_comb <- bind_rows(HAbrol_HAB05B_d18O_CCI, HAbrol_HAB05B_d18O_CCore, HAbrol_HAB05B_d18O_Logger, HAbrol_HAB05B_d18O_NOAA)

Plot_1_HAB05B_d18O <- HAbrol_HAB05B_d18O_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type)) +
  xlim(as.Date(c("1980-01-01", "2021-01-01")))

Plot_2_HAB05B_d18O <- HAbrol_HAB05B_d18O_CCore %>% 
  ggplot(aes(x=date, y = `HAB05B d18O`)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type))

Plot_1_HAB05B_d18O + Plot_2_HAB05B_d18O  #using Patchwork to bring plots together


##Houtman Abrolhos##
#HAB05B SrCa
HAbrol_HAB05B_SrCa_CCI <- read_csv(here::here("data_raw", "CCI_HAB05B.csv"))
HAbrol_HAB05B_SrCa_CCI <- HAbrol_HAB05B_SrCa_CCI %>% 
  mutate(sst = as.numeric(str_sub(HAbrol_HAB05B_SrCa_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

HAbrol_HAB05B_SrCa_CCore <- read_csv(here::here("data_raw", "CCore_H_Abrol_HAB_year.csv"))
HAbrol_HAB05B_SrCa_CCore <- HAbrol_HAB05B_SrCa_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(month = "01") %>% #add month column
  mutate(date = str_c(year, month, sep = "-")) %>% #join year-month columns
  mutate(date = ym(date)) %>%  #convert chr to date format
  select(data_type, date, `HAB05B Sr/Ca`)  #select only useful columns

HAbrol_HAB05B_CCore_CCI <- read_csv(here::here("data_raw", "CCore_SST_HAB05B_SrCa_CCI.csv")) %>% 
  mutate(data_type = "Coral Core CCI calibrated SST", .before = "date") %>% 
  select(data_type, date, sst)

HAbrol_HAB05B_SrCa_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_NTHFISHFL1.csv"))
HAbrol_HAB05B_SrCa_Logger <- HAbrol_HAB05B_SrCa_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(subsite, data_type, date, sst)  #select only useful columns

HAbrol_HAB05B_SrCa_NOAA <- read_csv(here::here("data_raw", "NOAA_HAB05B_SrCa_Proxy_SST.csv"))
HAbrol_HAB05B_SrCa_NOAA <- HAbrol_HAB05B_SrCa_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

HAbrol_HAB05B_GCM <- read_csv(here::here("data_raw/GCM", "GCM_HAB05B_with_mmm_and_dhw.csv")) %>% 
  select(Date, sst) %>% 
  rename(date = Date) %>% 
  mutate(data_type = "GCM", .before = date)

HAbrol_HAB05B_SrCa_comb <- bind_rows(HAbrol_HAB05B_SrCa_CCI, HAbrol_HAB05B_SrCa_Logger, HAbrol_HAB05B_SrCa_NOAA,
                                     HAbrol_HAB05B_GCM, HAbrol_HAB05B_CCore_CCI)

Plot_1_HAB05B_SrCa <- HAbrol_HAB05B_SrCa_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type)) +
  xlim(as.Date(c("1980-01-01", "2021-01-01")))

Plot_2_HAB05B_SrCa <- HAbrol_HAB05B_SrCa_CCore %>% 
  ggplot(aes(x=date, y = `HAB05B Sr/Ca`)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type))

Plot_1_HAB05B_SrCa + Plot_2_HAB05B_SrCa  #using Patchwork to bring plots together

##Scott Reef Loggers##
SCOTT_RPO_1_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_SCOTT_RPO_1.csv"))
SCOTT_RPO_1_Logger <- SCOTT_RPO_1_Logger %>% 
  rename(sst = mean_SST) %>% 
  mutate(data_type = "Logger") %>% 
  select(subsite, data_type, date, sst)  

SCOTT_SL4_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_SCOTT_SL4.csv"))
SCOTT_SL4_Logger <- SCOTT_SL4_Logger %>% 
  rename(sst = mean_SST) %>% 
  mutate(data_type = "Logger") %>%
  select(subsite, data_type, date, sst)  

SCOTT_SS3_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_SCOTT_SS3.csv"))
SCOTT_SS3_Logger <- SCOTT_SS3_Logger %>% 
  rename(sst = mean_SST) %>% 
  mutate(data_type = "Logger") %>%
  select(subsite, data_type, date, sst)  

SCOTTNR1_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_SCOTTNR1.csv"))
SCOTTNR1_Logger <- SCOTTNR1_Logger %>% 
  rename(sst = mean_SST) %>% 
  mutate(data_type = "Logger") %>%
  select(subsite, data_type, date, sst) 

SCOTTSL1_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_SCOTTSL1.csv"))
SCOTTSL1_Logger <- SCOTTSL1_Logger %>% 
  rename(sst = mean_SST) %>% 
  mutate(data_type = "Logger") %>%
  select(subsite, data_type, date, sst) 

SCOTTSL2_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_SCOTTSL2.csv"))
SCOTTSL2_Logger <- SCOTTSL2_Logger %>% 
  rename(sst = mean_SST) %>% 
  mutate(data_type = "Logger") %>%
  select(subsite, data_type, date, sst) 

SCOTTSL3_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_SCOTTSL3.csv"))
SCOTTSL3_Logger <- SCOTTSL3_Logger %>% 
  rename(sst = mean_SST) %>% 
  mutate(data_type = "Logger") %>%
  select(subsite, data_type, date, sst) 

SCOTTSS1_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_SCOTTSS1.csv"))
SCOTTSS1_Logger <- SCOTTSS1_Logger %>% 
  rename(sst = mean_SST) %>% 
  mutate(data_type = "Logger") %>%
  select(subsite, data_type, date, sst) 

SCOTTSS2_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_SCOTTSS2.csv"))
SCOTTSS2_Logger <- SCOTTSS2_Logger %>% 
  rename(sst = mean_SST) %>% 
  mutate(data_type = "Logger") %>%
  select(subsite, data_type, date, sst) 

##Other Data Loggers##
`100THSITE_Logger` <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_100THSITE.csv"))
`100THSITE_Logger` <- `100THSITE_Logger` %>% 
  rename(sst = mean_SST) %>% 
  mutate(data_type = "Logger") %>%
  select(subsite, data_type, date, sst) 

BUNDEGI_BR_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_BUNDEGI_BR.csv"))
BUNDEGI_BR_Logger <- BUNDEGI_BR_Logger %>% 
  rename(sst = mean_SST) %>% 
  mutate(data_type = "Logger") %>%
  select(subsite, data_type, date, sst) 

BUNDFL1_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_BUNDFL1.csv"))
BUNDFL1_Logger <- BUNDFL1_Logger %>% 
  rename(sst = mean_SST) %>% 
  mutate(data_type = "Logger") %>%
  select(subsite, data_type, date, sst) 

NTHFISHFL1_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_NTHFISHFL1.csv"))
NTHFISHFL1_Logger <- NTHFISHFL1_Logger %>% 
  rename(sst = mean_SST) %>% 
  mutate(data_type = "Logger") %>%
  select(subsite, data_type, date, sst) 

TANDFL1_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_TANDFL1.csv"))
TANDFL1_Logger <- TANDFL1_Logger %>% 
  rename(sst = mean_SST) %>% 
  mutate(data_type = "Logger") %>%
  select(subsite, data_type, date, sst) 

TANTABIDDI_SL1_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_TANTABIDDI_SL1.csv"))
TANTABIDDI_SL1_Logger <- TANTABIDDI_SL1_Logger %>% 
  rename(sst = mean_SST) %>% 
  mutate(data_type = "Logger") %>%
  select(subsite, data_type, date, sst) 

#Combine all data loggers 
Logger_COMBINED <- rbind(SCOTT_RPO_1_Logger, SCOTT_SL4_Logger, SCOTT_SS3_Logger, 
                         SCOTTNR1_Logger, SCOTTSL1_Logger, SCOTTSL2_Logger, SCOTTSL3_Logger, 
                         SCOTTSS1_Logger, SCOTTSS2_Logger, SCOTT_SS3_Logger, `100THSITE_Logger`, 
                         BUNDEGI_BR_Logger, BUNDFL1_Logger, NTHFISHFL1_Logger, TANDFL1_Logger, TANTABIDDI_SL1_Logger)

cols = c("SCOTT_RPO_1" = "black", "SCOTT_SL4" = "black", "SCOTT_SS3" = "black", 
         "SCOTTNR1" = "black", "SCOTTSL1" = "black", "SCOTTSL2" = "black", 
         "SCOTTSL3" = "black", "SCOTTSS1" = "black", "SCOTTSS2" = "black", "SCOTT_SS3" = "black", 
         "100THSITE" = "dark blue", "BUNDEGI_BR" = "sky blue", "BUNDFL1" = "brown",
         "NTHFISHFL1" = "dark green", "TANDFL1" = "light green", "TANTABIDDI_SL1" = "red")

ggplot(Logger_COMBINED) +
  geom_line(aes(x = date, y = sst, color = subsite), position = "jitter") +
  scale_color_manual(values = cols) +
  facet_wrap(~subsite)

ggsave("graphics/TS/logger.jpeg", dpi = 300)

##Scott Reef##
#SCOTT_RPO_1#
SCOTT_RPO_1_GCM <- read_csv(here::here("data_raw/GCM", "GCM_SCOTT_RPO_1_with_mmm_and_dhw.csv")) %>% 
  select(Date, sst) %>% 
  rename(date = Date) %>% 
  mutate(data_type = "GCM", .before = date)

SCOTT_RPO_1_CCI <- read_csv(here::here("data_raw", "CCI_SCOTT_RPO_1_with_mmm_and_dhw.csv")) %>% 
  select(Date, sst) %>% 
  rename(date = Date) %>% 
  mutate(data_type = "CCI", .before = date)

SCOTT_RPO_1_NOAA <- read_csv(here::here("data_raw", "NOAA_SCOTT_RPO_1_with_mmm_and_dhw.csv")) %>% 
  select(Date, sst) %>% 
  rename(date = Date) %>% 
  mutate(data_type = "NOAA", .before = date)

SCOTT_RPO_1_Logger <- SCOTT_RPO_1_Logger %>% 
  select(data_type, date, sst)

SCOTT_RPO_1_comb <- rbind(SCOTT_RPO_1_CCI, SCOTT_RPO_1_Logger, SCOTT_RPO_1_NOAA, SCOTT_RPO_1_GCM)

Plot_1_SCOTT_RPO_1 <- SCOTT_RPO_1_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type)) +
  xlim(as.Date(c("1980-01-01", "2021-01-01")))

#SCOTTSL1#
SCOTTSL1_GCM <- read_csv(here::here("data_raw/GCM", "GCM_SCOTTSL1_with_mmm_and_dhw.csv")) %>% 
  select(Date, sst) %>% 
  rename(date = Date) %>% 
  mutate(data_type = "GCM", .before = date)

SCOTTSL1_CCI <- read_csv(here::here("data_raw", "CCI_SCOTTSL1_with_mmm_and_dhw.csv")) %>% 
  select(Date, sst) %>% 
  rename(date = Date) %>% 
  mutate(data_type = "CCI", .before = date)

SCOTTSL1_NOAA <- read_csv(here::here("data_raw", "NOAA_SCOTTSL1_with_mmm_and_dhw.csv")) %>% 
  select(Date, sst) %>% 
  rename(date = Date) %>% 
  mutate(data_type = "NOAA", .before = date)

SCOTTSL1_Logger <- SCOTTSL1_Logger %>% 
  select(data_type, date, sst)

SCOTTSL1_comb <- rbind(SCOTTSL1_CCI, SCOTTSL1_Logger, SCOTTSL1_NOAA, SCOTTSL1_GCM)

Plot_1_SCOTT_SL1 <- SCOTTSL1_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type)) +
  xlim(as.Date(c("1980-01-01", "2021-01-01")))

#Combined time series satellites (CCI and NOAA), loggers, coral cores, GCM

setcols <- c("CCI" = "coral", "NOAA" = "purple", "Logger" = "blue", "GCM" = "black",
             "Coral Core NOAA calibrated SST" = "green", "Coral Core CCI calibrated SST" = "brown")
TS_BRS07 <- Browse_07_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  scale_color_manual(values = setcols) +
  xlim(as.Date(c("1980-01-01", "2021-01-01"))) +
  theme(legend.title = element_blank()) +
  labs(title = "Browse Island BRS07 (-14.12, 123.55)")

TS_BRS07

ggsave("graphics/TS/BRS07.jpeg", dpi = 300)

TS_DARL <- Cocos_DARL_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  scale_color_manual(values = setcols) +
  xlim(as.Date(c("1980-01-01", "2021-01-01"))) +
  theme(legend.title = element_blank()) +
  labs(title = "Cocos Keeling Island DARL (-12.09, 96.88)")

TS_DARL

ggsave("graphics/TS/DARL.jpeg", dpi = 300)

TS_SCOTT_RPO_1 <- SCOTT_RPO_1_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  scale_color_manual(values = setcols) +
  xlim(as.Date(c("1980-01-01", "2021-01-01"))) +
  theme(legend.title = element_blank()) +
  labs(title = "Scott Reef SCOTT_RPO_1 (-14.06, 121.78)")

TS_SCOTT_RPO_1

ggsave("graphics/TS/SCOTT_RPO_1.jpeg", dpi = 300)

TS_SCOTTSL1 <- SCOTTSL1_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  scale_color_manual(values = setcols) +
  xlim(as.Date(c("1980-01-01", "2021-01-01"))) +
  theme(legend.title = element_blank()) +
  labs(title = "Scott Reef SCOTTSL1 (-14.08, 121.95)")

TS_SCOTTSL1

ggsave("graphics/TS/SCOTTSL1.jpeg", dpi = 300)

TS_Ningaloo_08BND <- Ningaloo_08BND_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  scale_color_manual(values = setcols) +
  xlim(as.Date(c("1980-01-01", "2021-01-01"))) +
  theme(legend.title = element_blank()) +
  labs(title = "Ningaloo Reef 08BND (-21.87, 114.16)")

TS_Ningaloo_08BND

ggsave("graphics/TS/Ningaloo_08BND.jpeg", dpi = 300)

TS_Ningaloo_13BND <- Ningaloo_13BND_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  scale_color_manual(values = setcols) +
  xlim(as.Date(c("1980-01-01", "2021-01-01"))) +
  theme(legend.title = element_blank()) +
  labs(title = "Ningaloo Reef 13BND (-21.87, 114.16)")

TS_Ningaloo_13BND

ggsave("graphics/TS/Ningaloo_13BND.jpeg", dpi = 300)

TS_Ningaloo_08TNT <- Ningaloo_08TNT_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  scale_color_manual(values = setcols) +
  xlim(as.Date(c("1980-01-01", "2021-01-01"))) +
  theme(legend.title = element_blank()) +
  labs(title = "Ningaloo Reef 08TNT (-21.91, 113.97)")

TS_Ningaloo_08TNT

ggsave("graphics/TS/Ningaloo_08TNT.jpeg", dpi = 300)

TS_Ningaloo_13TNT <- Ningaloo_13TNT_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  scale_color_manual(values = setcols) +
  xlim(as.Date(c("1980-01-01", "2021-01-01"))) +
  theme(legend.title = element_blank()) +
  labs(title = "Ningaloo Reef 13TNT (-21.91, 113.97)")

TS_Ningaloo_13TNT

ggsave("graphics/TS/Ningaloo_13TNT.jpeg", dpi = 300)

TS_HAbrol_HAB10A <- HAbrol_HAB10A_SrCa_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  scale_color_manual(values = setcols) +
  xlim(as.Date(c("1980-01-01", "2021-01-01"))) +
  theme(legend.title = element_blank()) +
  labs(title = "Houtman Abrolhos HAB10A (-28.46, 113.75)")

TS_HAbrol_HAB10A

ggsave("graphics/TS/HAbrol_HAB10A.jpeg", dpi = 300)

TS_HAbrol_HAB05B <- HAbrol_HAB05B_SrCa_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  scale_color_manual(values = setcols) +
  xlim(as.Date(c("1980-01-01", "2021-01-01"))) +
  theme(legend.title = element_blank()) +
  labs(title = "Houtman Abrolhos HAB05B (-28.46, 113.77)")

TS_HAbrol_HAB05B

ggsave("graphics/TS/HAbrol_HAB05B.jpeg", dpi = 300)

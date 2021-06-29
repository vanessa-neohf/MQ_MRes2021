#Data visualisation for each coral core site 
#(Scott Reef to be plotted later)
#Plotting NOAA, CCI, Coral Core, Data Logger time series

#load packages
library(tidyverse)
library(lubridate)
library(patchwork)

##Browse Island##
##BRS05
Browse_05_CCI <- read_csv("CCI_BRS05.csv")
Browse_05_CCI <- Browse_05_CCI %>% 
  mutate(sst = as.numeric(str_sub(Browse_05_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

Browse_05_CCore <- read_csv("CCore_BRS05_BRS07_Year_Month.csv")
Browse_05_CCore <- Browse_05_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = ym(date)) %>% #converting date column fr chr to date format
  select(data_type, date, `BRS05 Sr/Ca [mmol/mol]`)  #select only useful columns

Browse_05_Logger <- read_csv("Logger_Avg_Daily_SST_SCOTTSS1.csv")
Browse_05_Logger <- Browse_05_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst)  #select only useful columns

Browse_05_NOAA <- read_csv("NOAA_BRS05_SST.csv")
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
Browse_07_CCI <- read_csv("CCI_BRS07.csv")
Browse_07_CCI <- Browse_07_CCI %>% 
  mutate(sst = as.numeric(str_sub(Browse_07_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

Browse_07_CCore <- read_csv("CCore_BRS05_BRS07_Year_Month.csv")
Browse_07_CCore <- Browse_07_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = ym(date)) %>% #converting date column fr chr to date format
  select(data_type, date, `BRS07 Sr/Ca [mmol/mol]`)  #select only useful columns

Browse_07_Logger <- read_csv("Logger_Avg_Daily_SST_SCOTTSS1.csv")
Browse_07_Logger <- Browse_07_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst)  #select only useful columns

Browse_07_NOAA <- read_csv("NOAA_BRS07_SST.csv")
Browse_07_NOAA <- Browse_07_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

Browse_07_comb <- bind_rows(Browse_07_CCI, Browse_07_Logger, Browse_07_NOAA, Browse_07_CCore)

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
Cocos_DARL_CCI <- read_csv("CCI_DARL.csv")
Cocos_DARL_CCI <- Cocos_DARL_CCI %>% 
  mutate(sst = as.numeric(str_sub(Cocos_DARL_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

Cocos_DARL_CCore <- read_csv("CCore_DAR_Long_Year_Month.csv")
Cocos_DARL_CCore <- Cocos_DARL_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = ym(date)) %>% #converting date column fr chr to date format
  select(data_type, date, `Sr/Ca`)  #select only useful columns

Cocos_DARL_Logger <- read_csv("Logger_Avg_Daily_SST_100THSITE.csv")
Cocos_DARL_Logger <- Cocos_DARL_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst)  #select only useful columns

Cocos_DARL_NOAA <- read_csv("NOAA_DAR_Long_SST.csv")
Cocos_DARL_NOAA <- Cocos_DARL_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

Cocos_DARL_comb <- bind_rows(Cocos_DARL_CCI, Cocos_DARL_CCore, Cocos_DARL_Logger, Cocos_DARL_NOAA)

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
Cocos_DAR3_CCI <- read_csv("CCI_DAR3.csv")
Cocos_DAR3_CCI <- Cocos_DAR3_CCI %>% 
  mutate(sst = as.numeric(str_sub(Cocos_DAR3_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

Cocos_DAR3_CCore <- read_csv("CCore_DAR3_Year_Month.csv")
Cocos_DAR3_CCore <- Cocos_DAR3_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = ym(date)) %>% #converting date column fr chr to date format
  select(data_type, date, `Sr/Ca`)  #select only useful columns

Cocos_DAR3_Logger <- read_csv("Logger_Avg_Daily_SST_100THSITE.csv")
Cocos_DAR3_Logger <- Cocos_DAR3_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst)  #select only useful columns

Cocos_DAR3_NOAA <- read_csv("NOAA_DAR3_SST.csv")
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
Ningaloo_13TNT_CCI <- read_csv("CCI_Tantabiddi.csv")
Ningaloo_13TNT_CCI <- Ningaloo_13TNT_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_13TNT_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_13TNT_CCore <- read_csv("CCore_Ningaloo_13TNT_Year_Month.csv")
Ningaloo_13TNT_CCore <- Ningaloo_13TNT_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = ym(date)) %>% #converting date column fr chr to date format
  select(data_type, date, `Sr/Ca`)  #select only useful columns

Ningaloo_13TNT_Logger <- read_csv("Logger_Avg_Daily_SST_TANDFL1.csv")
Ningaloo_13TNT_Logger <- Ningaloo_13TNT_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst)  #select only useful columns

Ningaloo_13TNT_NOAA <- read_csv("NOAA_Tantabiddi_13TNT_SST.csv")
Ningaloo_13TNT_NOAA <- Ningaloo_13TNT_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_13TNT_comb <- bind_rows(Ningaloo_13TNT_CCI, Ningaloo_13TNT_CCore, Ningaloo_13TNT_Logger, Ningaloo_13TNT_NOAA)

Plot_1_13TNT <- Ningaloo_13TNT_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type))

Plot_2_13TNT <- Ningaloo_13TNT_CCore %>% 
  ggplot(aes(x=date, y = `Sr/Ca`)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type))

Plot_1_13TNT + Plot_2_13TNT  #using Patchwork to bring plots together


##Ningaloo Reef##
##Tantabiddi 08TNT
Ningaloo_08TNT_CCI <- read_csv("CCI_Tantabiddi.csv")
Ningaloo_08TNT_CCI <- Ningaloo_08TNT_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_08TNT_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_08TNT_CCore <- read_csv("CCore_Ningaloo_08TNT_Year_Month.csv")
Ningaloo_08TNT_CCore <- Ningaloo_08TNT_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = ym(date)) %>% #converting date column fr chr to date format
  select(data_type, date, `Sr/Ca`)  #select only useful columns

Ningaloo_08TNT_Logger <- read_csv("Logger_Avg_Daily_SST_TANDFL1.csv")
Ningaloo_08TNT_Logger <- Ningaloo_08TNT_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst)  #select only useful columns

Ningaloo_08TNT_NOAA <- read_csv("NOAA_Tantabiddi_08TNT_SST.csv")
Ningaloo_08TNT_NOAA <- Ningaloo_08TNT_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_08TNT_comb <- bind_rows(Ningaloo_08TNT_CCI, Ningaloo_08TNT_CCore, Ningaloo_08TNT_Logger, Ningaloo_08TNT_NOAA)

Plot_1_08TNT <- Ningaloo_08TNT_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type))

Plot_2_08TNT <- Ningaloo_08TNT_CCore %>% 
  ggplot(aes(x=date, y = `Sr/Ca`)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type))

Plot_1_08TNT + Plot_2_08TNT  #using Patchwork to bring plots together


##Ningaloo Reef##
##Bundegi 13BND
Ningaloo_13BND_CCI <- read_csv("CCI_Bundegi.csv")
Ningaloo_13BND_CCI <- Ningaloo_13BND_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_13BND_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_13BND_CCore <- read_csv("CCore_Ningaloo_13BND_Year_Month.csv")
Ningaloo_13BND_CCore <- Ningaloo_13BND_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = ym(date)) %>% #converting date column fr chr to date format
  select(data_type, date, `Sr/Ca`)  #select only useful columns

Ningaloo_13BND_Logger <- read_csv("Logger_Avg_Daily_SST_BUNDFL1.csv")
Ningaloo_13BND_Logger <- Ningaloo_13BND_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst)  #select only useful columns

Ningaloo_13BND_NOAA <- read_csv("NOAA_Bundegi_13BND_SST.csv")
Ningaloo_13BND_NOAA <- Ningaloo_13BND_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_13BND_comb <- bind_rows(Ningaloo_13BND_CCI, Ningaloo_13BND_CCore, Ningaloo_13BND_Logger, Ningaloo_13BND_NOAA)

Plot_1_13BND <- Ningaloo_13BND_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type))

Plot_2_13BND <- Ningaloo_13BND_CCore %>% 
  ggplot(aes(x=date, y = `Sr/Ca`)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type))

Plot_1_13BND + Plot_2_13BND  #using Patchwork to bring plots together


##Ningaloo Reef##
##Bundegi 08BND
Ningaloo_08BND_CCI <- read_csv("CCI_Bundegi.csv")
Ningaloo_08BND_CCI <- Ningaloo_08BND_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_08BND_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_08BND_CCore <- read_csv("CCore_Ningaloo_08BND_Year_Month.csv")
Ningaloo_08BND_CCore <- Ningaloo_08BND_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = ym(date)) %>% #converting date column fr chr to date format
  select(data_type, date, `Sr/Ca`)  #select only useful columns

Ningaloo_08BND_Logger <- read_csv("Logger_Avg_Daily_SST_BUNDFL1.csv")
Ningaloo_08BND_Logger <- Ningaloo_08BND_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst)  #select only useful columns

Ningaloo_08BND_NOAA <- read_csv("NOAA_Bundegi_08BND_SST.csv")
Ningaloo_08BND_NOAA <- Ningaloo_08BND_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_08BND_comb <- bind_rows(Ningaloo_08BND_CCI, Ningaloo_08BND_CCore, Ningaloo_08BND_Logger, Ningaloo_08BND_NOAA)

Plot_1_08BND <- Ningaloo_08BND_comb %>% 
  ggplot(aes(x = date, y = sst)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type))

Plot_2_08BND <- Ningaloo_08BND_CCore %>% 
  ggplot(aes(x=date, y = `Sr/Ca`)) +
  geom_line(aes(color = data_type)) +
  geom_smooth(aes(color = data_type))

Plot_1_08BND + Plot_2_08BND  #using Patchwork to bring plots together


##Ningaloo Reef##
##TNT07C
Ningaloo_TNT07C_CCI <- read_csv("CCI_TNT07C.csv")
Ningaloo_TNT07C_CCI <- Ningaloo_TNT07C_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_TNT07C_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_TNT07C_CCore <- read_csv("CCore_Ningaloo_TNT07C_year.csv")
Ningaloo_TNT07C_CCore <- Ningaloo_TNT07C_CCore %>% 
  mutate(data_type = "Coral Core") %>%  #prepare data for binding with other data types
  mutate(month = "01") %>% #add month column
  mutate(date = str_c(year, month, sep = "-")) %>% #join year-month columns
  mutate(date = ym(date)) %>%  #convert chr to date format
  select(data_type, date, `Tantabiddi Sr/Ca`)


Ningaloo_TNT07C_Logger <- read_csv("Logger_Avg_Daily_SST_TANTABIDDI_SL1.csv")
Ningaloo_TNT07C_Logger <- Ningaloo_TNT07C_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst)  #select only useful columns

Ningaloo_TNT07C_NOAA <- read_csv("NOAA_TNT07C_SST.csv")
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
Ningaloo_BUN05A_CCI <- read_csv("CCI_BUN05A.csv")
Ningaloo_BUN05A_CCI <- Ningaloo_BUN05A_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_BUN05A_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_BUN05A_CCore <- read_csv("CCore_Ningaloo_BUN05A_year.csv")
Ningaloo_BUN05A_CCore <- Ningaloo_BUN05A_CCore %>% 
  mutate(data_type = "Coral Core") %>%  #prepare data for binding with other data types
  mutate(month = "01") %>% #add month column
  mutate(date = str_c(year, month, sep = "-")) %>% #join year-month columns
  mutate(date = ym(date)) %>%  #convert chr to date format
  select(data_type, date, `Bundegi Sr/Ca`)


Ningaloo_BUN05A_Logger <- read_csv("Logger_Avg_Daily_SST_BUNDEGI_BR.csv")
Ningaloo_BUN05A_Logger <- Ningaloo_BUN05A_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst)  #select only useful columns

Ningaloo_BUN05A_NOAA <- read_csv("NOAA_BUN05A_SST.csv")
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
Ningaloo_TNT_CCI <- read_csv("CCI_TNT.csv")
Ningaloo_TNT_CCI <- Ningaloo_TNT_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_TNT_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

Ningaloo_TNT_CCore <- read_csv("CCore_Ningaloo_TNT_Year_Month.csv")
Ningaloo_TNT_CCore <- Ningaloo_TNT_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = ym(date)) %>% #converting date column fr chr to date format
  select(data_type, date, d18O)  #select only useful columns

Ningaloo_TNT_Logger <- read_csv("Logger_Avg_Daily_SST_TANDFL1.csv")
Ningaloo_TNT_Logger <- Ningaloo_TNT_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst)  #select only useful columns

Ningaloo_TNT_NOAA <- read_csv("NOAA_TNT_SST.csv")
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
HAbrol_Wallabi_CCI <- read_csv("CCI_Wallabi_Island.csv")
HAbrol_Wallabi_CCI <- HAbrol_Wallabi_CCI %>% 
  mutate(sst = as.numeric(str_sub(HAbrol_Wallabi_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

HAbrol_Wallabi_CCore <- read_csv("CCore_H_Abrol_Wallabi_Year_Month.csv")
HAbrol_Wallabi_CCore <- HAbrol_Wallabi_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = ym(date)) %>% #converting date column fr chr to date format
  select(data_type, date, d18O)  #select only useful columns

HAbrol_Wallabi_Logger <- read_csv("Logger_Avg_Daily_SST_NTHFISHFL1.csv")
HAbrol_Wallabi_Logger <- HAbrol_Wallabi_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst)  #select only useful columns

HAbrol_Wallabi_NOAA <- read_csv("NOAA_Wallabi_Island_SST.csv")
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
HAbrol_HAB10A_d18O_CCI <- read_csv("CCI_HAB10A.csv")
HAbrol_HAB10A_d18O_CCI <- HAbrol_HAB10A_d18O_CCI %>% 
  mutate(sst = as.numeric(str_sub(HAbrol_HAB10A_d18O_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

HAbrol_HAB10A_d18O_CCore <- read_csv("CCore_H_Abrol_HAB_year.csv")
HAbrol_HAB10A_d18O_CCore <- HAbrol_HAB10A_d18O_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(month = "01") %>% #add month column
  mutate(date = str_c(year, month, sep = "-")) %>% #join year-month columns
  mutate(date = ym(date)) %>%  #convert chr to date format
  select(data_type, date, `HAB10A d18O`)  #select only useful columns

HAbrol_HAB10A_d18O_Logger <- read_csv("Logger_Avg_Daily_SST_NTHFISHFL1.csv")
HAbrol_HAB10A_d18O_Logger <- HAbrol_HAB10A_d18O_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst)  #select only useful columns

HAbrol_HAB10A_d18O_NOAA <- read_csv("NOAA_HAB10A_d18O_Proxy_SST.csv")
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
HAbrol_HAB10A_SrCa_CCI <- read_csv("CCI_HAB10A.csv")
HAbrol_HAB10A_SrCa_CCI <- HAbrol_HAB10A_SrCa_CCI %>% 
  mutate(sst = as.numeric(str_sub(HAbrol_HAB10A_SrCa_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

HAbrol_HAB10A_SrCa_CCore <- read_csv("CCore_H_Abrol_HAB_year.csv")
HAbrol_HAB10A_SrCa_CCore <- HAbrol_HAB10A_SrCa_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(month = "01") %>% #add month column
  mutate(date = str_c(year, month, sep = "-")) %>% #join year-month columns
  mutate(date = ym(date)) %>%  #convert chr to date format
  select(data_type, date, `HAB10A Sr/Ca`)  #select only useful columns

HAbrol_HAB10A_SrCa_Logger <- read_csv("Logger_Avg_Daily_SST_NTHFISHFL1.csv")
HAbrol_HAB10A_SrCa_Logger <- HAbrol_HAB10A_SrCa_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst)  #select only useful columns

HAbrol_HAB10A_SrCa_NOAA <- read_csv("NOAA_HAB10A_SrCa_Proxy_SST.csv")
HAbrol_HAB10A_SrCa_NOAA <- HAbrol_HAB10A_SrCa_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

HAbrol_HAB10A_SrCa_comb <- bind_rows(HAbrol_HAB10A_SrCa_CCI, HAbrol_HAB10A_SrCa_CCore, HAbrol_HAB10A_SrCa_Logger, HAbrol_HAB10A_SrCa_NOAA)

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
HAbrol_HAB05B_d18O_CCI <- read_csv("CCI_HAB05B.csv")
HAbrol_HAB05B_d18O_CCI <- HAbrol_HAB05B_d18O_CCI %>% 
  mutate(sst = as.numeric(str_sub(HAbrol_HAB05B_d18O_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

HAbrol_HAB05B_d18O_CCore <- read_csv("CCore_H_Abrol_HAB_year.csv")
HAbrol_HAB05B_d18O_CCore <- HAbrol_HAB05B_d18O_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(month = "01") %>% #add month column
  mutate(date = str_c(year, month, sep = "-")) %>% #join year-month columns
  mutate(date = ym(date)) %>%  #convert chr to date format
  select(data_type, date, `HAB05B d18O`)  #select only useful columns

HAbrol_HAB05B_d18O_Logger <- read_csv("Logger_Avg_Daily_SST_NTHFISHFL1.csv")
HAbrol_HAB05B_d18O_Logger <- HAbrol_HAB05B_d18O_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst)  #select only useful columns

HAbrol_HAB05B_d18O_NOAA <- read_csv("NOAA_HAB05B_d18O_Proxy_SST.csv")
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
HAbrol_HAB05B_SrCa_CCI <- read_csv("CCI_HAB05B.csv")
HAbrol_HAB05B_SrCa_CCI <- HAbrol_HAB05B_SrCa_CCI %>% 
  mutate(sst = as.numeric(str_sub(HAbrol_HAB05B_SrCa_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) #select only useful columns

HAbrol_HAB05B_SrCa_CCore <- read_csv("CCore_H_Abrol_HAB_year.csv")
HAbrol_HAB05B_SrCa_CCore <- HAbrol_HAB05B_SrCa_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(month = "01") %>% #add month column
  mutate(date = str_c(year, month, sep = "-")) %>% #join year-month columns
  mutate(date = ym(date)) %>%  #convert chr to date format
  select(data_type, date, `HAB05B Sr/Ca`)  #select only useful columns

HAbrol_HAB05B_SrCa_Logger <- read_csv("Logger_Avg_Daily_SST_NTHFISHFL1.csv")
HAbrol_HAB05B_SrCa_Logger <- HAbrol_HAB05B_SrCa_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst)  #select only useful columns

HAbrol_HAB05B_SrCa_NOAA <- read_csv("NOAA_HAB05B_SrCa_Proxy_SST.csv")
HAbrol_HAB05B_SrCa_NOAA <- HAbrol_HAB05B_SrCa_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) #select only useful columns

HAbrol_HAB05B_SrCa_comb <- bind_rows(HAbrol_HAB05B_SrCa_CCI, HAbrol_HAB05B_SrCa_CCore, HAbrol_HAB05B_SrCa_Logger, HAbrol_HAB05B_SrCa_NOAA)

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

##Scott Reef##
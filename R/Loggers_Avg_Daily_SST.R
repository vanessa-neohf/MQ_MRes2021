library(tidyverse)
library(lubridate)

#average daily SST for nearest data loggers to coral cores


#read csv files of logger sites
#SCOTT_RPO_1, SCOTT_SL4, SCOTT_SS3, SCOTTNR1, SCOTTSL1, SCOTTSL2, SCOTTSL3, SCOTTSS1, SCOTTSS2, 100THSITE, TANDFL1, BUNDFL1, TANTABIDDI_SL1, BUNDEGI_BR, NTHFISHFL1

Log_SCOTT_RPO_1 <- read_csv("SCOTT_RPO_1.csv",
                         col_types = cols(uncal_val = col_character()))

Log_SCOTT_SL4 <- read_csv("SCOTT_SL4.csv",
                            col_types = cols(uncal_val = col_character()))

Log_SCOTT_SS3 <- read_csv("SCOTT_SS3.csv",
                         col_types = cols(uncal_val = col_character()))

Log_SCOTTNR1 <- read_csv("SCOTTNR1.csv",
                         col_types = cols(uncal_val = col_character()))

Log_SCOTTSL1 <- read_csv("SCOTTSL1.csv",
                         col_types = cols(uncal_val = col_character()))

Log_SCOTTSL2 <- read_csv("SCOTTSL3.csv",
                         col_types = cols(uncal_val = col_character()))

Log_SCOTTSL3 <- read_csv("SCOTTSL3.csv",
                         col_types = cols(uncal_val = col_character()))

Log_SCOTTSS1 <- read_csv("SCOTTSS1.csv",
                         col_types = cols(uncal_val = col_character()))

Log_SCOTTSS2 <- read_csv("SCOTTSS2.csv",
                         col_types = cols(uncal_val = col_character()))

Log_100THSITE <- read_csv("100THSITE.csv",
                          col_types = cols(uncal_val = col_character()))

Log_TANDFL1 <- read_csv("TANDFL1.csv",
                          col_types = cols(uncal_val = col_character()))

Log_BUNDFL1 <- read_csv("BUNDFL1.csv",
                          col_types = cols(uncal_val = col_character()))

Log_TANTABIDDI_SL1 <- read_csv("TANTABIDDI_SL1.csv",
                          col_types = cols(uncal_val = col_character()))

Log_BUNDEGI_BR <- read_csv("BUNDEGI_BR.csv",
                          col_types = cols(uncal_val = col_character()))

Log_NTHFISHFL1 <- read_csv("NTHFISHFL1.csv",
                          col_types = cols(uncal_val = col_character()))

#extract daily dates from 'time' variable
#group_by daily dates
#summarize average daily SST 
#create summary table

tbl_SCOTT_RPO_1 <- Log_SCOTT_RPO_1 %>% 
  mutate(date = format(as.Date(time))) %>% 
  group_by(date) %>% 
  summarize(mean_SST = mean(qc_val, na.rm = TRUE))

tbl_SCOTT_SL4 <- Log_SCOTT_SL4 %>% 
  mutate(date = format(as.Date(time))) %>% 
  group_by(date) %>% 
  summarize(mean_SST = mean(qc_val, na.rm = TRUE))

tbl_SCOTT_SS3 <- Log_SCOTT_SS3 %>% 
  mutate(date = format(as.Date(time))) %>% 
  group_by(date) %>% 
  summarize(mean_SST = mean(qc_val, na.rm = TRUE))

tbl_SCOTTNR1 <- Log_SCOTTNR1 %>% 
  mutate(date = format(as.Date(time))) %>% 
  group_by(date) %>% 
  summarize(mean_SST = mean(qc_val, na.rm = TRUE))

tbl_SCOTTSL1 <- Log_SCOTTSL1 %>% 
  mutate(date = format(as.Date(time))) %>% 
  group_by(date) %>% 
  summarize(mean_SST = mean(qc_val, na.rm = TRUE))

tbl_SCOTTSL2 <- Log_SCOTTSL2 %>% 
  mutate(date = format(as.Date(time))) %>% 
  group_by(date) %>% 
  summarize(mean_SST = mean(qc_val, na.rm = TRUE))

tbl_SCOTTSL3 <- Log_SCOTTSL3 %>% 
  mutate(date = format(as.Date(time))) %>% 
  group_by(date) %>% 
  summarize(mean_SST = mean(qc_val, na.rm = TRUE))

tbl_SCOTTSS2 <- Log_SCOTTSS2 %>% 
  mutate(date = format(as.Date(time))) %>% 
  group_by(date) %>% 
  summarize(mean_SST = mean(qc_val, na.rm = TRUE))

tbl_100TH <- Log_100THSITE %>% 
  mutate(date = format(as.Date(time))) %>% 
  group_by(date) %>% 
  summarize(mean_SST = mean(qc_val, na.rm = TRUE))

tbl_BUN_BR <- Log_BUNDEGI_BR %>% 
  mutate(date = format(as.Date(time))) %>% 
  group_by(date) %>% 
  summarize(mean_SST = mean(qc_val, na.rm = TRUE))

tbl_BUNDFL1 <- Log_BUNDFL1 %>% 
  mutate(date = format(as.Date(time))) %>% 
  group_by(date) %>% 
  summarize(mean_SST = mean(qc_val, na.rm = TRUE))

tbl_NTHFISHFL1 <- Log_NTHFISHFL1 %>% 
  mutate(date = format(as.Date(time))) %>% 
  group_by(date) %>% 
  summarize(mean_SST = mean(qc_val, na.rm = TRUE))

tbl_SCOTTSS1 <- Log_SCOTTSS1 %>% 
  mutate(date = format(as.Date(time))) %>% 
  group_by(date) %>% 
  summarize(mean_SST = mean(qc_val, na.rm = TRUE))

tbl_TANDFL1 <- Log_TANDFL1 %>% 
  mutate(date = format(as.Date(time))) %>% 
  group_by(date) %>% 
  summarize(mean_SST = mean(qc_val, na.rm = TRUE))

tbl_TANTABIDDI_SL1 <- Log_TANTABIDDI_SL1 %>% 
  mutate(date = format(as.Date(time))) %>% 
  group_by(date) %>% 
  summarize(mean_SST = mean(qc_val, na.rm = TRUE))

#add date column to logger files (for joining with summary table)
#remove duplicate date columns with distinct()
Log_SCOTT_RPO_1 <- Log_SCOTT_RPO_1 %>% 
  mutate(date = format(as.Date(time))) %>% 
  distinct(date, .keep_all = TRUE)

Log_SCOTT_SL4 <- Log_SCOTT_SL4 %>% 
  mutate(date = format(as.Date(time))) %>% 
  distinct(date, .keep_all = TRUE)

Log_SCOTT_SS3 <- Log_SCOTT_SS3 %>% 
  mutate(date = format(as.Date(time))) %>% 
  distinct(date, .keep_all = TRUE)

Log_SCOTTNR1 <- Log_SCOTTNR1 %>% 
  mutate(date = format(as.Date(time))) %>% 
  distinct(date, .keep_all = TRUE)

Log_SCOTTSL1 <- Log_SCOTTSL1 %>% 
  mutate(date = format(as.Date(time))) %>% 
  distinct(date, .keep_all = TRUE)

Log_SCOTTSL2 <- Log_SCOTTSL2 %>% 
  mutate(date = format(as.Date(time))) %>% 
  distinct(date, .keep_all = TRUE)

Log_SCOTTSL3 <- Log_SCOTTSL3 %>% 
  mutate(date = format(as.Date(time))) %>% 
  distinct(date, .keep_all = TRUE)

Log_SCOTTSS2 <- Log_SCOTTSS2 %>% 
  mutate(date = format(as.Date(time))) %>% 
  distinct(date, .keep_all = TRUE)

Log_100THSITE <- Log_100THSITE %>% 
  mutate(date = format(as.Date(time))) %>% 
  distinct(date, .keep_all = TRUE)

Log_BUNDEGI_BR <- Log_BUNDEGI_BR %>% 
  mutate(date = format(as.Date(time))) %>% 
  distinct(date, .keep_all = TRUE)

Log_BUNDFL1 <- Log_BUNDFL1 %>% 
  mutate(date = format(as.Date(time))) %>% 
  distinct(date, .keep_all = TRUE)

Log_NTHFISHFL1 <- Log_NTHFISHFL1 %>% 
  mutate(date = format(as.Date(time))) %>% 
  distinct(date, .keep_all = TRUE)

Log_SCOTTSS1 <- Log_SCOTTSS1 %>% 
  mutate(date = format(as.Date(time))) %>% 
  distinct(date, .keep_all = TRUE)

Log_TANDFL1 <- Log_TANDFL1 %>% 
  mutate(date = format(as.Date(time))) %>% 
  distinct(date, .keep_all = TRUE)

Log_TANTABIDDI_SL1 <- Log_TANTABIDDI_SL1 %>% 
  mutate(date = format(as.Date(time))) %>% 
  distinct(date, .keep_all = TRUE)

#join SST values in summary table with logger file
Log_SCOTT_RPO_1 <- left_join(Log_SCOTT_RPO_1, tbl_SCOTT_RPO_1, by = "date")
Log_SCOTT_SL4 <- left_join(Log_SCOTT_SL4, tbl_SCOTT_SL4, by = "date")
Log_SCOTT_SS3 <- left_join(Log_SCOTT_SS3, tbl_SCOTT_SS3, by = "date")
Log_SCOTTNR1 <- left_join(Log_SCOTTNR1, tbl_SCOTTNR1, by = "date")
Log_SCOTTSL1 <- left_join(Log_SCOTTSL1, tbl_SCOTTSL1, by = "date")
Log_SCOTTSL2 <- left_join(Log_SCOTTSL2, tbl_SCOTTSL2, by = "date")
Log_SCOTTSL3 <- left_join(Log_SCOTTSL3, tbl_SCOTTSL3, by = "date")
Log_SCOTTSS2 <- left_join(Log_SCOTTSS2, tbl_SCOTTSS2, by = "date")
Log_100THSITE <- left_join(Log_100THSITE, tbl_100TH, by = "date")
Log_BUNDEGI_BR <- left_join(Log_BUNDEGI_BR, tbl_BUN_BR, by = "date")
Log_BUNDFL1 <- left_join(Log_BUNDFL1, tbl_BUNDFL1, by = "date")
Log_NTHFISHFL1 <- left_join(Log_NTHFISHFL1, tbl_NTHFISHFL1, by = "date")
Log_SCOTTSS1 <- left_join(Log_SCOTTSS1, tbl_SCOTTSS1, by = "date")
Log_TANDFL1 <- left_join(Log_TANDFL1, tbl_TANDFL1, by = "date")
Log_TANTABIDDI_SL1 <- left_join(Log_TANTABIDDI_SL1, tbl_TANTABIDDI_SL1, by = "date")

#export csv
write_csv(Log_SCOTT_RPO_1, "E:/Logger_Data/Avg_Daily_SST_Loggers/Logger_Avg_Daily_SST_SCOTT_RPO_1.csv")
write_csv(Log_SCOTT_SL4, "E:/Logger_Data/Avg_Daily_SST_Loggers/Logger_Avg_Daily_SST_SCOTT_SL4.csv")
write_csv(Log_SCOTT_SS3, "E:/Logger_Data/Avg_Daily_SST_Loggers/Logger_Avg_Daily_SST_SCOTT_SS3.csv")
write_csv(Log_SCOTTNR1, "E:/Logger_Data/Avg_Daily_SST_Loggers/Logger_Avg_Daily_SST_SCOTTNR1.csv")
write_csv(Log_SCOTTSL1, "E:/Logger_Data/Avg_Daily_SST_Loggers/Logger_Avg_Daily_SST_SCOTTSL1.csv")
write_csv(Log_SCOTTSL2, "E:/Logger_Data/Avg_Daily_SST_Loggers/Logger_Avg_Daily_SST_SCOTTSL2.csv")
write_csv(Log_SCOTTSL3, "E:/Logger_Data/Avg_Daily_SST_Loggers/Logger_Avg_Daily_SST_SCOTTSL3.csv")
write_csv(Log_SCOTTSS2, "E:/Logger_Data/Avg_Daily_SST_Loggers/Logger_Avg_Daily_SST_SCOTTSS2.csv")
write_csv(Log_100THSITE, "D:/Logger_Data/Avg_Daily_SST_Loggers/Logger_Avg_Daily_SST_100THSITE.csv")
write_csv(Log_BUNDEGI_BR, "D:/Logger_Data/Avg_Daily_SST_Loggers/Logger_Avg_Daily_SST_BUNDEGI_BR.csv")
write_csv(Log_BUNDFL1, "D:/Logger_Data/Avg_Daily_SST_Loggers/Logger_Avg_Daily_SST_BUNDFL1.csv")
write_csv(Log_NTHFISHFL1, "D:/Logger_Data/Avg_Daily_SST_Loggers/Logger_Avg_Daily_SST_NTHFISHFL1.csv")
write_csv(Log_SCOTTSS1, "D:/Logger_Data/Avg_Daily_SST_Loggers/Logger_Avg_Daily_SST_SCOTTSS1.csv")
write_csv(Log_TANDFL1, "D:/Logger_Data/Avg_Daily_SST_Loggers/Logger_Avg_Daily_SST_TANDFL1.csv")
write_csv(Log_TANTABIDDI_SL1, "D:/Logger_Data/Avg_Daily_SST_Loggers/Logger_Avg_Daily_SST_TANTABIDDI_SL1.csv")

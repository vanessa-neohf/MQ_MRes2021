library(tidyverse)
library(lubridate)

#average daily SST for nearest data loggers to coral cores


#read csv files of logger sites
#SCOTTSS1, 100THSITE, TANDFL1, BUNDFL1, TANTABIDDI_SL1, BUNDEGI_BR, NTHFISHFL1

Log_SCOTTSS1 <- read_csv("SCOTTSS1.csv",
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

tbl_100TH <- Log_100THSITE %>% 
  mutate(date = format(as.Date(time))) %>% 
  group_by(date) %>% 
  summarize(mean(qc_val, na.rm = TRUE))

tbl_BUN_BR <- Log_BUNDEGI_BR %>% 
  mutate(date = format(as.Date(time))) %>% 
  group_by(date) %>% 
  summarize(mean(qc_val, na.rm = TRUE))

tbl_BUNDFL1 <- Log_BUNDFL1 %>% 
  mutate(date = format(as.Date(time))) %>% 
  group_by(date) %>% 
  summarize(mean(qc_val, na.rm = TRUE))

tbl_NTHFISHFL1 <- Log_NTHFISHFL1 %>% 
  mutate(date = format(as.Date(time))) %>% 
  group_by(date) %>% 
  summarize(mean(qc_val, na.rm = TRUE))

tbl_SCOTTSS1 <- Log_SCOTTSS1 %>% 
  mutate(date = format(as.Date(time))) %>% 
  group_by(date) %>% 
  summarize(mean(qc_val, na.rm = TRUE))

tbl_TANDFL1 <- Log_TANDFL1 %>% 
  mutate(date = format(as.Date(time))) %>% 
  group_by(date) %>% 
  summarize(mean(qc_val, na.rm = TRUE))

tbl_TANTABIDDI_SL1 <- Log_TANTABIDDI_SL1 %>% 
  mutate(date = format(as.Date(time))) %>% 
  group_by(date) %>% 
  summarize(mean(qc_val, na.rm = TRUE))

#add date column to logger files (for joining with summary table)
Log_100THSITE <- Log_100THSITE %>% 
  mutate(date = format(as.Date(time)))

Log_BUNDEGI_BR <- Log_BUNDEGI_BR %>% 
  mutate(date = format(as.Date(time)))

Log_BUNDFL1 <- Log_BUNDFL1 %>% 
  mutate(date = format(as.Date(time)))

Log_NTHFISHFL1 <- Log_NTHFISHFL1 %>% 
  mutate(date = format(as.Date(time)))

Log_SCOTTSS1 <- Log_SCOTTSS1 %>% 
  mutate(date = format(as.Date(time)))

Log_TANDFL1 <- Log_TANDFL1 %>% 
  mutate(date = format(as.Date(time)))

Log_TANTABIDDI_SL1 <- Log_TANTABIDDI_SL1 %>% 
  mutate(date = format(as.Date(time)))

#join SST values in summary table with logger file
Log_100THSITE <- left_join(Log_100THSITE, tbl_100TH, by = "date")
Log_BUNDEGI_BR <- left_join(Log_BUNDEGI_BR, tbl_BUN_BR, by = "date")
Log_BUNDFL1 <- left_join(Log_BUNDFL1, tbl_BUNDFL1, by = "date")
Log_NTHFISHFL1 <- left_join(Log_NTHFISHFL1, tbl_NTHFISHFL1, by = "date")
Log_SCOTTSS1 <- left_join(Log_SCOTTSS1, tbl_SCOTTSS1, by = "date")
Log_TANDFL1 <- left_join(Log_TANDFL1, tbl_TANDFL1, by = "date")
Log_TANTABIDDI_SL1 <- left_join(Log_TANTABIDDI_SL1, tbl_TANTABIDDI_SL1, by = "date")

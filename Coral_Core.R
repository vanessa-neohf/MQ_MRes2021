library(tidyverse)

## 1. Extracting Browse Island BRS05, BRS07 data ##

#read Browse Island data
Browse <- read_csv("Browse_Island_Data.csv")

#creating a new variable for decimal places of age
#use floor to identify closest integer below actual value
#age-floor to create a new variable for month analysis

Floor <- floor(Browse$age)
Browse_Y_M <- Browse %>% 
  mutate(year = Floor, mth_deci = age-Floor)

#Check for number of distinct years
n_distinct(Browse_Y_M$year)

#Check for number of distinct months
n_distinct(Browse_Y_M$mth_deci)
unique(Browse_Y_M$mth_deci) #13 values including 12 months and NA

#Convert decimal places to month in Browse_Y_M
#every 0.083 in value represents a new month
#case_when to convert values to month
#rounding to standardize to 3dp
Browse_Year_Month <- Browse_Y_M %>% 
  mutate(mth_deci = round(mth_deci, 3),
         month = case_when(
           mth_deci == 0.000 ~ "January",
           mth_deci == 0.083 ~ "February",
           mth_deci == 0.167 ~ "March",
           mth_deci == 0.25 ~ "April",
           mth_deci == 0.333 ~ "May",
           mth_deci == 0.417 ~ "June",
           mth_deci == 0.5 ~ "July",
           mth_deci == 0.583 ~ "August",
           mth_deci == 0.667 ~ "September",
           mth_deci == 0.750 ~ "October",
           mth_deci == 0.833 ~ "November",
           mth_deci == 0.917 ~ "December",
           TRUE ~ as.character(NA)
         )
  ) %>% 
  select(-mth_deci)

#write to new CSV file
write_csv(Browse_Year_Month, "D:/Coral_Proxy_Data/BRS05_BRS07_Year_Month.csv")

## 2. Extracting Cocos(Keeling) Islands DAR Long, DAR3 data ##
library(lubridate)
library(tidyverse)

#Read master coral proxy file
Master <- read_csv("Coral_MASMA_final_decimal lat_long_270421.csv")

#Filter DAR3 and DAR Long data 
DAR_Long <- Master %>%
  rename("core_id" = "core id") %>% 
  filter(core_id == "DAR Long")

DAR3 <- Master %>% 
  rename("core_id" = "core id") %>% 
  filter(core_id == "DAR3")

#Create year and month column for DAR_Long and DAR3
#Determine number of distinct months is 12

DAR_Long <- DAR_Long %>% 
  rename("Year" = "year") %>% 
  mutate(year = floor(Year), month = Year - year)

unique(DAR_Long$month)
n_distinct(DAR_Long$month)
DAR_

DAR3 <- DAR3 %>% 
  rename("Year" = "year") %>% 
  mutate(year = floor(Year), month = Year - year)

unique(DAR3$month)
n_distinct(DAR3$month)

#Convert month in decimal places to the actual month (DAR_Long site)
#Extract to CSV file
DAR_Long %>% 
  distinct(month)
DAR_Long_Year_Month <- DAR_Long %>% 
  mutate(month = round(month, 3),
         month = case_when(
           month == 0.000 ~ "January",
           month == 0.080 ~ "February",
           month == 0.170 ~ "March",
           month == 0.250 ~ "April",
           month == 0.330 ~ "May",
           month == 0.420 ~ "June",
           month == 0.500 ~ "July",
           month == 0.580 ~ "August",
           month == 0.670 ~ "September",
           month == 0.750 ~ "October",
           month == 0.830 ~ "November",
           month == 0.920 ~ "December"
           )
         )

n_distinct(DAR_Long_Year_Month$month)    
write_csv(DAR_Long_Year_Month, 
          "D:/Coral_Proxy_Data/Coral_Site_SST/DAR_Long_Year_Month.csv", append = FALSE)

#Convert month in decimal places to the actual month (DAR3 site)
#Extract to CSV file
DAR3 %>% 
  distinct(month)

DAR3_Year_Month <- DAR3 %>% 
  mutate(month = round(month, 3),
         month = case_when(
           month == 0.000 ~ "January",
           month == 0.080 ~ "February",
           month == 0.170 ~ "March",
           month == 0.250 ~ "April",
           month == 0.330 ~ "May",
           month == 0.420 ~ "June",
           month == 0.500 ~ "July",
           month == 0.580 ~ "August",
           month == 0.670 ~ "September",
           month == 0.750 ~ "October",
           month == 0.830 ~ "November",
           month == 0.920 ~ "December"
         )
  )

n_distinct(DAR3_Year_Month$month)    
write_csv(DAR_Long_Year_Month, 
          "D:/Coral_Proxy_Data/Coral_Site_SST/DAR3_Year_Month.csv", append = FALSE)

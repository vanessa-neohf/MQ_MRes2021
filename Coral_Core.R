library(tidyverse)

## 1. Extracting Browse Island data ##

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

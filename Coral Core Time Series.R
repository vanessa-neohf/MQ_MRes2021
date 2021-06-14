library(tidyverse)
library(lubridate)
library(glue)

#setwd
setwd("E:/Coral_Proxy_Data/Coral_Site_SST")

#load csv files of coral cores
Browse <- read_csv("BRS05_BRS07_Year_Month.csv")
DAR_Long <- read_csv("DAR_Long_Year_Month.csv")
DAR3 <- read_csv("DAR3_Year_Month.csv")
Ningaloo_08BND <- read_csv("Ningaloo_08BND_Year_Month.csv")
Ningaloo_08TNT <- read_csv("Ningaloo_08TNT_Year_Month.csv")
Ningaloo_13BND <- read_csv("Ningaloo_13BND_Year_Month.csv")
Ningaloo_13TNT <- read_csv("Ningaloo_13TNT_Year_Month.csv")
Ningaloo_TNT <- read_csv("Ningaloo_TNT_Year_Month.csv")
Ningaloo_TNT07C <- read_csv("Ningaloo_TNT07C_year.csv")
Ningaloo_BUN05A <- read_csv("Ningaloo_BUN05A_year.csv")

#standardize column names of Sr/Ca proxies - core id and Sr/Ca
Ningaloo_08BND <- Ningaloo_08BND %>% 
  rename(core_id = `core id`)

Ningaloo_13BND <- Ningaloo_13BND %>% 
  rename(core_id = `core id`)

Ningaloo_08TNT <- Ningaloo_08TNT %>% 
  rename(core_id = `core id`)

Ningaloo_13TNT <- Ningaloo_13TNT %>% 
  rename(core_id = `core id`)

Ningaloo_TNT <- Ningaloo_TNT %>% #not Sr/Ca proxy
  rename(core_id = `core id`)

Browse_BRS07 <- Browse %>% 
  mutate(core_id = "BRS07") %>% 
  rename(`Sr/Ca` = "BRS07 Sr/Ca [mmol/mol]")

Browse_BRS05 <- Browse %>% 
  mutate(core_id = "BRS05") %>% 
  rename(`Sr/Ca` = "BRS05 Sr/Ca [mmol/mol]")

rm(Browse)

Ningaloo_BUN05A <- Ningaloo_BUN05A %>% 
  mutate(core_id = "BUN05A") %>% 
  rename(`Sr/Ca` = "Bundegi Sr/Ca")

Ningaloo_TNT07C <- Ningaloo_TNT07C %>% 
  mutate(core_id = "TNT07C") %>% 
  rename(`Sr/Ca` = "Tantabiddi Sr/Ca")

#bind rows of sites with monthly coral core Sr/Ca data
#select only 4 columns with Sr/Ca, year, month and core_id
#Use glue to combine year and month into new column
Browse_BRS05_BRS07 <- bind_rows(Browse_BRS05, Browse_BRS07)

Browse_BRS05_BRS07 <- Browse_BRS05_BRS07 %>% 
  select("age", "Sr/Ca", "year", "month", "core_id") %>% 
  rename(SrCa = "Sr/Ca")

Browse_BRS05 <- Browse_BRS05 %>% 
  rename(SrCa = "Sr/Ca")


#plot time series for Browse Island, Cocos Islands and Ningaloo Reef SrCa coral core sites
Browse_BRS05_BRS07 %>% 
  ggplot(mapping = aes(x = age, y = SrCa)) +
  geom_point() +
  facet_wrap(~core_id)
  


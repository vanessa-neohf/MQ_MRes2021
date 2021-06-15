library(tidyverse)
library(lubridate)
library(glue)

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
  rename(`Sr/Ca` = "BRS07 Sr/Ca [mmol/mol]") %>% 
  rename(Year = age)

Browse_BRS05 <- Browse %>% 
  mutate(core_id = "BRS05") %>% 
  rename(`Sr/Ca` = "BRS05 Sr/Ca [mmol/mol]") %>% 
  rename(Year = age)

rm(Browse)

Ningaloo_BUN05A <- Ningaloo_BUN05A %>% 
  mutate(core_id = "BUN05A") %>% 
  rename(`Sr/Ca` = "Bundegi Sr/Ca")

Ningaloo_TNT07C <- Ningaloo_TNT07C %>% 
  mutate(core_id = "TNT07C") %>% 
  rename(`Sr/Ca` = "Tantabiddi Sr/Ca")

#bind rows of sites with monthly coral core Sr/Ca data
#select only 4 columns with Sr/Ca, year, month and core_id
#rename Sr/Ca into readable variable
SrCa_Proxies <- bind_rows(Browse_BRS05, Browse_BRS07, DAR_Long, DAR3, 
                                  Ningaloo_08BND, Ningaloo_08TNT, Ningaloo_13BND, Ningaloo_13TNT,
                                  Ningaloo_TNT, Ningaloo_TNT07C, Ningaloo_BUN05A)

SrCa_Proxies <- SrCa_Proxies %>% 
  select("Year", "Sr/Ca", "year", "month", "core_id") %>% 
  rename(SrCa = "Sr/Ca")

SrCa_Monthly_Proxies <- bind_rows(Browse_BRS05, Browse_BRS07, DAR_Long, DAR3, 
                          Ningaloo_08BND, Ningaloo_08TNT, Ningaloo_13BND, Ningaloo_13TNT,
                          Ningaloo_TNT)

SrCa_Monthly_Proxies <- SrCa_Monthly_Proxies %>% 
  select("Year", "Sr/Ca", "year", "month", "core_id") %>% 
  rename(SrCa = "Sr/Ca")

#use glue to join year and month to convert to date format
SrCa_Proxies <- SrCa_Proxies %>% 
  mutate(date = glue("{year}{month}{01}")) %>% 
  ymd(date)


#plot time series for Browse Island, Cocos Islands and Ningaloo Reef SrCa coral core sites
Browse_BRS05 %>% 
  ggplot(mapping = aes(x = age, y = SrCa)) +
  geom_point() +
  geom_line()+
  coord_cartesian(xlim = c(1980,2012)) +
  facet_wrap(~core_id)

Browse_BRS07 %>% 
  ggplot(mapping = aes(x = age, y = SrCa)) +
  geom_point() +
  geom_line()+
  facet_wrap(~core_id)

SrCa_Proxies %>% 
  ggplot(mapping = aes(x = Year, y = SrCa)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~core_id)

SrCa_Monthly_Proxies %>% 
  ggplot(mapping = aes(x = Year, y = SrCa)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~core_id)

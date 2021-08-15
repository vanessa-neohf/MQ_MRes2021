library(tidyverse)
library(lubridate)
library(patchwork)
library(fable)
library(feasts)
library(forecast)
library(tsibble)

##Houtman Abrolhos##
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


#skip this
# TS plot for Wallabi Island CCI 
plot_Wallabi_Island_CCI_rawts <- autoplot(Wallabi_Island_CCI, .vars = sst) + 
  geom_smooth(method = "loess") + 
  ggtitle("Time Series Plot of Houtman Abrolhos Wallabi Island Site CCI")
ggsave(file = here::here("graphics", 
                         "plot_Wallabi_Island_CCI_rawts.png"),
       plot= plot_Wallabi_Island_CCI_rawts, width=10, height=4)
#skipped

# Running the ARIMA of fable to detrend
M_Wallabi_Island_CCI <- Wallabi_Island_CCI %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Wallabi_Island_CCI_ARIMA <- M_Wallabi_Island_CCI %>% 
  select(arima)

#skip this
plot_M_Wallabi_Island_CCI_detrend <- as_tibble(
  residuals(M_Wallabi_Island_CCI_ARIMA, type = "regression")) %>% 
  ggplot(aes(x = date, y = .resid)) + 
  geom_line() + 
  ggtitle("Time Series Plot of Detrended Wallabi Island CCI by Linear Regression")

ggsave(file = here::here("graphics", 
                         "plot_Wallabi_Island_CCI_detrend.png"),
       plot = plot_M_Wallabi_Island_CCI_detrend, width = 10, height = 4)

#skipped


M1_Wallabi_Island_CCore <- lm(`d18O` ~ 1 +date, Wallabi_Island_CCore)
augment(M1_Wallabi_Island_CCore) %>% 
  mutate(.model = "arima") %>% 
  select(.model, date, .resid)

#skip this
M_Ningaloo_13TNT_CCore <- Ningaloo_13TNT_CCore %>% 
  drop_na(`Sr/Ca`) %>% 
  model(
    arima = ARIMA(`Sr/Ca`~ 
                    trend(), stepwise = FALSE),
    lin_mod = TSLM(`Sr/Ca` ~ 
                     trend())
  )

M_Ningaloo_13TNT_CCore_ARIMA <- M_Ningaloo_13TNT_CCore %>% 
  select(arima)
#skipped

Wallabi_Island_Logger %>% 
  scan_gaps() 
# found gaps in the middle; 
ggplot(Wallabi_Island_Logger, aes(x = date, y = sst)) + 
  geom_line()
# has only roughly 1 year a data. need two complete year data 
# but we will push on 

# let's fill the gaps
Wallabi_Island_Logger <- Wallabi_Island_Logger %>% 
  add_case(Wallabi_Island_Logger %>% 
             scan_gaps() %>% 
             mutate(sst  = NA)
  ) %>% 
  arrange(date)

M_Wallabi_Island_Logger <- Wallabi_Island_Logger %>% 
  model(
    arima = ARIMA(sst ~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Wallabi_Island_Logger_ARIMA <- M_Wallabi_Island_Logger %>% 
  select(arima)


M_Wallabi_Island_NOAA <- Wallabi_Island_NOAA %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Wallabi_Island_NOAA_ARIMA <- M_Wallabi_Island_NOAA %>% 
  select(arima)

resid_comb_Wallabi_Island <- bind_rows(
  CCI =
    as_tibble(residuals(M_Wallabi_Island_CCI_ARIMA, type = "regression")),
  CCore =
    augment(M1_Wallabi_Island_CCore) %>% 
    mutate(.model = "arima") %>% 
    select(.model, date, .resid),
  NOAA = 
    as_tibble(residuals(M_Wallabi_Island_NOAA_ARIMA, type = "regression")),
  Logger = as_tibble(residuals(M_Wallabi_Island_Logger_ARIMA, 
                               type = "regression")),
  .id = "type"
) 

plot_comb_resid_Wallabi_Island <- resid_comb_Wallabi_Island %>%
  mutate(
    type = factor(type, levels=c(
      "CCI", "CCore", "NOAA", "Logger"))
  ) %>%
  ggplot(aes(x = date, y = .resid)) +
  geom_line() +
  facet_wrap( ~ type, ncol = 4) + 
  ylab("Regression Residuals") 

ggsave(file = here::here("graphics", 
                         "plot_comb_resid_Wallabi_Island.png"),
       plot = plot_comb_resid_Wallabi_Island, width = 10, height = 4)

resid_comb_Wallabi_Island %>% 
  group_by(type) %>% 
  summarise(sd = sd(.resid, na.rm = TRUE))


########################################################################

##Houtman Abrolhos Islands## 
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


# Running the ARIMA of fable to detrend
M_HAB10A_d18O_CCI <- HAB10A_d18O_CCI %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_HAB10A_d18O_CCI_ARIMA <- M_HAB10A_d18O_CCI %>% 
  select(arima)


M1_HAB10A_d18O_CCore <- lm(`HAB10A d18O` ~ 1 +date, HAB10A_d18O_CCore)
augment(M1_HAB10A_d18O_CCore) %>% 
  mutate(.model = "arima") %>% 
  select(.model, date, .resid)


HAB10A_d18O_Logger %>% 
  scan_gaps() 
# some gaps observed
ggplot(HAB10A_d18O_Logger, aes(x = date, y = sst)) + 
  geom_line()


#let's fill up the gaps
HAB10A_d18O_Logger <- HAB10A_d18O_Logger %>% 
  add_case(HAB10A_d18O_Logger %>% 
             scan_gaps() %>% 
             mutate(sst  = NA)
  ) %>% 
  arrange(date)


M_HAB10A_d18O_Logger <- HAB10A_d18O_Logger %>% 
  model(
    arima = ARIMA(sst ~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_HAB10A_d18O_Logger_ARIMA <- M_HAB10A_d18O_Logger %>% 
  select(arima)


M_HAB10A_d18O_NOAA <- HAB10A_d18O_NOAA %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_HAB10A_d18O_NOAA_ARIMA <- M_HAB10A_d18O_NOAA %>% 
  select(arima)

resid_comb_HAB10A_d18O <- bind_rows(
  CCI =
    as_tibble(residuals(M_HAB10A_d18O_CCI_ARIMA, type = "regression")),
  CCore =
    augment(M1_HAB10A_d18O_CCore) %>% 
    mutate(.model = "arima") %>% 
    select(.model, date, .resid),
  NOAA = 
    as_tibble(residuals(M_HAB10A_d18O_NOAA_ARIMA, type = "regression")),
  Logger = as_tibble(residuals(M_HAB10A_d18O_Logger_ARIMA, 
                               type = "regression")),
  .id = "type"
) 

plot_comb_resid_HAB10A_d18O <- resid_comb_HAB10A_d18O %>%
  mutate(
    type = factor(type, levels=c(
      "CCI", "CCore", "NOAA", "Logger"))
  ) %>%
  ggplot(aes(x = date, y = .resid)) +
  geom_line() +
  facet_wrap( ~ type, ncol = 4) + 
  ylab("Regression Residuals") 

ggsave(file = here::here("graphics", 
                         "plot_comb_resid_HAB10A_d18O.png"),
       plot = plot_comb_resid_HAB10A_d18O, width = 10, height = 4)

resid_comb_HAB10A_d18O %>% 
  group_by(type) %>% 
  summarise(sd = sd(.resid, na.rm = TRUE))


################################################################

##Houtman Abrolhos Islands## 
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


# Running the ARIMA of fable to detrend
M_HAB05B_d18O_CCI <- HAB05B_d18O_CCI %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_HAB05B_d18O_CCI_ARIMA <- M_HAB05B_d18O_CCI %>% 
  select(arima)


M1_HAB05B_d18O_CCore <- lm(`HAB05B d18O` ~ 1 +date, HAB05B_CCore)
augment(M1_HAB05B_d18O_CCore) %>% 
  mutate(.model = "arima") %>% 
  select(.model, date, .resid)


HAB05B_d18O_Logger %>% 
  scan_gaps() 
# some gaps observed
ggplot(HAB05B_d18O_Logger, aes(x = date, y = sst)) + 
  geom_line()


#let's fill up the gaps
HAB05B_d18O_Logger <- HAB05B_d18O_Logger %>% 
  add_case(HAB05B_d18O_Logger %>% 
             scan_gaps() %>% 
             mutate(sst  = NA)
  ) %>% 
  arrange(date)


M_HAB05B_d18O_Logger <- HAB05B_d18O_Logger %>% 
  model(
    arima = ARIMA(sst ~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_HAB05B_d18O_Logger_ARIMA <- M_HAB05B_d18O_Logger %>% 
  select(arima)


M_HAB05B_d18O_NOAA <- HAB05B_d18O_NOAA %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_HAB05B_d18O_NOAA_ARIMA <- M_HAB05B_d18O_NOAA %>% 
  select(arima)

resid_comb_HAB05B_d18O <- bind_rows(
  CCI =
    as_tibble(residuals(M_HAB05B_d18O_CCI_ARIMA, type = "regression")),
  CCore =
    augment(M1_HAB05B_d18O_CCore) %>% 
    mutate(.model = "arima") %>% 
    select(.model, date, .resid),
  NOAA = 
    as_tibble(residuals(M_HAB05B_d18O_NOAA_ARIMA, type = "regression")),
  Logger = as_tibble(residuals(M_HAB05B_d18O_Logger_ARIMA, 
                               type = "regression")),
  .id = "type"
) 

plot_comb_resid_HAB05B_d18O <- resid_comb_HAB05B_d18O %>%
  mutate(
    type = factor(type, levels=c(
      "CCI", "CCore", "NOAA", "Logger"))
  ) %>%
  ggplot(aes(x = date, y = .resid)) +
  geom_line() +
  facet_wrap( ~ type, ncol = 4) + 
  ylab("Regression Residuals") 

ggsave(file = here::here("graphics", 
                         "plot_comb_resid_HAB05B_d18O.png"),
       plot = plot_comb_resid_HAB05B_d18O, width = 10, height = 4)

resid_comb_HAB05B_d18O %>% 
  group_by(type) %>% 
  summarise(sd = sd(.resid, na.rm = TRUE))


#######################################################################

##Houtman Abrolhos Islands## 
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


# Running the ARIMA of fable to detrend
M_HAB10A_SrCa_CCI <- HAB10A_SrCa_CCI %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_HAB10A_SrCa_CCI_ARIMA <- M_HAB10A_SrCa_CCI %>% 
  select(arima)


M1_HAB10A_SrCa_CCore <- lm(`HAB10A Sr/Ca` ~ 1 +date, HAB10A_SrCa_CCore)
augment(M1_HAB10A_SrCa_CCore) %>% 
  mutate(.model = "arima") %>% 
  select(.model, date, .resid)


HAB10A_SrCa_Logger %>% 
  scan_gaps() 
# some gaps observed
ggplot(HAB10A_SrCa_Logger, aes(x = date, y = sst)) + 
  geom_line()


#let's fill up the gaps
HAB10A_SrCa_Logger <- HAB10A_SrCa_Logger %>% 
  add_case(HAB10A_SrCa_Logger %>% 
             scan_gaps() %>% 
             mutate(sst  = NA)
  ) %>% 
  arrange(date)


M_HAB10A_SrCa_Logger <- HAB10A_SrCa_Logger %>% 
  model(
    arima = ARIMA(sst ~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_HAB10A_SrCa_Logger_ARIMA <- M_HAB10A_SrCa_Logger %>% 
  select(arima)


M_HAB10A_SrCa_NOAA <- HAB10A_SrCa_NOAA %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_HAB10A_SrCa_NOAA_ARIMA <- M_HAB10A_SrCa_NOAA %>% 
  select(arima)

resid_comb_HAB10A_SrCa <- bind_rows(
  CCI =
    as_tibble(residuals(M_HAB10A_SrCa_CCI_ARIMA, type = "regression")),
  CCore =
    augment(M1_HAB10A_SrCa_CCore) %>% 
    mutate(.model = "arima") %>% 
    select(.model, date, .resid),
  NOAA = 
    as_tibble(residuals(M_HAB10A_SrCa_NOAA_ARIMA, type = "regression")),
  Logger = as_tibble(residuals(M_HAB10A_SrCa_Logger_ARIMA, 
                               type = "regression")),
  .id = "type"
) 

plot_comb_resid_HAB10A_SrCa <- resid_comb_HAB10A_SrCa %>%
  mutate(
    type = factor(type, levels=c(
      "CCI", "CCore", "NOAA", "Logger"))
  ) %>%
  ggplot(aes(x = date, y = .resid)) +
  geom_line() +
  facet_wrap( ~ type, ncol = 4) + 
  ylab("Regression Residuals") 

ggsave(file = here::here("graphics", 
                         "plot_comb_resid_HAB10A_SrCa.png"),
       plot = plot_comb_resid_HAB10A_SrCa, width = 10, height = 4)

resid_comb_HAB10A_SrCa %>% 
  group_by(type) %>% 
  summarise(sd = sd(.resid, na.rm = TRUE))


################################################################

##Houtman Abrolhos Islands## 
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


# Running the ARIMA of fable to detrend
M_HAB05B_SrCa_CCI <- HAB05B_SrCa_CCI %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_HAB05B_SrCa_CCI_ARIMA <- M_HAB05B_SrCa_CCI %>% 
  select(arima)


M1_HAB05B_SrCa_CCore <- lm(`HAB05B Sr/Ca` ~ 1 +date, HAB05B_SrCa_CCore)
augment(M1_HAB05B_SrCa_CCore) %>% 
  mutate(.model = "arima") %>% 
  select(.model, date, .resid)


HAB05B_SrCa_Logger %>% 
  scan_gaps() 
# some gaps observed
ggplot(HAB05B_SrCa_Logger, aes(x = date, y = sst)) + 
  geom_line()


#let's fill up the gaps
HAB05B_SrCa_Logger <- HAB05B_SrCa_Logger %>% 
  add_case(HAB05B_SrCa_Logger %>% 
             scan_gaps() %>% 
             mutate(sst  = NA)
  ) %>% 
  arrange(date)


M_HAB05B_SrCa_Logger <- HAB05B_SrCa_Logger %>% 
  model(
    arima = ARIMA(sst ~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_HAB05B_SrCa_Logger_ARIMA <- M_HAB05B_SrCa_Logger %>% 
  select(arima)


M_HAB05B_SrCa_NOAA <- HAB05B_SrCa_NOAA %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_HAB05B_SrCa_NOAA_ARIMA <- M_HAB05B_SrCa_NOAA %>% 
  select(arima)

resid_comb_HAB05B_SrCa <- bind_rows(
  CCI =
    as_tibble(residuals(M_HAB05B_SrCa_CCI_ARIMA, type = "regression")),
  CCore =
    augment(M1_HAB05B_SrCa_CCore) %>% 
    mutate(.model = "arima") %>% 
    select(.model, date, .resid),
  NOAA = 
    as_tibble(residuals(M_HAB05B_SrCa_NOAA_ARIMA, type = "regression")),
  Logger = as_tibble(residuals(M_HAB05B_SrCa_Logger_ARIMA, 
                               type = "regression")),
  .id = "type"
) 

plot_comb_resid_HAB05B_SrCa <- resid_comb_HAB05B_SrCa %>%
  mutate(
    type = factor(type, levels=c(
      "CCI", "CCore", "NOAA", "Logger"))
  ) %>%
  ggplot(aes(x = date, y = .resid)) +
  geom_line() +
  facet_wrap( ~ type, ncol = 4) + 
  ylab("Regression Residuals") 

ggsave(file = here::here("graphics", 
                         "plot_comb_resid_HAB05B_SrCa.png"),
       plot = plot_comb_resid_HAB05B_SrCa, width = 10, height = 4)

resid_comb_HAB05B_SrCa %>% 
  group_by(type) %>% 
  summarise(sd = sd(.resid, na.rm = TRUE))

library(tidyverse)
library(lubridate)
library(patchwork)
library(fable)
library(feasts)
library(forecast)
library(tsibble)

##Cocos (Keeling) Islands
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


# TS plot for DAR Long CCI 
plot_DARL_CCI_rawts <- autoplot(DARL_CCI, .vars = sst) + 
  geom_smooth(method = "loess") + 
  ggtitle("Time Series Plot of DARL Site CCI")
ggsave(file = here::here("graphics", 
                         "plot_DARL_CCI_rawts.png"),
       plot= plot_DARL_CCI_rawts, width=10, height=4)

# Running the ARIMA of fable to detrend
M_DARL_CCI <- DARL_CCI %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_DARL_CCI_ARIMA <- M_DARL_CCI %>% 
  select(arima)

plot_M_DARL_CCI_detrend <- as_tibble(
  residuals(M_DARL_CCI_ARIMA, type = "regression")) %>% 
  ggplot(aes(x = date, y = .resid)) + 
  geom_line() + 
  ggtitle("Time Series Plot of Detrended DAR Long CCI by Linear Regression")

ggsave(file = here::here("graphics", 
                         "plot_DARL_CCI_detrend.png"),
       plot = plot_M_DARL_CCI_detrend, width = 10, height = 4)


M1_DARL_CCore <- lm(`Sr/Ca` ~ 1 +date, DARL_CCore)
augment(M1_DARL_CCore) %>% 
  mutate(.model = "arima") %>% 
  select(.model, date, .resid)

#skip this
M_DARL_CCore <- DARL_CCore %>% 
  drop_na(`Sr/Ca`) %>% 
  model(
    arima = ARIMA(`Sr/Ca`~ 
                    trend(), stepwise = FALSE),
    lin_mod = TSLM(`Sr/Ca` ~ 
                     trend())
  )

M_DARL_CCore_ARIMA <- M_DARL_CCore %>% 
  select(arima)
#skipped

DARL_Logger %>% 
  scan_gaps() 
# found 1 year gaps in the middle; 
ggplot(DARL_Logger, aes(x = date, y = sst)) + 
  geom_line()
# but we will push on 

# let's fill the gaps
DARL_Logger <- DARL_Logger %>% 
  add_case(DARL_Logger %>% 
             scan_gaps() %>% 
             mutate(sst  = NA)
  ) %>% 
  arrange(date)

M_DARL_Logger <- DARL_Logger %>% 
  model(
    arima = ARIMA(sst ~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_DARL_Logger_ARIMA <- M_DARL_Logger %>% 
  select(arima)


M_DARL_NOAA <- DARL_NOAA %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_DARL_NOAA_ARIMA <- M_DARL_NOAA %>% 
  select(arima)

resid_comb_DARL <- bind_rows(
  CCI =
    as_tibble(residuals(M_DARL_CCI_ARIMA, type = "regression")),
  CCore =
    augment(M1_DARL_CCore) %>% 
    mutate(.model = "arima") %>% 
    select(.model, date, .resid),
  NOAA = 
    as_tibble(residuals(M_DARL_NOAA_ARIMA, type = "regression")),
  Logger = as_tibble(residuals(M_DARL_Logger_ARIMA, 
                               type = "regression")),
  .id = "type"
) 

plot_comb_resid_DARL<- resid_comb_DARL %>%
  mutate(
    type = factor(type, levels=c(
      "CCI", "CCore", "NOAA", "Logger"))
  ) %>%
  ggplot(aes(x = date, y = .resid)) +
  geom_line() +
  facet_wrap( ~ type, ncol = 4) + 
  ylab("Regression Residuals") 

ggsave(file = here::here("graphics", 
                         "plot_comb_resid_DARL.png"),
       plot = plot_comb_resid_DARL, width = 10, height = 4)

resid_comb_DARL %>% 
  group_by(type) %>% 
  summarise(sd = sd(.resid, na.rm = TRUE))


###############################################################

##Cocos (Keeling) Islands
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


# TS plot for DAR Long CCI 
plot_DAR3_CCI_rawts <- autoplot(DAR3_CCI, .vars = sst) + 
  geom_smooth(method = "loess") + 
  ggtitle("Time Series Plot of DAR3 Site CCI")
ggsave(file = here::here("graphics", 
                         "plot_DAR3_CCI_rawts.png"),
       plot= plot_DAR3_CCI_rawts, width=10, height=4)

# Running the ARIMA of fable to detrend
M_DAR3_CCI <- DAR3_CCI %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_DAR3_CCI_ARIMA <- M_DAR3_CCI %>% 
  select(arima)

plot_M_DAR3_CCI_detrend <- as_tibble(
  residuals(M_DAR3_CCI_ARIMA, type = "regression")) %>% 
  ggplot(aes(x = date, y = .resid)) + 
  geom_line() + 
  ggtitle("Time Series Plot of Detrended DAR3 CCI by Linear Regression")

ggsave(file = here::here("graphics", 
                         "plot_DAR3_CCI_detrend.png"),
       plot = plot_M_DAR3_CCI_detrend, width = 10, height = 4)


M1_DAR3_CCore <- lm(`Sr/Ca` ~ 1 +date, DAR3_CCore)
augment(M1_DAR3_CCore) %>% 
  mutate(.model = "arima") %>% 
  select(.model, date, .resid)

#skip this
M_DAR3_CCore <- DAR3_CCore %>% 
  drop_na(`Sr/Ca`) %>% 
  model(
    arima = ARIMA(`Sr/Ca`~ 
                    trend(), stepwise = FALSE),
    lin_mod = TSLM(`Sr/Ca` ~ 
                     trend())
  )

M_DAR3_CCore_ARIMA <- M_DAR3_CCore %>% 
  select(arima)
#skipped

DAR3_Logger %>% 
  scan_gaps() 
# found 1 year gaps in the middle; 
ggplot(DAR3_Logger, aes(x = date, y = sst)) + 
  geom_line()
# but we will push on 

# let's fill the gaps
DAR3_Logger <- DAR3_Logger %>% 
  add_case(DAR3_Logger %>% 
             scan_gaps() %>% 
             mutate(sst  = NA)
  ) %>% 
  arrange(date)

M_DAR3_Logger <- DAR3_Logger %>% 
  model(
    arima = ARIMA(sst ~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_DAR3_Logger_ARIMA <- M_DAR3_Logger %>% 
  select(arima)


M_DAR3_NOAA <- DAR3_NOAA %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_DAR3_NOAA_ARIMA <- M_DAR3_NOAA %>% 
  select(arima)

resid_comb_DAR3 <- bind_rows(
  CCI =
    as_tibble(residuals(M_DAR3_CCI_ARIMA, type = "regression")),
  CCore =
    augment(M1_DAR3_CCore) %>% 
    mutate(.model = "arima") %>% 
    select(.model, date, .resid),
  NOAA = 
    as_tibble(residuals(M_DAR3_NOAA_ARIMA, type = "regression")),
  Logger = as_tibble(residuals(M_DAR3_Logger_ARIMA, 
                               type = "regression")),
  .id = "type"
) 

plot_comb_resid_DAR3<- resid_comb_DAR3 %>%
  mutate(
    type = factor(type, levels=c(
      "CCI", "CCore", "NOAA", "Logger"))
  ) %>%
  ggplot(aes(x = date, y = .resid)) +
  geom_line() +
  facet_wrap( ~ type, ncol = 4) + 
  ylab("Regression Residuals") 

ggsave(file = here::here("graphics", 
                         "plot_comb_resid_DAR3.png"),
       plot = plot_comb_resid_DAR3, width = 10, height = 4)

resid_comb_DAR3 %>% 
  group_by(type) %>% 
  summarise(sd = sd(.resid, na.rm = TRUE))

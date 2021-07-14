library(tidyverse)
library(lubridate)
library(patchwork)
library(fable)
library(feasts)
library(forecast)
library(tsibble)
##Browse Island##
##BRS05
Browse_05_CCI <- read_csv(here::here("data_raw", "CCI_BRS05.csv")) 
Browse_05_CCI <- Browse_05_CCI %>% 
  mutate(sst = as.numeric(str_sub(Browse_05_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>% #select only useful columns 
  as_tsibble(index = date) # convert them to tsibble

# found rows of na at the bottom of the csv file 
# add in the n_max argument to cut them out 
Browse_05_CCore <- read_csv(here::here("data_raw", "CCore_BRS05_BRS07_Year_Month.csv"), n_max = 740) %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = str_c(year, month, sep = "-")) %>% #stringing year and month
  mutate(date = yearmonth(date)) %>% #converting date column fr chr to date format
  select(data_type, date, `BRS05 Sr/Ca [mmol/mol]`) %>%  #select only useful columns 
  as_tsibble(index = date) # convert them to tsibble 

Browse_05_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_SCOTTSS1.csv")) %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst)  %>% #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Browse_05_NOAA <- read_csv(here::here("data_raw", "NOAA_BRS05_SST.csv")) %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

# TS plot for Browse 05 CCI 
plot_Browse_05_CCI_rawts <- autoplot(Browse_05_CCI, .vars = sst) + 
  geom_smooth(method = "loess") + 
  ggtitle("Time Series Plot of Browse 05 CCI")
ggsave(file = here::here("graphics", 
                         "plot_Browse_05_CCI_rawts.png"),
       plot= plot_Browse_05_CCI_rawts, width=10, height=4)

# Running the ARIMA of fable to detrend
M_05_CCI <- Browse_05_CCI %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_05_CCI_ARIMA <- M_05_CCI %>% 
  select(arima)

plot_Browse_05_CCI_detrend <- as_tibble(
  residuals(M_05_CCI_ARIMA, type = "regression")) %>% 
  ggplot(aes(x = date, y = .resid)) + 
  geom_line() + 
  ggtitle("Time Series Plot of Detrended Browse 05 CCI by Linear Regression")

ggsave(file = here::here("graphics", 
                         "plot_Browse_05_CCI_detrend.png"),
       plot = plot_Browse_05_CCI_detrend, width = 10, height = 4)


M_05_CCI <- Browse_05_CCI %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_05_CCI_ARIMA <- M_05_CCI %>% 
  select(arima)

plot_Browse_05_CCI_detrend <- as_tibble(
  residuals(M_05_CCI_ARIMA, type = "regression")) %>% 
  ggplot(aes(x = date, y = .resid)) + 
  geom_line() + 
  ggtitle("Time Series Plot of Detrended Browse 05 CCI by Linear Regression")

ggsave(file = here::here("graphics", 
                         "plot_Browse_05_CCI_detrend.png"),
       plot = plot_Browse_05_CCI_detrend, width = 10, height = 4)


M_05_CCore <- Browse_05_CCore %>% 
  drop_na(`BRS05 Sr/Ca [mmol/mol]`) %>% 
  model(
    arima = ARIMA(`BRS05 Sr/Ca [mmol/mol]`~ 
                    trend(), stepwise = FALSE),
    lin_mod = TSLM(`BRS05 Sr/Ca [mmol/mol]` ~ 
                     trend())
  )

M_05_CCore_ARIMA <- M_05_CCore %>% 
  select(arima)

Browse_05_Logger %>% 
  scan_gaps() 
# found tons of gaps in the middle; 
ggplot(Browse_05_Logger, aes(x = date, y = sst)) %>% 
  geom_line()
# has only roughly 1 year a data. need two complete year data 
# but we will push on 

# let's fill the gaps
Browse_05_Logger <- Browse_05_Logger %>% 
  add_case(Browse_05_Logger %>% 
             scan_gaps() %>% 
             mutate(sst  = NA)
           ) %>% 
  arrange(date)

M_05_Logger <- Browse_05_Logger %>% 
  model(
  arima = ARIMA(sst ~ trend(), stepwise = FALSE),
  lin_mod = TSLM(sst ~ trend())
)

M_05_Logger_ARIMA <- M_05_Logger %>% 
  select(arima)
  

M_05_NOAA <- Browse_05_NOAA %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_05_NOAA_ARIMA <- M_05_NOAA %>% 
  select(arima)

resid_comb <- bind_rows(
  CCI =
    as_tibble(residuals(M_05_CCI_ARIMA, type = "regression")),
  CCore =
    as_tibble(residuals(M_05_CCore_ARIMA, type = "regression")),
  NOAA = 
    as_tibble(residuals(M_05_NOAA_ARIMA, type = "regression")),
  Logger = as_tibble(residuals(M_05_Logger_ARIMA, 
                               type = "regression")),
  .id = "type"
) 

plot_comb_resid <- resid_comb %>%
  mutate(
    type = factor(type, levels=c(
      "CCI", "CCore", "NOAA", "Logger"))
  ) %>%
  ggplot(aes(x = date, y = .resid)) +
  geom_line() +
  facet_wrap( ~ type, ncol = 4) + 
  ylab("Regression Residuals") 

ggsave(file = here::here("graphics", 
                         "plot_comb_resid.png"),
       plot = plot_comb_resid, width = 10, height = 4)

resid_comb %>% 
  group_by(type) %>% 
  summarise(sd = sd(.resid, na.rm = TRUE))

install.packages("TSclust")



-------------------------------------------

CCI <- 
  as_tibble(residuals(fit, type = "regression")) %>% 
  mutate(dat = as.character(date)) %>% 
  rename(CCI_resid = .resid)
NOAA <- 
  as_tibble(residuals(fit2, type = "regression")) %>% 
  mutate(dat = as.character(date)) %>% 
  rename(NOAA_resid = .resid)

left_join(CCI, NOAA) %>% 
  mutate(resid_diff = CCI_resid - NOAA_resid) %>% 
  ggplot(aes(x = date, y = resid_diff)) + 
  geom_point() + 
  geom_smooth(method="loess")


install.packages("TSclust")

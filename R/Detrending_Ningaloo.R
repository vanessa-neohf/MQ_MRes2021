library(tidyverse)
library(lubridate)
library(patchwork)
library(fable)
library(feasts)
library(forecast)
library(tsibble)

##Ningaloo Reef##
##Tantabiddi 13TNT
Ningaloo_13TNT_CCI <- read_csv(here::here("data_raw", "CCI_Tantabiddi.csv"))
Ningaloo_13TNT_CCI <- Ningaloo_13TNT_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_13TNT_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_13TNT_CCore <- read_csv(here::here("data_raw", "CCore_Ningaloo_13TNT_Year_Month.csv"))
Ningaloo_13TNT_CCore <- Ningaloo_13TNT_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = as_date(date)) %>% 
  select(data_type, date, `Sr/Ca`) %>%     #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_13TNT_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_TANDFL1.csv"))
Ningaloo_13TNT_Logger <- Ningaloo_13TNT_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst) %>%   #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_13TNT_NOAA <- read_csv(here::here("data_raw", "NOAA_Tantabiddi_13TNT_SST.csv"))
Ningaloo_13TNT_NOAA <- Ningaloo_13TNT_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble


# TS plot for Ningaloo 13TNT CCI 
plot_Ningaloo_13TNT_CCI_rawts <- autoplot(Ningaloo_13TNT_CCI, .vars = sst) + 
  geom_smooth(method = "loess") + 
  ggtitle("Time Series Plot of Ningaloo 13TNT Site CCI")
ggsave(file = here::here("graphics", 
                         "plot_Ningaloo_13TNT_CCI_rawts.png"),
       plot= plot_Ningaloo_13TNT_CCI_rawts, width=10, height=4)

# Running the ARIMA of fable to detrend
M_Ningaloo_13TNT_CCI <- Ningaloo_13TNT_CCI %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Ningaloo_13TNT_CCI_ARIMA <- M_Ningaloo_13TNT_CCI %>% 
  select(arima)

plot_M_Ningaloo_13TNT_CCI_detrend <- as_tibble(
  residuals(M_Ningaloo_13TNT_CCI_ARIMA, type = "regression")) %>% 
  ggplot(aes(x = date, y = .resid)) + 
  geom_line() + 
  ggtitle("Time Series Plot of Detrended Ningaloo 13TNT CCI by Linear Regression")

ggsave(file = here::here("graphics", 
                         "plot_Ningaloo_13TNT_CCI_detrend.png"),
       plot = plot_M_Ningaloo_13TNT_CCI_detrend, width = 10, height = 4)


M1_13TNT_CCore <- lm(`Sr/Ca` ~ 1 +date, Ningaloo_13TNT_CCore)
augment(M1_13TNT_CCore) %>% 
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

Ningaloo_13TNT_Logger %>% 
  scan_gaps() 
# found tons of gaps in the middle; 
ggplot(Ningaloo_13TNT_Logger, aes(x = date, y = sst)) + 
  geom_line()
# has only roughly 1 year a data. need two complete year data 
# but we will push on 

# let's fill the gaps
Ningaloo_13TNT_Logger <- Ningaloo_13TNT_Logger %>% 
  add_case(Ningaloo_13TNT_Logger %>% 
             scan_gaps() %>% 
             mutate(sst  = NA)
           ) %>% 
  arrange(date)

M_Ningaloo_13TNT_Logger <- Ningaloo_13TNT_Logger %>% 
  model(
  arima = ARIMA(sst ~ trend(), stepwise = FALSE),
  lin_mod = TSLM(sst ~ trend())
)

M_Ningaloo_13TNT_Logger_ARIMA <- M_Ningaloo_13TNT_Logger %>% 
  select(arima)
  

M_Ningaloo_13TNT_NOAA <- Ningaloo_13TNT_NOAA %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Ningaloo_13TNT_NOAA_ARIMA <- M_Ningaloo_13TNT_NOAA %>% 
  select(arima)

resid_comb_13TNT <- bind_rows(
  CCI =
    as_tibble(residuals(M_Ningaloo_13TNT_CCI_ARIMA, type = "regression")),
  CCore =
    augment(M1_13TNT_CCore) %>% 
    mutate(.model = "arima") %>% 
    select(.model, date, .resid),
  NOAA = 
    as_tibble(residuals(M_Ningaloo_13TNT_NOAA_ARIMA, type = "regression")),
  Logger = as_tibble(residuals(M_Ningaloo_13TNT_Logger_ARIMA, 
                               type = "regression")),
  .id = "type"
) 

plot_comb_resid_13TNT <- resid_comb_13TNT %>%
  mutate(
    type = factor(type, levels=c(
      "CCI", "CCore", "NOAA", "Logger"))
  ) %>%
  ggplot(aes(x = date, y = .resid)) +
  geom_line() +
  facet_wrap( ~ type, ncol = 4) + 
  ylab("Regression Residuals") 

ggsave(file = here::here("graphics", 
                         "plot_comb_resid_13TNT.png"),
       plot = plot_comb_resid_13TNT, width = 10, height = 4)

resid_comb_13TNT %>% 
  group_by(type) %>% 
  summarise(sd = sd(.resid, na.rm = TRUE))


################################################

##Ningaloo Reef##
##Tantabiddi 08TNT
Ningaloo_08TNT_CCI <- read_csv(here::here("data_raw", "CCI_Tantabiddi.csv"))
Ningaloo_08TNT_CCI <- Ningaloo_08TNT_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_08TNT_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_08TNT_CCore <- read_csv(here::here("data_raw", "CCore_Ningaloo_08TNT_Year_Month.csv"))
Ningaloo_08TNT_CCore <- Ningaloo_08TNT_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = as_date(date)) %>% 
  select(data_type, date, `Sr/Ca`) %>%     #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_08TNT_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_TANDFL1.csv"))
Ningaloo_08TNT_Logger <- Ningaloo_08TNT_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst) %>%   #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_08TNT_NOAA <- read_csv(here::here("data_raw", "NOAA_Tantabiddi_08TNT_SST.csv"))
Ningaloo_08TNT_NOAA <- Ningaloo_08TNT_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble


# TS plot for Ningaloo 08TNT CCI 
plot_Ningaloo_08TNT_CCI_rawts <- autoplot(Ningaloo_08TNT_CCI, .vars = sst) + 
  geom_smooth(method = "loess") + 
  ggtitle("Time Series Plot of Ningaloo 08TNT Site CCI")
ggsave(file = here::here("graphics", 
                         "plot_Ningaloo_08TNT_CCI_rawts.png"),
       plot= plot_Ningaloo_08TNT_CCI_rawts, width=10, height=4)

# Running the ARIMA of fable to detrend
M_Ningaloo_08TNT_CCI <- Ningaloo_08TNT_CCI %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Ningaloo_08TNT_CCI_ARIMA <- M_Ningaloo_08TNT_CCI %>% 
  select(arima)

plot_M_Ningaloo_08TNT_CCI_detrend <- as_tibble(
  residuals(M_Ningaloo_08TNT_CCI_ARIMA, type = "regression")) %>% 
  ggplot(aes(x = date, y = .resid)) + 
  geom_line() + 
  ggtitle("Time Series Plot of Detrended Ningaloo 08TNT CCI by Linear Regression")

ggsave(file = here::here("graphics", 
                         "plot_Ningaloo_08TNT_CCI_detrend.png"),
       plot = plot_M_Ningaloo_08TNT_CCI_detrend, width = 10, height = 4)



M1_08TNT_CCore <- lm(`Sr/Ca` ~ 1 +date, Ningaloo_08TNT_CCore)
augment(M1_08TNT_CCore) %>% 
  mutate(.model = "arima") %>% 
  select(.model, date, .resid)

#skip this
M_Ningaloo_08TNT_CCore <- Ningaloo_08TNT_CCore %>% 
  drop_na(`Sr/Ca`) %>% 
  model(
    arima = ARIMA(`Sr/Ca`~ 
                    trend(), stepwise = FALSE),
    lin_mod = TSLM(`Sr/Ca` ~ 
                     trend())
  )

M_Ningaloo_08TNT_CCore_ARIMA <- M_Ningaloo_08TNT_CCore %>% 
  select(arima)
#skipped

Ningaloo_08TNT_Logger %>% 
  scan_gaps() 
# found tons of gaps in the middle; 
ggplot(Ningaloo_08TNT_Logger, aes(x = date, y = sst)) + 
  geom_line()


# let's fill the gaps
Ningaloo_08TNT_Logger <- Ningaloo_08TNT_Logger %>% 
  add_case(Ningaloo_08TNT_Logger %>% 
             scan_gaps() %>% 
             mutate(sst  = NA)
  ) %>% 
  arrange(date)

M_Ningaloo_08TNT_Logger <- Ningaloo_08TNT_Logger %>% 
  model(
    arima = ARIMA(sst ~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Ningaloo_08TNT_Logger_ARIMA <- M_Ningaloo_08TNT_Logger %>% 
  select(arima)


M_Ningaloo_08TNT_NOAA <- Ningaloo_08TNT_NOAA %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Ningaloo_08TNT_NOAA_ARIMA <- M_Ningaloo_08TNT_NOAA %>% 
  select(arima)

resid_comb_08TNT <- bind_rows(
  CCI =
    as_tibble(residuals(M_Ningaloo_08TNT_CCI_ARIMA, type = "regression")),
  CCore =
    augment(M1_08TNT_CCore) %>% 
    mutate(.model = "arima") %>% 
    select(.model, date, .resid),
  NOAA = 
    as_tibble(residuals(M_Ningaloo_08TNT_NOAA_ARIMA, type = "regression")),
  Logger = as_tibble(residuals(M_Ningaloo_08TNT_Logger_ARIMA, 
                               type = "regression")),
  .id = "type"
) 

plot_comb_resid_08TNT <- resid_comb_08TNT %>%
  mutate(
    type = factor(type, levels=c(
      "CCI", "CCore", "NOAA", "Logger"))
  ) %>%
  ggplot(aes(x = date, y = .resid)) +
  geom_line() +
  facet_wrap( ~ type, ncol = 4) + 
  ylab("Regression Residuals") 

ggsave(file = here::here("graphics", 
                         "plot_comb_resid_08TNT.png"),
       plot = plot_comb_resid_08TNT, width = 10, height = 4)

resid_comb_08TNT %>% 
  group_by(type) %>% 
  summarise(sd = sd(.resid, na.rm = TRUE))


###############################################################

##Ningaloo Reef##
##Bundegi 13BND
Ningaloo_13BND_CCI <- read_csv(here::here("data_raw", "CCI_Bundegi.csv"))
Ningaloo_13BND_CCI <- Ningaloo_13BND_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_13BND_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_13BND_CCore <- read_csv(here::here("data_raw", "CCore_Ningaloo_13BND_Year_Month.csv"))
Ningaloo_13BND_CCore <- Ningaloo_13BND_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = as_date(date)) %>% 
  select(data_type, date, `Sr/Ca`) %>%     #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_13BND_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_BUNDFL1.csv"))
Ningaloo_13BND_Logger <- Ningaloo_13BND_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst) %>%   #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_13BND_NOAA <- read_csv(here::here("data_raw", "NOAA_Bundegi_13BND_SST.csv"))
Ningaloo_13BND_NOAA <- Ningaloo_13BND_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble


# Running the ARIMA of fable to detrend
M_Ningaloo_13BND_CCI <- Ningaloo_13BND_CCI %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Ningaloo_13BND_CCI_ARIMA <- M_Ningaloo_13BND_CCI %>% 
  select(arima)


M1_13BND_CCore <- lm(`Sr/Ca` ~ 1 +date, Ningaloo_13BND_CCore)
augment(M1_13TNT_CCore) %>% 
  mutate(.model = "arima") %>% 
  select(.model, date, .resid)


Ningaloo_13BND_Logger %>% 
  scan_gaps() 
# found tons of gaps in the middle; 
ggplot(Ningaloo_13BND_Logger, aes(x = date, y = sst)) + 
  geom_line()

# let's fill the gaps
Ningaloo_13BND_Logger <- Ningaloo_13BND_Logger %>% 
  add_case(Ningaloo_13BND_Logger %>% 
             scan_gaps() %>% 
             mutate(sst  = NA)
  ) %>% 
  arrange(date)

M_Ningaloo_13BND_Logger <- Ningaloo_13BND_Logger %>% 
  model(
    arima = ARIMA(sst ~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Ningaloo_13BND_Logger_ARIMA <- M_Ningaloo_13BND_Logger %>% 
  select(arima)


M_Ningaloo_13BND_NOAA <- Ningaloo_13BND_NOAA %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Ningaloo_13BND_NOAA_ARIMA <- M_Ningaloo_13BND_NOAA %>% 
  select(arima)

resid_comb_13BND <- bind_rows(
  CCI =
    as_tibble(residuals(M_Ningaloo_13BND_CCI_ARIMA, type = "regression")),
  CCore =
    augment(M1_13BND_CCore) %>% 
    mutate(.model = "arima") %>% 
    select(.model, date, .resid),
  NOAA = 
    as_tibble(residuals(M_Ningaloo_13BND_NOAA_ARIMA, type = "regression")),
  Logger = as_tibble(residuals(M_Ningaloo_13BND_Logger_ARIMA, 
                               type = "regression")),
  .id = "type"
) 

plot_comb_resid_13BND <- resid_comb_13BND %>%
  mutate(
    type = factor(type, levels=c(
      "CCI", "CCore", "NOAA", "Logger"))
  ) %>%
  ggplot(aes(x = date, y = .resid)) +
  geom_line() +
  facet_wrap( ~ type, ncol = 4) + 
  ylab("Regression Residuals") 

ggsave(file = here::here("graphics", 
                         "plot_comb_resid_13BND.png"),
       plot = plot_comb_resid_13BND, width = 10, height = 4)

resid_comb_13BND %>% 
  group_by(type) %>% 
  summarise(sd = sd(.resid, na.rm = TRUE))


################################################

##Ningaloo Reef##
##Bundegi 08BND
Ningaloo_08BND_CCI <- read_csv(here::here("data_raw", "CCI_Bundegi.csv"))
Ningaloo_08BND_CCI <- Ningaloo_08BND_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_08BND_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_08BND_CCore <- read_csv(here::here("data_raw", "CCore_Ningaloo_08BND_Year_Month.csv"))
Ningaloo_08BND_CCore <- Ningaloo_08BND_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = as_date(date)) %>% 
  select(data_type, date, `Sr/Ca`) %>%     #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_08BND_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_BUNDFL1.csv"))
Ningaloo_08BND_Logger <- Ningaloo_08BND_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst) %>%   #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_08BND_NOAA <- read_csv(here::here("data_raw", "NOAA_Bundegi_08BND_SST.csv"))
Ningaloo_08BND_NOAA <- Ningaloo_08BND_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble


# Running the ARIMA of fable to detrend
M_Ningaloo_08BND_CCI <- Ningaloo_08BND_CCI %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Ningaloo_08BND_CCI_ARIMA <- M_Ningaloo_08BND_CCI %>% 
  select(arima)


M1_08BND_CCore <- lm(`Sr/Ca` ~ 1 +date, Ningaloo_08BND_CCore)
augment(M1_08BND_CCore) %>% 
  mutate(.model = "arima") %>% 
  select(.model, date, .resid)


Ningaloo_08BND_Logger %>% 
  scan_gaps() 
# found tons of gaps in the middle; 
ggplot(Ningaloo_08BND_Logger, aes(x = date, y = sst)) + 
  geom_line()


# let's fill the gaps
Ningaloo_08BND_Logger <- Ningaloo_08BND_Logger %>% 
  add_case(Ningaloo_08BND_Logger %>% 
             scan_gaps() %>% 
             mutate(sst  = NA)
  ) %>% 
  arrange(date)

M_Ningaloo_08BND_Logger <- Ningaloo_08BND_Logger %>% 
  model(
    arima = ARIMA(sst ~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Ningaloo_08BND_Logger_ARIMA <- M_Ningaloo_08BND_Logger %>% 
  select(arima)


M_Ningaloo_08BND_NOAA <- Ningaloo_08BND_NOAA %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Ningaloo_08BND_NOAA_ARIMA <- M_Ningaloo_08BND_NOAA %>% 
  select(arima)

resid_comb_08BND <- bind_rows(
  CCI =
    as_tibble(residuals(M_Ningaloo_08BND_CCI_ARIMA, type = "regression")),
  CCore =
    augment(M1_08BND_CCore) %>% 
    mutate(.model = "arima") %>% 
    select(.model, date, .resid),
  NOAA = 
    as_tibble(residuals(M_Ningaloo_08BND_NOAA_ARIMA, type = "regression")),
  Logger = as_tibble(residuals(M_Ningaloo_08BND_Logger_ARIMA, 
                               type = "regression")),
  .id = "type"
) 

plot_comb_resid_08BND <- resid_comb_08BND %>%
  mutate(
    type = factor(type, levels=c(
      "CCI", "CCore", "NOAA", "Logger"))
  ) %>%
  ggplot(aes(x = date, y = .resid)) +
  geom_line() +
  facet_wrap( ~ type, ncol = 4) + 
  ylab("Regression Residuals") 

ggsave(file = here::here("graphics", 
                         "plot_comb_resid_08BND.png"),
       plot = plot_comb_resid_08BND, width = 10, height = 4)

resid_comb_08BND %>% 
  group_by(type) %>% 
  summarise(sd = sd(.resid, na.rm = TRUE))


###################################################


##Ningaloo Reef##
##TNT

Ningaloo_TNT_CCI <- read_csv(here::here("data_raw", "CCI_TNT.csv"))
Ningaloo_TNT_CCI <- Ningaloo_TNT_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_TNT_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_TNT_CCore <- read_csv(here::here("data_raw", "CCore_Ningaloo_TNT_Year_Month.csv"))
Ningaloo_TNT_CCore <- Ningaloo_TNT_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(date = as_date(date)) %>% 
  select(data_type, date, `Sr/Ca`) %>%     #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_TNT_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_TANDFL1.csv"))
Ningaloo_TNT_Logger <- Ningaloo_TNT_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst) %>%   #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_TNT_NOAA <- read_csv(here::here("data_raw", "NOAA_TNT_SST.csv"))
Ningaloo_TNT_NOAA <- Ningaloo_TNT_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble


# Running the ARIMA of fable to detrend
M_Ningaloo_TNT_CCI <- Ningaloo_TNT_CCI %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Ningaloo_TNT_CCI_ARIMA <- M_Ningaloo_TNT_CCI %>% 
  select(arima)


M1_TNT_CCore <- lm(`Sr/Ca` ~ 1 +date, Ningaloo_TNT_CCore)
augment(M1_TNT_CCore) %>% 
  mutate(.model = "arima") %>% 
  select(.model, date, .resid)


Ningaloo_TNT_Logger %>% 
  scan_gaps() 
# found gaps in the middle; 
ggplot(Ningaloo_TNT_Logger, aes(x = date, y = sst)) + 
  geom_line()


# let's fill the gaps
Ningaloo_TNT_Logger <- Ningaloo_TNT_Logger %>% 
  add_case(Ningaloo_TNT_Logger %>% 
             scan_gaps() %>% 
             mutate(sst  = NA)
  ) %>% 
  arrange(date)

M_Ningaloo_TNT_Logger <- Ningaloo_TNT_Logger %>% 
  model(
    arima = ARIMA(sst ~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Ningaloo_TNT_Logger_ARIMA <- M_Ningaloo_TNT_Logger %>% 
  select(arima)


M_Ningaloo_TNT_NOAA <- Ningaloo_TNT_NOAA %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Ningaloo_TNT_NOAA_ARIMA <- M_Ningaloo_TNT_NOAA %>% 
  select(arima)

resid_comb_TNT <- bind_rows(
  CCI =
    as_tibble(residuals(M_Ningaloo_TNT_CCI_ARIMA, type = "regression")),
  CCore =
    augment(M1_TNT_CCore) %>% 
    mutate(.model = "arima") %>% 
    select(.model, date, .resid),
  NOAA = 
    as_tibble(residuals(M_Ningaloo_TNT_NOAA_ARIMA, type = "regression")),
  Logger = as_tibble(residuals(M_Ningaloo_TNT_Logger_ARIMA, 
                               type = "regression")),
  .id = "type"
) 

plot_comb_resid_TNT <- resid_comb_TNT %>%
  mutate(
    type = factor(type, levels=c(
      "CCI", "CCore", "NOAA", "Logger"))
  ) %>%
  ggplot(aes(x = date, y = .resid)) +
  geom_line() +
  facet_wrap( ~ type, ncol = 4) + 
  ylab("Regression Residuals") 

ggsave(file = here::here("graphics", 
                         "plot_comb_resid_TNT.png"),
       plot = plot_comb_resid_TNT, width = 10, height = 4)

resid_comb_TNT %>% 
  group_by(type) %>% 
  summarise(sd = sd(.resid, na.rm = TRUE))

###################################################


##Ningaloo Reef## 
##TNT07C
##Only annual coral core data

Ningaloo_TNT07C_CCI <- read_csv(here::here("data_raw", "CCI_TNT07C.csv"))
Ningaloo_TNT07C_CCI <- Ningaloo_TNT07C_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_TNT07C_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_TNT07C_CCore <- read_csv(here::here("data_raw", "CCore_Ningaloo_TNT07C_year.csv"))

Ningaloo_TNT07C_CCore <- Ningaloo_TNT07C_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(month = "01") %>% 
  mutate(date = str_c(year, month, sep = "-")) %>% 
  mutate(date = ym(date)) %>% 
  select(data_type, date, `Tantabiddi Sr/Ca`) %>% 
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_TNT07C_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_TANTABIDDI_SL1.csv"))
Ningaloo_TNT07C_Logger <- Ningaloo_TNT07C_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst) %>%   #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_TNT07C_NOAA <- read_csv(here::here("data_raw", "NOAA_TNT07C_SST.csv"))
Ningaloo_TNT07C_NOAA <- Ningaloo_TNT07C_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble


# Running the ARIMA of fable to detrend
M_Ningaloo_TNT07C_CCI <- Ningaloo_TNT07C_CCI %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Ningaloo_TNT07C_CCI_ARIMA <- M_Ningaloo_TNT07C_CCI %>% 
  select(arima)


M1_TNT07C_CCore <- lm(`Tantabiddi Sr/Ca` ~ 1 +date, Ningaloo_TNT07C_CCore)
augment(M1_TNT07C_CCore) %>% 
  mutate(.model = "arima") %>% 
  select(.model, date, .resid)


Ningaloo_TNT07C_Logger %>% 
  scan_gaps() 
# no gaps
ggplot(Ningaloo_TNT07C_Logger, aes(x = date, y = sst)) + 
  geom_line()


#skip this as there are no gaps
Ningaloo_TNT07C_Logger <- Ningaloo_TNT07C_Logger %>% 
  add_case(Ningaloo_TNT07C_Logger %>% 
             scan_gaps() %>% 
             mutate(sst  = NA)
  ) %>% 
  arrange(date)
#skipped above

M_Ningaloo_TNT07C_Logger <- Ningaloo_TNT07C_Logger %>% 
  model(
    arima = ARIMA(sst ~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Ningaloo_TNT07C_Logger_ARIMA <- M_Ningaloo_TNT07C_Logger %>% 
  select(arima)


M_Ningaloo_TNT07C_NOAA <- Ningaloo_TNT07C_NOAA %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Ningaloo_TNT07C_NOAA_ARIMA <- M_Ningaloo_TNT07C_NOAA %>% 
  select(arima)

resid_comb_TNT07C <- bind_rows(
  CCI =
    as_tibble(residuals(M_Ningaloo_TNT07C_CCI_ARIMA, type = "regression")),
  CCore =
    augment(M1_TNT07C_CCore) %>% 
    mutate(.model = "arima") %>% 
    select(.model, date, .resid),
  NOAA = 
    as_tibble(residuals(M_Ningaloo_TNT07C_NOAA_ARIMA, type = "regression")),
  Logger = as_tibble(residuals(M_Ningaloo_TNT07C_Logger_ARIMA, 
                               type = "regression")),
  .id = "type"
) 

plot_comb_resid_TNT07C <- resid_comb_TNT07C %>%
  mutate(
    type = factor(type, levels=c(
      "CCI", "CCore", "NOAA", "Logger"))
  ) %>%
  ggplot(aes(x = date, y = .resid)) +
  geom_line() +
  facet_wrap( ~ type, ncol = 4) + 
  ylab("Regression Residuals") 

ggsave(file = here::here("graphics", 
                         "plot_comb_resid_TNT07C.png"),
       plot = plot_comb_resid_TNT07C, width = 10, height = 4)

resid_comb_TNT07C %>% 
  group_by(type) %>% 
  summarise(sd = sd(.resid, na.rm = TRUE))


#################################################

##Ningaloo Reef## 
##BUN05A
##Only annual coral core data

Ningaloo_BUN05A_CCI <- read_csv(here::here("data_raw", "CCI_BUN05A.csv"))
Ningaloo_BUN05A_CCI <- Ningaloo_BUN05A_CCI %>% 
  mutate(sst = as.numeric(str_sub(Ningaloo_BUN05A_CCI$`mean temperature deg C`, 3, 13))) %>%  #remove punctuation [[ and ]] from temperature data
  select(-`mean temperature deg C`) %>%  #remove column with [[ and ]]
  mutate(data_type = "CCI") %>%  #prepare data for binding with other data types
  rename(date = daily_date) %>%  #prepare data for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_BUN05A_CCore <- read_csv(here::here("data_raw", "CCore_Ningaloo_BUN05A_year.csv"))
Ningaloo_BUN05A_CCore <- Ningaloo_BUN05A_CCore %>% 
  mutate(data_type = "Coral Core") %>% #prepare data for binding with other data types
  mutate(month = "01") %>% 
  mutate(date = str_c(year, month, sep = "-")) %>% 
  mutate(date = ym(date)) %>% 
  select(data_type, date, `Bundegi Sr/Ca`) %>% 
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_BUN05A_Logger <- read_csv(here::here("data_raw", "Logger_Avg_Daily_SST_BUNDEGI_BR.csv"))
Ningaloo_BUN05A_Logger <- Ningaloo_BUN05A_Logger %>% 
  rename(sst = mean_SST) %>% #prepare data for binding with other data types
  mutate(data_type = "Logger") %>% #prepare data for binding with other data types
  select(data_type, date, sst) %>%   #select only useful columns
  as_tsibble(index = date) # convert them to tsibble

Ningaloo_BUN05A_NOAA <- read_csv(here::here("data_raw", "NOAA_BUN05A_SST.csv"))
Ningaloo_BUN05A_NOAA <- Ningaloo_BUN05A_NOAA %>% 
  mutate(data_type = "NOAA") %>%  #prepare data for binding with other data types
  select(-date) %>% #remove monthly date column
  rename(date = Date) %>% #rename daily date column to prepare for binding with other data types
  select(data_type, date, sst) %>%  #select only useful columns
  as_tsibble(index = date) # convert them to tsibble


# Running the ARIMA of fable to detrend
M_Ningaloo_BUN05A_CCI <- Ningaloo_BUN05A_CCI %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Ningaloo_BUN05A_CCI_ARIMA <- M_Ningaloo_BUN05A_CCI %>% 
  select(arima)


M1_BUN05A_CCore <- lm(`Bundegi Sr/Ca` ~ 1 +date, Ningaloo_BUN05A_CCore)
augment(M1_BUN05A_CCore) %>% 
  mutate(.model = "arima") %>% 
  select(.model, date, .resid)


Ningaloo_BUN05A_Logger %>% 
  scan_gaps() 
# no gaps
ggplot(Ningaloo_BUN05A_Logger, aes(x = date, y = sst)) + 
  geom_line()


#skip this as there are no gaps
Ningaloo_BUN05A_Logger <- Ningaloo_BUN05A_Logger %>% 
  add_case(Ningaloo_BUN05A_Logger %>% 
             scan_gaps() %>% 
             mutate(sst  = NA)
  ) %>% 
  arrange(date)
#skipped above

M_Ningaloo_BUN05A_Logger <- Ningaloo_BUN05A_Logger %>% 
  model(
    arima = ARIMA(sst ~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Ningaloo_BUN05A_Logger_ARIMA <- M_Ningaloo_BUN05A_Logger %>% 
  select(arima)


M_Ningaloo_BUN05A_NOAA <- Ningaloo_BUN05A_NOAA %>% 
  model(
    arima = ARIMA(sst~ trend(), stepwise = FALSE),
    lin_mod = TSLM(sst ~ trend())
  )

M_Ningaloo_BUN05A_NOAA_ARIMA <- M_Ningaloo_BUN05A_NOAA %>% 
  select(arima)

resid_comb_BUN05A <- bind_rows(
  CCI =
    as_tibble(residuals(M_Ningaloo_BUN05A_CCI_ARIMA, type = "regression")),
  CCore =
    augment(M1_BUN05A_CCore) %>% 
    mutate(.model = "arima") %>% 
    select(.model, date, .resid),
  NOAA = 
    as_tibble(residuals(M_Ningaloo_BUN05A_NOAA_ARIMA, type = "regression")),
  Logger = as_tibble(residuals(M_Ningaloo_BUN05A_Logger_ARIMA, 
                               type = "regression")),
  .id = "type"
) 

plot_comb_resid_BUN05A<- resid_comb_BUN05A %>%
  mutate(
    type = factor(type, levels=c(
      "CCI", "CCore", "NOAA", "Logger"))
  ) %>%
  ggplot(aes(x = date, y = .resid)) +
  geom_line() +
  facet_wrap( ~ type, ncol = 4) + 
  ylab("Regression Residuals") 

ggsave(file = here::here("graphics", 
                         "plot_comb_resid_BUN05A.png"),
       plot = plot_comb_resid_BUN05A, width = 10, height = 4)

resid_comb_BUN05A %>% 
  group_by(type) %>% 
  summarise(sd = sd(.resid, na.rm = TRUE))




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

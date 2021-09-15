# Start script from row 60 #
# Run script in pre_tsclust_sites_raw_data.R before running this to load raw data
# Sites showing significance for quadratic terms in both CCI and NOAA: 13TNT, 08BND, Wallabi Island
# Sites showing significance for quadratic terms in only CCI: BRS07
# For site HAB05B_SrCa, no significance in quadratic terms, 
# HAB05B_SrCa - no significance in linear terms for NOAA but significance for CCI

library(TSclust)
library(mgcv)
library(broom)
        
#####-----#####
library(TSclust)
dat <- as_tibble(Ningaloo_13TNT_CCI) %>% 
  add_row(as_tibble(Ningaloo_13TNT_NOAA)) %>% 
  add_row(as_tibble(Ningaloo_13TNT_Logger)) %>% 
  add_row(as_tibble(Ningaloo_13TNT_CCore) %>% 
            rename(sst = `Sr/Ca`)) %>% 
  mutate(data_type = 
           case_when( data_type == "CCI" ~ "ningaloo_CCI",
                      data_type == "Logger" ~ "ningaloo_Logger", 
                      data_type == "NOAA" ~ "ningaloo_NOAA",
                      data_type == "Coral Core" ~ "ningaloo_coral_core",
                      TRUE ~ data_type))

dat2 <- as_tibble(Browse_05_CCI) %>% 
  add_row(as_tibble(Browse_05_Logger)) %>% 
  add_row(as_tibble(Browse_05_NOAA)) %>% 
  mutate(data_type = 
           case_when( data_type == "CCI" ~ "browse_CCI",
                      data_type == "Logger" ~ "browse_Logger",
                      data_type == "NOAA" ~ "browse_NOAA",
                      TRUE ~ data_type))

# To use the TSclust package, we need each series to be in their own column
# It's important we line the dates up, so we are going to use pivot_wider
dat_joint <- dat %>% 
  #add_row(dat2) %>% 
  pivot_wider(names_from = data_type, values_from = sst) %>% 
  select(-date) %>% 
  drop_na() 
# dropped browse_Logger as it contains the least amount of data

# structure based similarity measure 
# whether the series goes up and down together
tsdist <- diss( t(dat_joint) , "CORT")
names(tsdist) <- colnames(dat_joint)
hc <- hclust(tsdist)
plot(hc)
# I prefer this one. 

# shape based similarity measure 
# whether the series goes are close enough together. 
tsdist2 <- diss( t(dat_joint) , "COR")
names(tsdist2) <- colnames(dat_joint)
hc2 <- hclust(tsdist2)
plot(hc2)


#####--------------------##### 
## To run the calibration analysis
# Need to run another script (Detrended.R files) to get the following tibbles:
# Ningaloo_13TNT_CCI
# Ningaloo_13TNT_Logger
# Ningaloo_13TNT_CCore
# Ningaloo_13TNT_NOAA
#

library(TSclust)
dat <- as_tibble(Ningaloo_13TNT_CCI) %>% 
  add_row(as_tibble(Ningaloo_13TNT_NOAA)) %>% 
  # only 13 data points available if logger data is included in analysis: add_row(as_tibble(Ningaloo_13TNT_Logger)) %>% 
  add_row(as_tibble(Ningaloo_13TNT_CCore) %>% 
            rename(sst = `Sr/Ca`)) %>% 
  mutate(data_type = 
           case_when( data_type == "CCI" ~ "ningaloo_CCI",
                      # only 13 data points if logger data is included: data_type == "Logger" ~ "ningaloo_Logger", 
                      data_type == "NOAA" ~ "ningaloo_NOAA",
                      data_type == "Coral Core" ~ "ningaloo_coral_core",
                      TRUE ~ data_type))

dat_joint <- dat %>% 
  #add_row(dat2) %>% 
  pivot_wider(names_from = data_type, values_from = sst) %>% 
  drop_na() 

GGally::ggpairs(dat_joint)
## coral core is slightly more aligned with NOAA than CCI 
ggplot(dat_joint, aes(x = ningaloo_coral_core, y = ningaloo_NOAA)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend with a slight curvature. 
library(mgcv)
gam1 <- gam(ningaloo_NOAA ~ s(ningaloo_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam1)
summary(gam1)
# edf = 2.677 so linear + quadratic should give a good approx to the trend

lm1 <- lm(ningaloo_NOAA ~ 1 + ningaloo_coral_core + I(ningaloo_coral_core^2), 
          data = dat_joint)
summary(lm1)

# repeat for CCI 
ggplot(dat_joint, aes(x = ningaloo_coral_core, y = ningaloo_CCI)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend with a slight curvature. 
gam2 <- gam(ningaloo_CCI ~ s(ningaloo_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam2)
summary(gam2)
# edf = 2.724 so linear + quadratic should give a good approx to the trend

lm2 <- lm(ningaloo_CCI ~ 1 + ningaloo_coral_core + I(ningaloo_coral_core^2), 
          data = dat_joint)
summary(lm2)

modelsummary::modelsummary(list(lm1, lm2))

# extracting fitted sst values of coral core from CCI and NOAA data
CCore_SST_Ningaloo_13TNT_NOAA <- lm1 %>% 
  augment() %>% 
  rename(`Sr/Ca` = ningaloo_coral_core, sst = .fitted) %>% 
  select(`Sr/Ca`, sst)

CCore_SST_Ningaloo_13TNT_CCI <- lm2 %>% 
  augment() %>% 
  rename(`Sr/Ca` = ningaloo_coral_core, sst = .fitted) %>% 
  select(`Sr/Ca`, sst)

write_csv(CCore_SST_Ningaloo_13TNT_NOAA, here::here
          ("data_raw/CCore_SST_Ningaloo_13TNT_NOAA.csv"))
write_csv(CCore_SST_Ningaloo_13TNT_CCI, here::here
          ("data_raw/CCore_SST_Ningaloo_13TNT_CCI.csv"))

# Coral core is able to explain the variability in NOAA's sst a tiny bit better than CCI for this ningaloo site.  
# Need to think about whether a straight pair-up using the collection date of coral core data with sst is a good idea. 
# Alternative can be averaging the sst data leading up to the 



#####--------------------##### 
## To run the calibration analysis
# Need to run another script (Detrended.R files) to get the following tibbles:
# Ningaloo_08TNT_CCI
# Ningaloo_08TNT_Logger
# Ningaloo_08TNT_CCore
# Ningaloo_08TNT_NOAA
#

library(TSclust)
dat <- as_tibble(Ningaloo_08TNT_CCI) %>% 
  add_row(as_tibble(Ningaloo_08TNT_NOAA)) %>% 
  # no values if logger data is added here: add_row(as_tibble(Ningaloo_08TNT_Logger)) %>% 
  add_row(as_tibble(Ningaloo_08TNT_CCore) %>% 
            rename(sst = `Sr/Ca`)) %>% 
  mutate(data_type = 
           case_when( data_type == "CCI" ~ "ningaloo_CCI",
                      data_type == "Logger" ~ "ningaloo_Logger", 
                      data_type == "NOAA" ~ "ningaloo_NOAA",
                      data_type == "Coral Core" ~ "ningaloo_coral_core",
                      TRUE ~ data_type))

dat_joint <- dat %>% 
  #add_row(dat2) %>% 
  pivot_wider(names_from = data_type, values_from = sst) %>% 
  select(-date) %>% 
  drop_na() 

GGally::ggpairs(dat_joint)
## coral core is slightly more aligned with NOAA over CCI 
ggplot(dat_joint, aes(x = ningaloo_coral_core, y = ningaloo_NOAA)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend 
library(mgcv)
gam1 <- gam(ningaloo_NOAA ~ s(ningaloo_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam1)
summary(gam1)
# edf = 2.477 so linear + quadratic should give a good approx to the trend

lm1 <- lm(ningaloo_NOAA ~ 1 + ningaloo_coral_core + I(ningaloo_coral_core^2), 
          data = dat_joint)
summary(lm1)

# p-value for quadratic is 0.564
# use linear model instead

lm1 <- lm(ningaloo_NOAA ~ 1 + ningaloo_coral_core, 
          data = dat_joint)
summary(lm1)

# repeat for CCI 
ggplot(dat_joint, aes(x = ningaloo_coral_core, y = ningaloo_CCI)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend 
gam2 <- gam(ningaloo_CCI ~ s(ningaloo_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam2)
summary(gam2)
# edf = 2.391 so linear + quadratic should give a good approx to the trend

lm2 <- lm(ningaloo_CCI ~ 1 + ningaloo_coral_core + I(ningaloo_coral_core^2), 
          data = dat_joint)
summary(lm2)

# p-value for quadratic is 0.329
# use linear model instead

lm2 <- lm(ningaloo_CCI ~ 1 + ningaloo_coral_core, 
          data = dat_joint)
summary(lm2)

modelsummary::modelsummary(list(lm1, lm2))

# extracting fitted sst values of coral core from CCI and NOAA data
CCore_SST_Ningaloo_08TNT_NOAA <- lm1 %>% 
  augment() %>% 
  rename(`Sr/Ca` = ningaloo_coral_core, sst = .fitted) %>% 
  select(`Sr/Ca`, sst)

CCore_SST_Ningaloo_08TNT_CCI <- lm2 %>% 
  augment() %>% 
  rename(`Sr/Ca` = ningaloo_coral_core, sst = .fitted) %>% 
  select(`Sr/Ca`, sst)

write_csv(CCore_SST_Ningaloo_08TNT_NOAA, here::here
          ("data_raw/CCore_SST_Ningaloo_08TNT_NOAA.csv"))
write_csv(CCore_SST_Ningaloo_08TNT_CCI, here::here
          ("data_raw/CCore_SST_Ningaloo_08TNT_CCI.csv"))

# Coral core is able to explain the variability in NOAA's sst better than CCI for this ningaloo site.  
# if logger data is added for this site, no values are obtained for the models


#####--------------------##### 
## To run the calibration analysis
# Need to run another script (Detrended.R files) to get the following tibbles:
# Ningaloo_13BND_CCI
# Ningaloo_13BND_Logger
# Ningaloo_13BND_CCore
# Ningaloo_13BND_NOAA
#

library(TSclust)
dat <- as_tibble(Ningaloo_13BND_CCI) %>% 
  add_row(as_tibble(Ningaloo_13BND_NOAA)) %>% 
  # only 10 observations when logger data is added : add_row(as_tibble(Ningaloo_13BND_Logger)) %>%
  add_row(as_tibble(Ningaloo_13BND_CCore) %>% 
            rename(sst = `Sr/Ca`)) %>% 
  mutate(data_type = 
           case_when( data_type == "CCI" ~ "ningaloo_CCI",
                      data_type == "Logger" ~ "ningaloo_Logger", 
                      data_type == "NOAA" ~ "ningaloo_NOAA",
                      data_type == "Coral Core" ~ "ningaloo_coral_core",
                      TRUE ~ data_type))

dat_joint <- dat %>% 
  #add_row(dat2) %>% 
  pivot_wider(names_from = data_type, values_from = sst) %>% 
  select(-date) %>% 
  drop_na() 

GGally::ggpairs(dat_joint)
## coral core is much more aligned with NOAA over CCI 
ggplot(dat_joint, aes(x = ningaloo_coral_core, y = ningaloo_NOAA)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend with a slight curvature for NOAA vs coral core. 
library(mgcv)
gam1 <- gam(ningaloo_NOAA ~ s(ningaloo_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam1)
summary(gam1)
# edf = 2.283 so linear + quadratic should give a good approx to the trend

lm1 <- lm(ningaloo_NOAA ~ 1 + ningaloo_coral_core + I(ningaloo_coral_core^2), 
          data = dat_joint)
summary(lm1)

# p-value for quadratic is 0.195
# use linear model instead

lm1 <- lm(ningaloo_NOAA ~ 1 + ningaloo_coral_core, 
          data = dat_joint)
summary(lm1)

# repeat for CCI 
ggplot(dat_joint, aes(x = ningaloo_coral_core, y = ningaloo_CCI)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend 
gam2 <- gam(ningaloo_CCI ~ s(ningaloo_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam2)
summary(gam2)
# edf = 1.672 so linear + quadratic should give a good approx to the trend

lm2 <- lm(ningaloo_CCI ~ 1 + ningaloo_coral_core + I(ningaloo_coral_core^2), 
          data = dat_joint)
summary(lm2)

# p-value for quadratic is 0.665
# use linear model instead

lm2 <- lm(ningaloo_CCI ~ 1 + ningaloo_coral_core, 
          data = dat_joint)
summary(lm2)

modelsummary::modelsummary(list(lm1, lm2))

# extracting fitted sst values of coral core from CCI and NOAA data
CCore_SST_Ningaloo_13BND_NOAA <- lm1 %>% 
  augment() %>% 
  rename(`Sr/Ca` = ningaloo_coral_core, sst = .fitted) %>% 
  select(`Sr/Ca`, sst)

CCore_SST_Ningaloo_13BND_CCI <- lm2 %>% 
  augment() %>% 
  rename(`Sr/Ca` = ningaloo_coral_core, sst = .fitted) %>% 
  select(`Sr/Ca`, sst)

write_csv(CCore_SST_Ningaloo_13BND_NOAA, here::here
          ("data_raw/CCore_SST_Ningaloo_13BND_NOAA.csv"))
write_csv(CCore_SST_Ningaloo_13BND_CCI, here::here
          ("data_raw/CCore_SST_Ningaloo_13BND_CCI.csv"))

# Coral core is able to explain the variability in NOAA's sst much better than CCI for this ningaloo site. 
## removing logger data in calculations reduces R-sq for CCI model
# only 10 points when logger data is included in analysis

#####--------------------##### 
## To run the calibration analysis
# Need to run another script (Detrended.R files) to get the following tibbles:
# Ningaloo_08BND_CCI
# Ningaloo_08BND_Logger
# Ningaloo_08BND_CCore
# Ningaloo_08BND_NOAA
#

library(TSclust)
dat <- as_tibble(Ningaloo_08BND_CCI) %>% 
  add_row(as_tibble(Ningaloo_08BND_NOAA)) %>% 
  #no values if logger data is included : add_row(as_tibble(Ningaloo_08BND_Logger)) %>% 
  add_row(as_tibble(Ningaloo_08BND_CCore) %>% 
            rename(sst = `Sr/Ca`)) %>% 
  mutate(data_type = 
           case_when( data_type == "CCI" ~ "ningaloo_CCI",
                      data_type == "Logger" ~ "ningaloo_Logger", 
                      data_type == "NOAA" ~ "ningaloo_NOAA",
                      data_type == "Coral Core" ~ "ningaloo_coral_core",
                      TRUE ~ data_type))

dat_joint <- dat %>% 
  #add_row(dat2) %>% 
  pivot_wider(names_from = data_type, values_from = sst) %>% 
  select(-date) %>% 
  drop_na() 

GGally::ggpairs(dat_joint)
## coral core is more aligned with NOAA over CCI 
ggplot(dat_joint, aes(x = ningaloo_coral_core, y = ningaloo_NOAA)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend with curvature. 
library(mgcv)
gam1 <- gam(ningaloo_NOAA ~ s(ningaloo_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam1)
summary(gam1)
# edf = 3.657 so linear + quadratic should give a good approx to the trend

lm1 <- lm(ningaloo_NOAA ~ 1 + ningaloo_coral_core + I(ningaloo_coral_core^2), 
          data = dat_joint)
summary(lm1)

# significant p-value for quadratic

# repeat for CCI 
ggplot(dat_joint, aes(x = ningaloo_coral_core, y = ningaloo_CCI)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend with curvature. 
gam2 <- gam(ningaloo_CCI ~ s(ningaloo_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam2)
summary(gam2)
# edf = 2.88 so linear + quadratic should give a good approx to the trend

lm2 <- lm(ningaloo_CCI ~ 1 + ningaloo_coral_core + I(ningaloo_coral_core^2), 
          data = dat_joint)
summary(lm2)

# significant p-value for quadratic

modelsummary::modelsummary(list(lm1, lm2))

# extracting fitted sst values of coral core from CCI and NOAA data
CCore_SST_Ningaloo_08BND_NOAA <- lm1 %>% 
  augment() %>% 
  rename(`Sr/Ca` = ningaloo_coral_core, sst = .fitted) %>% 
  select(`Sr/Ca`, sst)

CCore_SST_Ningaloo_08BND_CCI <- lm2 %>% 
  augment() %>% 
  rename(`Sr/Ca` = ningaloo_coral_core, sst = .fitted) %>% 
  select(`Sr/Ca`, sst)

write_csv(CCore_SST_Ningaloo_08BND_NOAA, here::here
          ("data_raw/CCore_SST_Ningaloo_08BND_NOAA.csv"))
write_csv(CCore_SST_Ningaloo_08BND_CCI, here::here
          ("data_raw/CCore_SST_Ningaloo_08BND_CCI.csv"))

# Coral core is able to explain the variability in NOAA's sst better than CCI for this ningaloo site. 
# including logger data in calculations result in no data available

#####--------------------##### 
## To run the calibration analysis
# Need to run another script (Detrended.R files) to get the following tibbles:
# Ningaloo_TNT_CCI
# Ningaloo_TNT_Logger
# Ningaloo_TNT_CCore
# Ningaloo_TNT_NOAA
#

library(TSclust)
dat <- as_tibble(Ningaloo_TNT_CCI) %>% 
  add_row(as_tibble(Ningaloo_TNT_NOAA)) %>% 
  # no data is available when logger data is added: add_row(as_tibble(Ningaloo_TNT_Logger)) %>% 
  add_row(as_tibble(Ningaloo_TNT_CCore) %>% 
            rename(sst = `d18O`)) %>% 
  mutate(data_type = 
           case_when( data_type == "CCI" ~ "ningaloo_CCI",
                      data_type == "Logger" ~ "ningaloo_Logger", 
                      data_type == "NOAA" ~ "ningaloo_NOAA",
                      data_type == "Coral Core" ~ "ningaloo_coral_core",
                      TRUE ~ data_type))

dat_joint <- dat %>% 
  #add_row(dat2) %>% 
  pivot_wider(names_from = data_type, values_from = sst) %>% 
  select(-date) %>% 
  drop_na() 

GGally::ggpairs(dat_joint)
## coral core is slightly more aligned with CCI than NOAA
ggplot(dat_joint, aes(x = ningaloo_coral_core, y = ningaloo_NOAA)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend with a slight curvature. 
library(mgcv)
gam1 <- gam(ningaloo_NOAA ~ s(ningaloo_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam1)
summary(gam1)
# edf = 3.138 so linear + quadratic should give a good approx to the trend

lm1 <- lm(ningaloo_NOAA ~ 1 + ningaloo_coral_core + I(ningaloo_coral_core^2), 
          data = dat_joint)
summary(lm1)

# p-value for quadratic is 0.160
# use linear model instead

lm1 <- lm(ningaloo_NOAA ~ 1 + ningaloo_coral_core, 
          data = dat_joint)
summary(lm1)

# repeat for CCI 
ggplot(dat_joint, aes(x = ningaloo_coral_core, y = ningaloo_CCI)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend with a slight curvature. 
gam2 <- gam(ningaloo_CCI ~ s(ningaloo_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam2)
summary(gam2)
# edf = 3.056 so linear + quadratic should give a good approx to the trend

lm2 <- lm(ningaloo_CCI ~ 1 + ningaloo_coral_core + I(ningaloo_coral_core^2), 
          data = dat_joint)
summary(lm2)

# p-value for quadratic is 0.258
# use linear model instead

lm2 <- lm(ningaloo_CCI ~ 1 + ningaloo_coral_core, 
          data = dat_joint)
summary(lm2)

modelsummary::modelsummary(list(lm1, lm2))

# extracting fitted sst values of coral core from CCI and NOAA data
CCore_SST_Ningaloo_TNT_NOAA <- lm1 %>% 
  augment() %>% 
  rename(`d18O` = ningaloo_coral_core, sst = .fitted) %>% 
  select(`d18O`, sst)

CCore_SST_Ningaloo_TNT_CCI <- lm2 %>% 
  augment() %>% 
  rename(`d18O` = ningaloo_coral_core, sst = .fitted) %>% 
  select(`d18O`, sst)

write_csv(CCore_SST_Ningaloo_TNT_NOAA, here::here
          ("data_raw/CCore_SST_Ningaloo_TNT_NOAA.csv"))
write_csv(CCore_SST_Ningaloo_TNT_CCI, here::here
          ("data_raw/CCore_SST_Ningaloo_TNT_CCI.csv"))

# Coral core is able to explain the variability in CCI's sst a tiny bit better than NOAA for this ningaloo site.  
# no data is available for analysis when logger data is included

#####--------------------##### 
## To run the calibration analysis
# Need to run another script (Detrended.R files) to get the following tibbles:
# Ningaloo_TNT07C_CCI
# Ningaloo_TNT07C_Logger
# Ningaloo_TNT07C_CCore
# Ningaloo_TNT07C_NOAA
#

library(TSclust)
dat <- as_tibble(Ningaloo_TNT07C_CCI) %>% 
  add_row(as_tibble(Ningaloo_TNT07C_NOAA)) %>% 
  # no data is available when logger data is added: add_row(as_tibble(Ningaloo_TNT07C_Logger)) %>% 
  add_row(as_tibble(Ningaloo_TNT07C_CCore) %>% 
            rename(sst = `Tantabiddi Sr/Ca`)) %>% 
  mutate(data_type = 
           case_when( data_type == "CCI" ~ "ningaloo_CCI",
                      data_type == "Logger" ~ "ningaloo_Logger", 
                      data_type == "NOAA" ~ "ningaloo_NOAA",
                      data_type == "Coral Core" ~ "ningaloo_coral_core",
                      TRUE ~ data_type))

dat_joint <- dat %>% 
  #add_row(dat2) %>% 
  pivot_wider(names_from = data_type, values_from = sst) %>% 
  select(-date) %>% 
  drop_na() 

GGally::ggpairs(dat_joint)
## coral core is slightly more aligned with NOAA over CCI 
ggplot(dat_joint, aes(x = ningaloo_coral_core, y = ningaloo_NOAA)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend 
library(mgcv)
gam1 <- gam(ningaloo_NOAA ~ s(ningaloo_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam1)
summary(gam1)
# edf = 1.237 so linear + quadratic should give a good approx to the trend

lm1 <- lm(ningaloo_NOAA ~ 1 + ningaloo_coral_core + I(ningaloo_coral_core^2), 
          data = dat_joint)
summary(lm1)

# p-value for quadratic is 0.312
# use linear model

lm1 <- lm(ningaloo_NOAA ~ 1 + ningaloo_coral_core, 
          data = dat_joint)
summary(lm1)

# repeat for CCI 
ggplot(dat_joint, aes(x = ningaloo_coral_core, y = ningaloo_CCI)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend 
gam2 <- gam(ningaloo_CCI ~ s(ningaloo_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam2)
summary(gam2)
# edf = 1.139 so linear + quadratic should give a good approx to the trend

lm2 <- lm(ningaloo_CCI ~ 1 + ningaloo_coral_core + I(ningaloo_coral_core^2), 
          data = dat_joint)
summary(lm2)

# p-value for quadratic is 0.726
# use linear model

lm2 <- lm(ningaloo_CCI ~ 1 + ningaloo_coral_core, 
          data = dat_joint)
summary(lm2)

modelsummary::modelsummary(list(lm1, lm2))

# extracting fitted sst values of coral core from CCI and NOAA data
CCore_SST_Ningaloo_TNT07C_NOAA <- lm1 %>% 
  augment() %>% 
  rename(`Sr/Ca` = ningaloo_coral_core, sst = .fitted) %>% 
  select(`Sr/Ca`, sst)

CCore_SST_Ningaloo_TNT07C_CCI <- lm2 %>% 
  augment() %>% 
  rename(`Sr/Ca` = ningaloo_coral_core, sst = .fitted) %>% 
  select(`Sr/Ca`, sst)

write_csv(CCore_SST_Ningaloo_TNT07C_NOAA, here::here
          ("data_raw/CCore_SST_Ningaloo_TNT07C_NOAA.csv"))
write_csv(CCore_SST_Ningaloo_TNT07C_CCI, here::here
          ("data_raw/CCore_SST_Ningaloo_TNT07C_CCI.csv"))

# Coral core is able to explain the variability in NOAA's sst better than CCI for this ningaloo site.  
# low R-sq value for both <0.5
# no data is available for analysis when logger data is included

#####--------------------##### 
## To run the calibration analysis
# Need to run another script (Detrended.R files) to get the following tibbles:
# Ningaloo_BUN05A_CCI
# Ningaloo_BUN05A_Logger
# Ningaloo_BUN05A_CCore
# Ningaloo_BUN05A_NOAA
#

library(TSclust)
dat <- as_tibble(Ningaloo_BUN05A_CCI) %>% 
  add_row(as_tibble(Ningaloo_BUN05A_NOAA)) %>% 
  # no data is available when logger data is added: add_row(as_tibble(Ningaloo_BUN05A_Logger)) %>% 
  add_row(as_tibble(Ningaloo_BUN05A_CCore) %>% 
            rename(sst = `Bundegi Sr/Ca`)) %>% 
  mutate(data_type = 
           case_when( data_type == "CCI" ~ "ningaloo_CCI",
                      data_type == "Logger" ~ "ningaloo_Logger", 
                      data_type == "NOAA" ~ "ningaloo_NOAA",
                      data_type == "Coral Core" ~ "ningaloo_coral_core",
                      TRUE ~ data_type))

dat_joint <- dat %>% 
  #add_row(dat2) %>% 
  pivot_wider(names_from = data_type, values_from = sst) %>% 
  select(-date) %>% 
  drop_na() 

GGally::ggpairs(dat_joint)
## coral core is more aligned with CCI than NOAA 
ggplot(dat_joint, aes(x = ningaloo_coral_core, y = ningaloo_NOAA)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend 
library(mgcv)
gam1 <- gam(ningaloo_NOAA ~ s(ningaloo_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam1)
summary(gam1)
# edf = 1.032 so quadratic might not give a good approx to the trend

lm1 <- lm(ningaloo_NOAA ~ 1 + ningaloo_coral_core + I(ningaloo_coral_core^2), 
          data = dat_joint)
summary(lm1)

# p-value for quadratic is 0.610
# use linear model

lm1 <- lm(ningaloo_NOAA ~ 1 + ningaloo_coral_core, 
          data = dat_joint)
summary(lm1)

# repeat for CCI 
ggplot(dat_joint, aes(x = ningaloo_coral_core, y = ningaloo_CCI)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend 
gam2 <- gam(ningaloo_CCI ~ s(ningaloo_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam2)
summary(gam2)
# edf = 1.221 so linear + quadratic should give a good approx to the trend

lm2 <- lm(ningaloo_CCI ~ 1 + ningaloo_coral_core + I(ningaloo_coral_core^2), 
          data = dat_joint)
summary(lm2)

# p-value for quadratic is 0.816
# use linear model

lm2 <- lm(ningaloo_CCI ~ 1 + ningaloo_coral_core, 
          data = dat_joint)
summary(lm2)

modelsummary::modelsummary(list(lm1, lm2))

# extracting fitted sst values of coral core from CCI and NOAA data
CCore_SST_Ningaloo_BUN05A_NOAA <- lm1 %>% 
  augment() %>% 
  rename(`Sr/Ca` = ningaloo_coral_core, sst = .fitted) %>% 
  select(`Sr/Ca`, sst)

CCore_SST_Ningaloo_BUN05A_CCI <- lm2 %>% 
  augment() %>% 
  rename(`Sr/Ca` = ningaloo_coral_core, sst = .fitted) %>% 
  select(`Sr/Ca`, sst)

write_csv(CCore_SST_Ningaloo_BUN05A_NOAA, here::here
          ("data_raw/CCore_SST_Ningaloo_BUN05A_NOAA.csv"))
write_csv(CCore_SST_Ningaloo_BUN05A_CCI, here::here
          ("data_raw/CCore_SST_Ningaloo_BUN05A_CCI.csv"))

# Coral core is able to explain the variability in CCI's sst much better than NOAA for this ningaloo site.  
# low R-sq value for both <0.5
# no data is available for analysis when logger data is included


#####-----#####
#####--------------------#####
## To run the calibration analysis
# Need to run another script (Detrended.R files) to get the following tibbles:
# Browse_05_CCI
# Browse_05_Logger
# Browse_05_CCore
# Browse_05_NOAA
#

library(TSclust)
dat <- as_tibble(Browse_05_CCI) %>% 
  add_row(as_tibble(Browse_05_NOAA)) %>% 
  # only few data points when logger data is included: add_row(as_tibble(Browse_05_Logger)) %>% 
  add_row(as_tibble(Browse_05_CCore) %>% 
            rename(sst = `BRS05 Sr/Ca [mmol/mol]`)) %>% 
  mutate(data_type = 
           case_when( data_type == "CCI" ~ "browse_CCI",
                      data_type == "Logger" ~ "browse_Logger", 
                      data_type == "NOAA" ~ "browse_NOAA",
                      data_type == "Coral Core" ~ "browse_coral_core",
                      TRUE ~ data_type))

dat_joint <- dat %>% 
  #add_row(dat2) %>% 
  pivot_wider(names_from = data_type, values_from = sst) %>% 
  select(-date) %>% 
  drop_na() 

GGally::ggpairs(dat_joint)
## coral core is slightly more aligned with CCI than NOAA
ggplot(dat_joint, aes(x = browse_coral_core, y = browse_NOAA)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# decreasing trend with curvature. 
library(mgcv)
gam1 <- gam(browse_NOAA ~ s(browse_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam1)
summary(gam1)
# edf = 3.841 so linear + quadratic should give a good approx to the trend

lm1 <- lm(browse_NOAA ~ 1 + browse_coral_core + I(browse_coral_core^2), 
          data = dat_joint)
summary(lm1)

# p-value for quadratic is 0.175
# use linear model

lm1 <- lm(browse_NOAA ~ 1 + browse_coral_core, 
          data = dat_joint)
summary(lm1)

# repeat for CCI 
ggplot(dat_joint, aes(x = browse_coral_core, y = browse_CCI)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend with a slight curvature. 
gam2 <- gam(browse_CCI ~ s(browse_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam2)
summary(gam2)
# edf = 3.727 so linear + quadratic should give a good approx to the trend

lm2 <- lm(browse_CCI ~ 1 + browse_coral_core + I(browse_coral_core^2), 
          data = dat_joint)
summary(lm2)

# p-value for quadratic is 0.204
# use linear model

lm2 <- lm(browse_CCI ~ 1 + browse_coral_core, 
          data = dat_joint)
summary(lm2)


modelsummary::modelsummary(list(lm1, lm2))

# extracting fitted sst values of coral core from CCI and NOAA data
CCore_SST_BRS05_NOAA <- lm1 %>% 
  augment() %>% 
  rename(`Sr/Ca` = browse_coral_core, sst = .fitted) %>% 
  select(`Sr/Ca`, sst)

CCore_SST_BRS05_CCI <- lm2 %>% 
  augment() %>% 
  rename(`Sr/Ca` = browse_coral_core, sst = .fitted) %>% 
  select(`Sr/Ca`, sst)

write_csv(CCore_SST_BRS05_NOAA, here::here
          ("data_raw/CCore_SST_BRS05_NOAA.csv"))
write_csv(CCore_SST_BRS05_CCI, here::here
          ("data_raw/CCore_SST_BRS05_CCI.csv"))

# Coral core is able to explain the variability in CCI's sst much better than NOAA for this browse island site.  
# R-sq value is much higher when logger data is included
# wayyyy more data points when logger data is excluded, but lower R-sq

#####--------------------#####
## To run the calibration analysis
# Need to run another script (Detrended.R files) to get the following tibbles:
# Browse_07_CCI
# Browse_07_Logger
# Browse_07_CCore
# Browse_07_NOAA
#

library(TSclust)
dat <- as_tibble(Browse_07_CCI) %>% 
  add_row(as_tibble(Browse_07_NOAA)) %>% 
  # only 9 points are available when including logger data: add_row(as_tibble(Browse_07_Logger)) %>% 
  add_row(as_tibble(Browse_07_CCore) %>% 
            rename(sst = `BRS07 Sr/Ca [mmol/mol]`)) %>% 
  mutate(data_type = 
           case_when( data_type == "CCI" ~ "browse_CCI",
                      data_type == "Logger" ~ "browse_Logger", 
                      data_type == "NOAA" ~ "browse_NOAA",
                      data_type == "Coral Core" ~ "browse_coral_core",
                      TRUE ~ data_type))

dat_joint <- dat %>% 
  #add_row(dat2) %>% 
  pivot_wider(names_from = data_type, values_from = sst) %>% 
  select(-date) %>% 
  drop_na() 

GGally::ggpairs(dat_joint)
## coral core is slightly more aligned with CCI than NOAA 
ggplot(dat_joint, aes(x = browse_coral_core, y = browse_NOAA)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend with a slight curvature. 
library(mgcv)
gam1 <- gam(browse_NOAA ~ s(browse_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam1)
summary(gam1)
# edf = 3.039 so linear + quadratic should give a good approx to the trend

lm1 <- lm(browse_NOAA ~ 1 + browse_coral_core + I(browse_coral_core^2), 
          data = dat_joint)
summary(lm1)

# p-value for quadratic is 0.146
# use linear model

lm1 <- lm(browse_NOAA ~ 1 + browse_coral_core, 
          data = dat_joint)
summary(lm1)

# repeat for CCI 
ggplot(dat_joint, aes(x = browse_coral_core, y = browse_CCI)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend with a slight curvature. 
gam2 <- gam(browse_CCI ~ s(browse_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam2)
summary(gam2)
# edf = 3.166 so linear + quadratic should give a good approx to the trend

lm2 <- lm(browse_CCI ~ 1 + browse_coral_core + I(browse_coral_core^2), 
          data = dat_joint)
summary(lm2)

# p-value for quadratic is significant, keep quadratic model

modelsummary::modelsummary(list(lm1, lm2))

# extracting fitted sst values of coral core from CCI and NOAA data
CCore_SST_BRS07_NOAA <- lm1 %>% 
  augment() %>% 
  rename(`Sr/Ca` = browse_coral_core, sst = .fitted) %>% 
  select(`Sr/Ca`, sst)

CCore_SST_BRS07_CCI <- lm2 %>% 
  augment() %>% 
  rename(`Sr/Ca` = browse_coral_core, sst = .fitted) %>% 
  select(`Sr/Ca`, sst)

write_csv(CCore_SST_BRS07_NOAA, here::here
          ("data_raw/CCore_SST_BRS07_NOAA.csv"))
write_csv(CCore_SST_BRS07_CCI, here::here
          ("data_raw/CCore_SST_BRS07_CCI.csv"))

# Coral core is able to explain the variability in CCI's sst much better than NOAA for this browse island site.  
# GAM does not work when logger data is included, as there are only 9 points

#####-----#####

#####--------------------#####
## To run the calibration analysis
# Need to run another script (Detrended.R files) to get the following tibbles:
# DARL_CCI
# DARL_Logger
# DARL_CCore
# DARL_NOAA
#

library(TSclust)
dat <- as_tibble(DARL_CCI) %>% 
  add_row(as_tibble(DARL_NOAA)) %>% 
  #less than 10 points if logger data is included : add_row(as_tibble(DARL_Logger)) %>% 
  add_row(as_tibble(DARL_CCore) %>% 
            rename(sst = `Sr/Ca`)) %>% 
  mutate(data_type = 
           case_when( data_type == "CCI" ~ "Cocos_CCI",
                      data_type == "Logger" ~ "Cocos_Logger", 
                      data_type == "NOAA" ~ "Cocos_NOAA",
                      data_type == "Coral Core" ~ "Cocos_coral_core",
                      TRUE ~ data_type))

dat_joint <- dat %>% 
  #add_row(dat2) %>% 
  pivot_wider(names_from = data_type, values_from = sst) %>% 
  select(-date) %>% 
  drop_na() 

GGally::ggpairs(dat_joint)
## coral core is slightly more aligned with CCI than NOAA 
ggplot(dat_joint, aes(x = Cocos_coral_core, y = Cocos_NOAA)) + 
  geom_point() + 
  geom_smooth(method = "gam")

# linear decreasing trend

library(mgcv)
gam1 <- gam(Cocos_NOAA ~ s(Cocos_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam1)
summary(gam1)
# edf = 1.881 so linear + quadratic should give a good approx to the trend

lm1 <- lm(Cocos_NOAA ~ 1 + Cocos_coral_core + I(Cocos_coral_core^2), 
          data = dat_joint)
summary(lm1)

# p-value for quadratic is 0.322
# use linear model

lm1 <- lm(Cocos_NOAA ~ 1 + Cocos_coral_core, 
          data = dat_joint)
summary(lm1)

# repeat for CCI 
ggplot(dat_joint, aes(x = Cocos_coral_core, y = Cocos_CCI)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend 
gam2 <- gam(Cocos_CCI ~ s(Cocos_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam2)
summary(gam2)
# edf = 1.969 so linear + quadratic should give a good approx to the trend

lm2 <- lm(Cocos_CCI ~ 1 + Cocos_coral_core + I(Cocos_coral_core^2), 
          data = dat_joint)
summary(lm2)

# p-value for quadratic is 0.506
# use linear model

lm2 <- lm(Cocos_CCI ~ 1 + Cocos_coral_core, 
          data = dat_joint)
summary(lm2)

modelsummary::modelsummary(list(lm1, lm2))

# extracting fitted sst values of coral core from CCI and NOAA data
CCore_SST_DARL_NOAA <- lm1 %>% 
  augment() %>% 
  rename(`Sr/Ca` = Cocos_coral_core, sst = .fitted) %>% 
  select(`Sr/Ca`, sst)

CCore_SST_DARL_CCI <- lm2 %>% 
  augment() %>% 
  rename(`Sr/Ca` = Cocos_coral_core, sst = .fitted) %>% 
  select(`Sr/Ca`, sst)

write_csv(CCore_SST_DARL_NOAA, here::here
          ("data_raw/CCore_SST_DARL_NOAA.csv"))
write_csv(CCore_SST_DARL_CCI, here::here
          ("data_raw/CCore_SST_DARL_CCI.csv"))

# Coral core is able to explain the variability in CCI's sst better than NOAA for this Cocos (Keeling) island site.  
# GAM does not work when logger data is included, as there are less than 10 points
#R-sq values are very low



#####--------------------#####
## To run the calibration analysis
# Need to run another script (Detrended.R files) to get the following tibbles:
# DARL_CCI
# DARL_Logger
# DARL_CCore
# DARL_NOAA
#

library(TSclust)
dat <- as_tibble(DAR3_CCI) %>% 
  add_row(as_tibble(DAR3_NOAA)) %>% 
  #less than 10 points if logger data is included : add_row(as_tibble(DAR3_Logger)) %>% 
  add_row(as_tibble(DAR3_CCore) %>% 
            rename(sst = `Sr/Ca`)) %>% 
  mutate(data_type = 
           case_when( data_type == "CCI" ~ "Cocos_CCI",
                      data_type == "Logger" ~ "Cocos_Logger", 
                      data_type == "NOAA" ~ "Cocos_NOAA",
                      data_type == "Coral Core" ~ "Cocos_coral_core",
                      TRUE ~ data_type))

dat_joint <- dat %>% 
  #add_row(dat2) %>% 
  pivot_wider(names_from = data_type, values_from = sst) %>% 
  select(-date) %>% 
  drop_na() 

GGally::ggpairs(dat_joint)
## coral core is slightly more align with NOAA over CCI 
ggplot(dat_joint, aes(x = Cocos_coral_core, y = Cocos_NOAA)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend with a slight curvature. 
library(mgcv)
gam1 <- gam(Cocos_NOAA ~ s(Cocos_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam1)
summary(gam1)
# edf = 1.881 so linear + quadratic should give a good approx to the trend

lm1 <- lm(Cocos_NOAA ~ 1 + Cocos_coral_core + I(Cocos_coral_core^2), 
          data = dat_joint)
summary(lm1)

# p-value for quadratic is 0.322
# use linear model

lm1 <- lm(Cocos_NOAA ~ 1 + Cocos_coral_core, 
          data = dat_joint)
summary(lm1)

# repeat for CCI 
ggplot(dat_joint, aes(x = Cocos_coral_core, y = Cocos_CCI)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend with a slight curvature. 
gam2 <- gam(Cocos_CCI ~ s(Cocos_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam2)
summary(gam2)
# edf = 1.969 so linear + quadratic should give a good approx to the trend

lm2 <- lm(Cocos_CCI ~ 1 + Cocos_coral_core + I(Cocos_coral_core^2), 
          data = dat_joint)
summary(lm2)

# p-value for quadratic is 0.506
# use linear model

modelsummary::modelsummary(list(lm1, lm2))

# extracting fitted sst values of coral core from CCI and NOAA data
CCore_SST_DAR3_NOAA <- lm1 %>% 
  augment() %>% 
  rename(`Sr/Ca` = Cocos_coral_core, sst = .fitted) %>% 
  select(`Sr/Ca`, sst)

CCore_SST_DAR3_CCI <- lm2 %>% 
  augment() %>% 
  rename(`Sr/Ca` = Cocos_coral_core, sst = .fitted) %>% 
  select(`Sr/Ca`, sst)

write_csv(CCore_SST_DAR3_NOAA, here::here
          ("data_raw/CCore_SST_DAR3_NOAA.csv"))
write_csv(CCore_SST_DAR3_CCI, here::here
          ("data_raw/CCore_SST_DAR3_CCI.csv"))

# Coral core is able to explain the variability in CCI's sst better than NOAA for this Cocos (Keeling) island site.  
# GAM does not work when logger data is included, as there are less than 10 points
#R-sq values are very low


#####-----#####

#####--------------------#####
## To run the calibration analysis
# Need to run another script (Detrended.R files) to get the following tibbles:
# Wallabi_Island_CCI
# Wallabi_Island_Logger
# Wallabi_Island_CCore
# Wallabi_Island_NOAA
#

library(TSclust)
dat <- as_tibble(Wallabi_Island_CCI) %>% 
  add_row(as_tibble(Wallabi_Island_NOAA)) %>% 
  # no available data if logger data is included : add_row(as_tibble(Wallabi_Island_Logger)) %>% 
  add_row(as_tibble(Wallabi_Island_CCore) %>% 
            rename(sst = `d18O`)) %>% 
  mutate(data_type = 
           case_when( data_type == "CCI" ~ "HAbrol_CCI",
                      data_type == "Logger" ~ "HAbrol_Logger", 
                      data_type == "NOAA" ~ "HAbrol_NOAA",
                      data_type == "Coral Core" ~ "HAbrol_coral_core",
                      TRUE ~ data_type))

dat_joint <- dat %>% 
  #add_row(dat2) %>% 
  pivot_wider(names_from = data_type, values_from = sst) %>% 
  select(-date) %>% 
  drop_na() 

GGally::ggpairs(dat_joint)
## coral core is slightly more aligned with CCI than NOAA 
ggplot(dat_joint, aes(x = HAbrol_coral_core, y = HAbrol_NOAA)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# trend is explained by a curvature. 
library(mgcv)
gam1 <- gam(HAbrol_NOAA ~ s(HAbrol_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam1)
summary(gam1)
# edf = 3.398 so linear + quadratic should give a good approx to the trend

lm1 <- lm(HAbrol_NOAA ~ 1 + HAbrol_coral_core + I(HAbrol_coral_core^2), 
          data = dat_joint)
summary(lm1)

# p-value for quadratic is significant

# repeat for CCI 
ggplot(dat_joint, aes(x = HAbrol_coral_core, y = HAbrol_CCI)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# trend is explained by curvature. 
gam2 <- gam(HAbrol_CCI ~ s(HAbrol_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam2)
summary(gam2)
# edf = 2.975 so linear + quadratic should give a good approx to the trend

lm2 <- lm(HAbrol_CCI ~ 1 + HAbrol_coral_core + I(HAbrol_coral_core^2), 
          data = dat_joint)
summary(lm2)

# p-value for quadratic is significant

modelsummary::modelsummary(list(lm1, lm2))

# extracting fitted sst values of coral core from CCI and NOAA data
CCore_SST_Wallabi_Island_NOAA <- lm1 %>% 
  augment() %>% 
  rename(`d18O` = HAbrol_coral_core, sst = .fitted) %>% 
  select(`d18O`, sst)

CCore_SST_Wallabi_Island_CCI <- lm2 %>% 
  augment() %>% 
  rename(`d18O` = HAbrol_coral_core, sst = .fitted) %>% 
  select(`d18O`, sst)

write_csv(CCore_SST_Wallabi_Island_NOAA, here::here
          ("data_raw/CCore_SST_Wallabi_Island_NOAA.csv"))
write_csv(CCore_SST_Wallabi_Island_CCI, here::here
          ("data_raw/CCore_SST_Wallabi_Island_CCI.csv"))

# Coral core is able to explain the variability in CCI's sst better than NOAA for this Houtman Abrolhos island site.  
# No data is available when logger data is included
#R-sq values are very low <0.5
#####--------------------#####
## To run the calibration analysis
# Need to run another script (Detrended.R files) to get the following tibbles:
# HAB10A_d18O_CCI
# HAB10A_d18O_Logger
# HAB10A_d18O_CCore
# HAB10A_d18O_NOAA
#

library(TSclust)
dat <- as_tibble(HAB10A_d18O_CCI) %>% 
  add_row(as_tibble(HAB10A_d18O_NOAA)) %>% 
  # no available data if logger data is included : add_row(as_tibble(HAB10A_d18O_Logger)) %>% 
  add_row(as_tibble(HAB10A_d18O_CCore) %>% 
            rename(sst = `HAB10A d18O`)) %>% 
  mutate(data_type = 
           case_when( data_type == "CCI" ~ "HAbrol_CCI",
                      data_type == "Logger" ~ "HAbrol_Logger", 
                      data_type == "NOAA" ~ "HAbrol_NOAA",
                      data_type == "Coral Core" ~ "HAbrol_coral_core",
                      TRUE ~ data_type))

dat_joint <- dat %>% 
  #add_row(dat2) %>% 
  pivot_wider(names_from = data_type, values_from = sst) %>% 
  select(-date) %>% 
  drop_na() 

GGally::ggpairs(dat_joint)
## coral core is slightly more aligned with NOAA than CCI 
ggplot(dat_joint, aes(x = HAbrol_coral_core, y = HAbrol_NOAA)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend with a slight curvature. 
library(mgcv)
gam1 <- gam(HAbrol_NOAA ~ s(HAbrol_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam1)
summary(gam1)
# edf = 1.16 so linear + quadratic should give a good approx to the trend

lm1 <- lm(HAbrol_NOAA ~ 1 + HAbrol_coral_core + I(HAbrol_coral_core^2), 
          data = dat_joint)
summary(lm1)

# p-value for quadratic is 0.0657
# use linear model

lm1 <- lm(HAbrol_NOAA ~ 1 + HAbrol_coral_core, 
          data = dat_joint)
summary(lm1)

# repeat for CCI 
ggplot(dat_joint, aes(x = HAbrol_coral_core, y = HAbrol_CCI)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend with a slight curvature. 
gam2 <- gam(HAbrol_CCI ~ s(HAbrol_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam2)
summary(gam2)
# edf = 0.9356 so quadratic model might not give a good approx to the trend

lm2 <- lm(HAbrol_CCI ~ 1 + HAbrol_coral_core + I(HAbrol_coral_core^2), 
          data = dat_joint)
summary(lm2)

# p-value for quadratic is 0.272
# use linear model

lm2 <- lm(HAbrol_CCI ~ 1 + HAbrol_coral_core, 
          data = dat_joint)
summary(lm2)

modelsummary::modelsummary(list(lm1, lm2))

# extracting fitted sst values of coral core from CCI and NOAA data
CCore_SST_HAB10A_d18O_NOAA <- lm1 %>% 
  augment() %>% 
  rename(`d18O` = HAbrol_coral_core, sst = .fitted) %>% 
  select(`d18O`, sst)

CCore_SST_HAB10A_d18O_CCI <- lm2 %>% 
  augment() %>% 
  rename(`d18O` = HAbrol_coral_core, sst = .fitted) %>% 
  select(`d18O`, sst)

write_csv(CCore_SST_HAB10A_d18O_NOAA, here::here
          ("data_raw/CCore_SST_HAB10A_d18O_NOAA.csv"))
write_csv(CCore_SST_HAB10A_d18O_CCI, here::here
          ("data_raw/CCore_SST_HAB10A_d18O_CCI.csv"))

# Coral core is able to explain the variability in NOAA's sst better than CCI for this Houtman Abrolhos island site.  
# No data is available when logger data is included
#R-sq values are very low <0.5

#####--------------------#####
## To run the calibration analysis
# Need to run another script (Detrended.R files) to get the following tibbles:
# HAB05B_d18O_CCI
# HAB05B_d18O_Logger
# HAB05B_d18O_CCore
# HAB05B_d18O_NOAA
#

library(TSclust)
dat <- as_tibble(HAB05B_d18O_CCI) %>% 
  add_row(as_tibble(HAB05B_d18O_NOAA)) %>% 
  # no available data if logger data is included : add_row(as_tibble(HAB05B_d18O_Logger)) %>% 
  add_row(as_tibble(HAB05B_d18O_CCore) %>% 
            rename(sst = `HAB05B d18O`)) %>% 
  mutate(data_type = 
           case_when( data_type == "CCI" ~ "HAbrol_CCI",
                      data_type == "Logger" ~ "HAbrol_Logger", 
                      data_type == "NOAA" ~ "HAbrol_NOAA",
                      data_type == "Coral Core" ~ "HAbrol_coral_core",
                      TRUE ~ data_type))

dat_joint <- dat %>% 
  #add_row(dat2) %>% 
  pivot_wider(names_from = data_type, values_from = sst) %>% 
  select(-date) %>% 
  drop_na() 

GGally::ggpairs(dat_joint)
## only 9 points, unable to support GAM

lm1 <- lm(HAbrol_NOAA ~ 1 + HAbrol_coral_core, 
          data = dat_joint)
summary(lm1)

#p-value not significant for linear model

# repeat for CCI 
## only 9 points, unable to support GAM

lm2 <- lm(HAbrol_CCI ~ 1 + HAbrol_coral_core, 
          data = dat_joint)
summary(lm2)

# p-value is 0.0925, not significant for linear model


modelsummary::modelsummary(list(lm1, lm2))


#####--------------------#####
## To run the calibration analysis
# Need to run another script (Detrended.R files) to get the following tibbles:
# HAB10A_SrCa_CCI
# HAB10A_SrCa_Logger
# HAB10A_SrCa_CCore
# HAB10A_SrCa_NOAA
#

library(TSclust)
dat <- as_tibble(HAB10A_SrCa_CCI) %>% 
  add_row(as_tibble(HAB10A_SrCa_NOAA)) %>% 
  # no available data if logger data is included : add_row(as_tibble(HAB10A_SrCa_Logger)) %>% 
  add_row(as_tibble(HAB10A_SrCa_CCore) %>% 
            rename(sst = `HAB10A Sr/Ca`)) %>% 
  mutate(data_type = 
           case_when( data_type == "CCI" ~ "HAbrol_CCI",
                      data_type == "Logger" ~ "HAbrol_Logger", 
                      data_type == "NOAA" ~ "HAbrol_NOAA",
                      data_type == "Coral Core" ~ "HAbrol_coral_core",
                      TRUE ~ data_type))

dat_joint <- dat %>% 
  #add_row(dat2) %>% 
  pivot_wider(names_from = data_type, values_from = sst) %>% 
  select(-date) %>% 
  drop_na() 

GGally::ggpairs(dat_joint)
## coral core is more aligned with CCI than NOAA 
ggplot(dat_joint, aes(x = HAbrol_coral_core, y = HAbrol_NOAA)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# trend is linear and seems to be decreasing slightly
library(mgcv)
gam1 <- gam(HAbrol_NOAA ~ s(HAbrol_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam1)
summary(gam1)
# edf is low, only 0.2038!!

lm1 <- lm(HAbrol_NOAA ~ 1 + HAbrol_coral_core + I(HAbrol_coral_core^2), 
          data = dat_joint)
summary(lm1)

# p-value for quadratic is 0.538
# use linear model

lm1 <- lm(HAbrol_NOAA ~ 1 + HAbrol_coral_core, 
          data = dat_joint)
summary(lm1)

# p-value for linear model is not significant as well

# repeat for CCI 
ggplot(dat_joint, aes(x = HAbrol_coral_core, y = HAbrol_CCI)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend with a slight curvature. 
gam2 <- gam(HAbrol_CCI ~ s(HAbrol_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam2)
summary(gam2)
# edf = 0.8382 so quadratic might not give a good approx to the trend

lm2 <- lm(HAbrol_CCI ~ 1 + HAbrol_coral_core + I(HAbrol_coral_core^2), 
          data = dat_joint)
summary(lm2)

# p-value is 0.885
# use linear model

lm2 <- lm(HAbrol_CCI ~ 1 + HAbrol_coral_core, 
          data = dat_joint)
summary(lm2)

# p-value for linear model is not significant as well (0.0638)

modelsummary::modelsummary(list(lm1, lm2))

# Coral core is able to explain the variability in CCI's sst better than NOAA for this Houtman Abrolhos island site.  
# No data is available when logger data is included
# model summary shows not significant
#R-sq values are very low <0.5

#####--------------------#####
## To run the calibration analysis
# Need to run another script (Detrended.R files) to get the following tibbles:
# HAB05B_SrCa_CCI
# HAB05B_SrCa_Logger
# HAB05B_SrCa_CCore
# HAB05B_SrCa_NOAA
#

library(TSclust)
dat <- as_tibble(HAB05B_SrCa_CCI) %>% 
  add_row(as_tibble(HAB05B_SrCa_NOAA)) %>% 
  # no available data if logger data is included : add_row(as_tibble(HAB05B_SrCa_Logger)) %>% 
  add_row(as_tibble(HAB05B_SrCa_CCore) %>% 
            rename(sst = `HAB05B Sr/Ca`)) %>% 
  mutate(data_type = 
           case_when( data_type == "CCI" ~ "HAbrol_CCI",
                      data_type == "Logger" ~ "HAbrol_Logger", 
                      data_type == "NOAA" ~ "HAbrol_NOAA",
                      data_type == "Coral Core" ~ "HAbrol_coral_core",
                      TRUE ~ data_type))

dat_joint <- dat %>% 
  #add_row(dat2) %>% 
  pivot_wider(names_from = data_type, values_from = sst) %>% 
  select(-date) %>% 
  drop_na() 

GGally::ggpairs(dat_joint)
## coral core is slightly more aligned with CCI than NOAA
ggplot(dat_joint, aes(x = HAbrol_coral_core, y = HAbrol_NOAA)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear and slight decreasing trend
library(mgcv)
gam1 <- gam(HAbrol_NOAA ~ s(HAbrol_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam1)
summary(gam1)
# edf is 0.5599 LOW!

lm1 <- lm(HAbrol_NOAA ~ 1 + HAbrol_coral_core + I(HAbrol_coral_core^2), 
          data = dat_joint)
summary(lm1)

# p-value for quadratic is 0.0828
# use linear model

lm1 <- lm(HAbrol_NOAA ~ 1 + HAbrol_coral_core, 
          data = dat_joint)
summary(lm1)

# p-value for linear model is not significant as well

# repeat for CCI 
ggplot(dat_joint, aes(x = HAbrol_coral_core, y = HAbrol_CCI)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend
gam2 <- gam(HAbrol_CCI ~ s(HAbrol_coral_core, bs = "cs"), data = dat_joint, method = "REML")
plot(gam2)
summary(gam2)
# edf = 0.9579 so linear + quadratic should give a good approx to the trend

lm2 <- lm(HAbrol_CCI ~ 1 + HAbrol_coral_core + I(HAbrol_coral_core^2), 
          data = dat_joint)
summary(lm2)

# p-value for quadratic is 0.217
# use linear model

lm2 <- lm(HAbrol_CCI ~ 1 + HAbrol_coral_core, 
          data = dat_joint)
summary(lm2)

# p-value for linear model is significant

modelsummary::modelsummary(list(lm1, lm2))

# extracting fitted sst values of coral core from CCI and NOAA data
CCore_SST_HAB05B_SrCa_CCI <- lm2 %>% 
  augment() %>% 
  rename(`Sr/Ca` = HAbrol_coral_core, sst = .fitted) %>% 
  select(`Sr/Ca`, sst)


write_csv(CCore_SST_HAB05B_SrCa_CCI, here::here
          ("data_raw/CCore_SST_HAB05B_SrCa_CCI.csv"))

# Coral core is able to explain the variability in CCI's sst better than NOAA for this Houtman Abrolhos island site.  
# No data is available when logger data is included
# model summary shows not significant
# R-sq values are very low <0.5
# p-value for linear models are only significant for CCI but not NOAA
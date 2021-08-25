# Ningaloo_13TNT_CCI
# Ningaloo_13TNT_Logger
# Ningaloo_13TNT_CCore
# Ningaloo_13TNT_NOAA
# Need to run the other scripts to obtian the dataset first 

library(TSclust)
dat <- as_tibble(Ningaloo_13TNT_CCI) %>% 
  add_row(as_tibble(Ningaloo_13TNT_NOAA)) %>% 
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
# Need to run another script to get the following tibbles:
# Ningaloo_13TNT_CCI
# Ningaloo_13TNT_Logger
# Ningaloo_13TNT_CCore
# Ningaloo_13TNT_NOAA
#

dat_joint <- dat %>% 
  #add_row(dat2) %>% 
  pivot_wider(names_from = data_type, values_from = sst) %>% 
  select(-date) %>% 
  drop_na() 

GGally::ggpairs(dat_joint)
## coral core is slightly more align with NOAA over CCI 
ggplot(dat_joint, aes(x = ningaloo_coral_core, y = ningaloo_NOAA)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend with a slight curvature. 
library(mgcv)
gam1 <- gam(ningaloo_NOAA ~ s(ningaloo_coral_core, bs = "cs"), data = dat_joint)
plot(gam1)
summary(gam1)
# edf ~ 2 so linear + quadratic should give a good approx to the trend

lm1 <- lm(ningaloo_NOAA ~ 1 + ningaloo_coral_core + I(ningaloo_coral_core^2), 
          data = dat_joint)
summary(lm1)

# repeat for CCI 
ggplot(dat_joint, aes(x = ningaloo_coral_core, y = ningaloo_CCI)) + 
  geom_point() + 
  geom_smooth(method = "gam")
# linear decreasing trend with a slight curvature. 
gam2 <- gam(ningaloo_CCI ~ s(ningaloo_coral_core, bs = "cs"), data = dat_joint)
plot(gam2)
summary(gam2)
# edf ~ 2 so linear + quadratic should give a good approx to the trend

lm2 <- lm(ningaloo_CCI ~ 1 + ningaloo_coral_core + I(ningaloo_coral_core^2), 
          data = dat_joint)
summary(lm1)

modelsummary::modelsummary(list(lm1, lm2))

# Coral core is able to explain the variability in NOAA's sst a tiny bit better than CCI for the site ningaloo. 
# Need to think about whether a straight pair-up using the collection date of coral core data with sst is a good idea. Alternative can be averaging the sst data leading up to the 
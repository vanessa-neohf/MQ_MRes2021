# Ningaloo_13TNT_CCI
# Ningaloo_13TNT_Logger
# Ningaloo_13TNT_CCore
# Ningaloo_13TNT_NOAA
# Need to run the other scripts to obtian the dataset first 

library(TSclust)
dat <- as_tibble(Ningaloo_13TNT_CCI) %>% 
  add_row(as_tibble(Ningaloo_13TNT_Logger)) %>% 
  add_row(as_tibble(Ningaloo_13TNT_NOAA)) %>% 
  mutate(data_type = 
           case_when( data_type == "CCI" ~ "ningaloo_CCI",
                      data_type == "Logger" ~ "ningaloo_Logger", 
                      data_type == "NOAA" ~ "ningaloo_NOAA",
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
  add_row(dat2) %>% 
  pivot_wider(names_from = data_type, values_from = sst) %>% 
  select(-`NA`, -date, -browse_Logger) %>% 
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

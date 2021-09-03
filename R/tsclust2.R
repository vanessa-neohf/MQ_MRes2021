## ---- 
Setup
library(mgcv)
library(tidyverse)

## ---- 
# Now try to group data by month 

Ningaloo_13TNT_CCI_mon <- Ningaloo_13TNT_CCI %>% 
  as_tibble() %>% 
  mutate(
    year_fct = factor(year(date)),
    month_fct = factor(month(date, label = TRUE))
  ) %>% 
  group_by(year_fct, month_fct) %>% 
  select(-date) %>% 
  summarise(sst_mean = mean(sst, na.rm = TRUE)) %>% 
  mutate(data_type = "CCI")

Ningaloo_13TNT_CCore_mon <- Ningaloo_13TNT_CCore %>% 
  as_tibble() %>% 
  mutate(
    year_fct = factor(year(date)),
    month_fct = factor(month(date, label = TRUE))
  ) %>% 
  group_by(year_fct, month_fct) %>% 
  select(-date) %>% 
  summarise(`Sr/Ca_mean` = mean(`Sr/Ca`, na.rm = TRUE)) %>% 
  mutate(data_type = "Coral Core")

Ningaloo_13TNT_NOAA_mon <- Ningaloo_13TNT_NOAA %>% 
  as_tibble() %>% 
  mutate(
    year_fct = factor(year(date)),
    month_fct = factor(month(date, label = TRUE))
  ) %>% 
  group_by(year_fct, month_fct) %>% 
  select(-date) %>% 
  summarise(sst_mean = mean(sst, na.rm = TRUE)) %>% 
  mutate(data_type = "NOAA") 


dat <- as_tibble(Ningaloo_13TNT_CCI_mon) %>% 
  add_row(as_tibble(Ningaloo_13TNT_NOAA_mon)) %>% 
  add_row(as_tibble(Ningaloo_13TNT_CCore_mon) %>% 
            rename(sst_mean = `Sr/Ca_mean`)) %>% 
  mutate(data_type = 
           case_when( data_type == "CCI" ~ "ningaloo_CCI",
                      data_type == "NOAA" ~ "ningaloo_NOAA",
                      data_type == "Coral Core" ~ "ningaloo_coral_core",
                      TRUE ~ data_type))

dat_joint_mean <- dat %>% 
  pivot_wider(names_from = data_type, values_from = sst_mean) %>% 
  select(- c(year_fct, month_fct)) %>% 
  drop_na() 

GGally::ggpairs(dat_joint_mean)
## coral core is slightly more aligned with NOAA than CCI 
ggplot(dat_joint, aes(x = ningaloo_coral_core, y = ningaloo_NOAA)) + 
  geom_point() + 
  geom_smooth(method = "gam")

gam1 <- gam(ningaloo_NOAA ~ s(ningaloo_coral_core, bs = "cs"), data = dat_joint)
plot(gam1)
summary(gam1)



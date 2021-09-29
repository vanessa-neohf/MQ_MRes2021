# Process SST data to get bleaching metrics

# setup ====
rm(list = ls())

# ..... Libraries ====
library(tidyverse)
library(readxl)
library(lubridate)
library(curl)
# library(RCurl)
library(raster)
library(tools)
library(hydrostats)
library(ncdf4)
# library(modes)
library(parallel)
library(beepr)
library(runner)


# ..... Functions ====
shiny_run = TRUE 
cron_run = FALSE
source('kyle_code/app_functions.r')  # This loads the functions from a separate file


# ..... Data files ====
in_dir <- 'kyle_code/data/'
out_dir <- 'kyle_code/data/Coral_Core/'

sst_file <- 'CCore_SST_Ningaloo_Wallabi_Island_CCI.csv'


# Local data directories
local_dir_mmm_climatology = 'kyle_code/data/'

# Local files
mmm_climatology_file = 'ct5km_climatology_v3.1_20190101.nc'  # Used for NOAA's DHW product



# ..... import SST data ====
sst_data <- read_csv(file = paste0(in_dir,sst_file)) %>% 
  mutate(end_date = max(date), Lat = "-28.28", Lon = "113.46")

# ..... settings ====
sst_input <- 'sst'
lat_input <- 'Lat'  # latitude column
lon_input <- 'Lon'  # longitude column
date_input <- 'date'  # date column
end_date_input <- 'end_date'  # date column indicating site date of interest i.e. the date to calculate metrics to
date_format <- 'ymd'  # date column format. e.g. 31/02/1998 = 'dmy'
days <- 84  # number of days to calculate metrics for based on the date value for each row

mmm_from_sst_bool = TRUE  # Calculate mean monthly maximum from sst data based on a start and end date
mmm_from_sst_start_year = 1985
mmm_from_sst_end_year = 1990

mmm_climatology_bool <- TRUE  # mean monthly maximum using NOAAs provided MMM climatology layer

pros_sst <- TRUE  # get sea surface temperature metrics
pros_dhw <- TRUE  # get degree heating weeks metrics
pros_high_spells <- TRUE  # get high spells metrics
pros_low_spells <- TRUE  # get low spells metrics

threshold <- 1  # threshold in degrees Celsius on top of MMM to calculate DHW, high spells and low spells. NOAA default is 1


# Main ====
# ..... convert lat, long and date column names ====
sst_data <- sst_data %>%
  rename(sst = sst_input,
         Date = date_input,
         Latitude = lat_input,
         Longitude = lon_input,
         end_date = end_date_input) %>%
  mutate(Date = date(parse_date_time(Date, orders = date_format)),
         end_date = date(parse_date_time(end_date, orders = date_format)))


# ..... MMM from SST series ====
if(mmm_from_sst_bool) {

  calculate_maximum_monthly_mean_from_sst <- function(sst_data, 
                                                      mmm_from_sst_end_year, 
                                                      mmm_from_sst_start_year,
                                                      group_vars,
                                                      continue_with_missing_sst_data = F
                                                      ) {
  
  mmm_from_sst_year_range <- paste0(mmm_from_sst_start_year,'_to_',mmm_from_sst_end_year)
  
  mmm_data <- sst_data %>%
    mutate(year = year(Date),
           month = month(Date)) %>%
    filter(year <= mmm_from_sst_end_year,
           year >= mmm_from_sst_start_year) %>%
    filter(!year %in% c(1991, 1992))
  
  
  if(sum(is.na(mmm_data[['sst']])) > 0) {
    print('warning, some sst values are missing, mean monthly maximums may deviate from actuals')
    if(continue_with_missing_sst_data == F) {
      return()
    }
  }
  
  out <- mmm_data %>%
    group_by(across(c(group_vars, 'year', 'month'))) %>%
    distinct() %>%
    summarise(monthly_means = mean(sst)) %>%
    ungroup() %>%
    group_by(across(c(group_vars, 'month'))) %>%
    summarise(monthly_means = mean(monthly_means)) %>%
    ungroup() %>%
    group_by(across(c(group_vars))) %>%
    summarise(mmm_from_sst = max(monthly_means)) %>%
    mutate(mmm_from_sst_year_range = mmm_from_sst_year_range) %>%
    distinct()
    
  
  return(out)
  
}


mmm_from_sst <- 
  calculate_maximum_monthly_mean_from_sst(sst_data = sst_data,
                                          mmm_from_sst_end_year = mmm_from_sst_end_year,
                                          mmm_from_sst_start_year = mmm_from_sst_start_year,
                                          group_vars = c('Latitude', 'Longitude'),
                                          continue_with_missing_sst_data = F)

}




# ..... Add MMM_from_SST ====
sst_data_plus_mmm_from_sst <- sst_data %>% 
  left_join(., mmm_from_sst, by = c('Latitude', 'Longitude'))


# Output

output_data <- sst_data_plus_mmm_from_sst %>% 
  mutate(Site = "Wallabi Island")

out_name <- sst_file %>% str_replace(pattern = '.csv', replacement = '')
out_name <- paste0(out_dir, out_name,'_with_mmm.csv')

write_csv(x = output_data, out_name)

length(unique(output_data$Date)) == nrow(output_data) #check for any duplicates

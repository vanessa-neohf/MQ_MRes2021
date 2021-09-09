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


# ..... Data file ====
csv_file <- 'kyle_code/data/NOAA_SST.csv'


# ..... settings ====
sst_input <- 'sst'
lat_input <- 'Latitude'  # latitude column
lon_input <- 'Longitude'  # longitude column
date_input <- 'Date'  # date column

end_date_input <- 'end_date'  # date column indicating site date of interest i.e. the date to calculate metrics to
date_format <- 'ymd'  # date column format. e.g. 31/02/1998 = 'dmy'
days <- 84  # number of days to calculate metrics for based on the date value for each row

mmm_from_sst_bool = TRUE  # Calculate mean monthly maximum from sst data based on a start and end date
mmm_from_sst_start_year = 1985  # NOAA defaults, minus 1993
mmm_from_sst_end_year = 1990  # NOAA defaults, minus 1993

mmm_climatology_bool <- TRUE  # mean monthly maximum using NOAAs provided MMM climatology layer

pros_sst <- TRUE  # get sea surface temperature metrics
pros_dhw <- TRUE  # get degree heating weeks metrics
pros_high_spells <- TRUE  # get high spells metrics
pros_low_spells <- TRUE  # get low spells metrics

threshold <- 1  # threshold in degrees celcius on top of MMM to calculate DHW, high spells and low spells. NOAA default is 1


# ..... Constants ====

# Local data directories
local_dir_mmm_climatology = 'kyle_code/data/'

# Local files
mmm_climatology_file = 'ct5km_climatology_v3.1_20190101.nc'  # Used for NOAA's DHW product



# ..... import SST data ====
sst_data <- read_csv(file = csv_file) 


# ..... convert sst, lat, long and date column names ====
sst_data <- sst_data %>%
  rename(sst = sst_input,
         Date = date_input,
         Latitude = lat_input,
         Longitude = lon_input,
         end_date = end_date_input) %>%
  mutate(Date = date(parse_date_time(Date, orders = date_format)),
         end_date = date(parse_date_time(end_date, orders = date_format)))


# ..... fix for brackets around sst data ====
sst_data <- sst_data %>%
  mutate(sst = str_replace(string = sst, pattern = '\\[\\[', replacement = '')) %>%
  mutate(sst = str_replace(string = sst, pattern = '\\]\\]', replacement = '')) %>%
  mutate(sst = as.numeric(sst))




# ..... MMM Climatology recalculation ====
if(mmm_climatology_bool) {
  
  coords_dates_mmm_climatology <- sst_data %>% 
    mutate(Date = ymd('2019/01/01')) %>% 
    dplyr::select(all_of(c('Latitude', 'Longitude', 'Date', 'end_date'))) %>% 
    distinct()
  mmm_climatology_data_out <- get_raster_time_series_load_read_local(local_nc_files = paste0(local_dir_mmm_climatology,mmm_climatology_file), 
                                                                     coords_dates = coords_dates_mmm_climatology, 
                                                                     message = 'Processing MMM Climatology data', 
                                                                     datestring_format = 'ymd',
                                                                     shiny_run = FALSE,
                                                                     method='simple'
  ) %>%
    rename(mmm_climatology=value)
  mmm_climatology_data_out <- mmm_climatology_data_out %>% rename(mmm_climatology_as_at = Date)
  
}


# ..... MMM from SST series ====
if(mmm_from_sst_bool) {

  calculate_maximum_monthly_mean_from_sst <- function(sst_data, 
                                                      mmm_from_sst_end_year, 
                                                      mmm_from_sst_start_year,
                                                      continue_with_missing_sst_data = F) {
    
  mmm_from_sst_year_range <- paste0(mmm_from_sst_start_year,'_to_',mmm_from_sst_end_year)
  
  mmm_data <- sst_data %>%
    mutate(year = year(Date),
           month = month(Date)) %>%
    filter(year <= mmm_from_sst_end_year,
           year >= mmm_from_sst_start_year)
  
  
  if(sum(is.na(mmm_data[['sst']])) > 0) {
    print('warning, some sst values are missing, mean monthly maximums may deviate from actuals')
    if(continue_with_missing_sst_data == F) {
      return()
    }
  }
  
  out <- mmm_data %>%
    group_by(Latitude, Longitude, year, month) %>%
    distinct() %>%
    summarise(monthly_means = mean(sst)) %>%
    ungroup() %>%
    group_by(Latitude, Longitude, year) %>%
    summarise(monthly_means = mean(monthly_means)) %>%
    ungroup() %>%
    group_by(Latitude, Longitude) %>%
    filter(monthly_means == max(monthly_means)) %>%
    rename(mmm_from_sst = monthly_means) %>%
    mutate(mmm_from_sst_year_range = mmm_from_sst_year_range) %>%
    distinct()
    
  
  return(out)
  
}

  
  mmm_from_sst <- 
    calculate_maximum_monthly_mean_from_sst(sst_data = sst_data,
                                            mmm_from_sst_end_year = mmm_from_sst_end_year,
                                            mmm_from_sst_start_year = mmm_from_sst_start_year,
                                            continue_with_missing_sst_data = F)

}

# ..... Add MMMs to SST and calculate DHW ====
sst_data_plus_mmm_and_dhw <- sst_data %>% 
  
{if(mmm_climatology_bool) {
  left_join(., mmm_climatology_data_out, by = c('Latitude', 'Longitude', 'end_date')) %>%
    mutate(hotspot = sst - mmm_climatology,
           degree_heating_day_mmm_climatology = ifelse(hotspot >= threshold, hotspot, 0),
           degree_heating_week_mmm_climatology = degree_heating_day_mmm_climatology/7) 
} else {
  .
}} %>%
  
{if(mmm_from_sst_bool) {
  left_join(., mmm_from_sst, by = c('Latitude', 'Longitude')) %>%
    mutate(hotspot = sst - mmm_from_sst,
           degree_heating_day_mmm_from_sst = ifelse(hotspot >= threshold, hotspot, 0),
           degree_heating_week_mmm_from_sst = degree_heating_day_mmm_from_sst/7) 
} else {
  .
}} %>%
  mutate(dhw_threshold = threshold)


# Check how your sst derived mmm's match up with the NOAA provided ones
if(mmm_climatology_bool && mmm_from_sst_bool) {
  
  mean_absolute_deviation_from_noaa_mmm <- sst_data_plus_mmm_and_dhw %>%
    mutate(mad = mmm_from_sst - mmm_climatology) %>%
    group_by(Latitude, Longitude) %>%
    summarise(mad = mean(abs(mad))) %>%
    print() %>%
    ungroup() %>%
    summarise(mad = mean(abs(mad))) %>%
    print()

}



# Calculating running sum of DHW
unique(sst_data$Reef_Site) #for NOAA combined file only

output_data <- sst_data_plus_mmm_and_dhw %>% 
  dplyr::select(-end_date) %>%
  mutate(DHW_value = degree_heating_week_mmm_from_sst + dhw_threshold) %>% 
  mutate(DHW_value = if_else(DHW_value <= 1, 0, DHW_value))

output_data <- output_data %>%   #for NOAA combined file only
  filter(Reef_Site == "Wallabi Island")

output_data <- output_data %>% 
  mutate(accum_DHW_12weeks = runner(
    x = output_data$DHW_value,
    f = sum, 
    k = "84 days",
    idx = output_data$Date,
    na_pad = TRUE
  )) %>% 
  mutate(accum_DHW_12weeks = accum_DHW_12weeks / 7) %>%  #calculate DHW with sum of SST (84 days) divided by 7
  mutate(Date = as.character(Date)) 

max(output_data$accum_DHW_12weeks) #check accumulated DHW values
 

out_name <- csv_file %>% str_replace(pattern = 'SST.csv', replacement = '')
out_name <- paste0(out_name,'Wallabi_Island_with_mmm_and_dhw.csv')

write_csv(x = output_data, out_name)


# Will finish metrics calculation if needed

# # ..... calculate metrics ====
# # SST
# sst_data_processed <- sst_data_plus_mmm_out %>% 
#   dplyr::select(-Date) %>%
#   group_by(Latitude, Longitude, end_date) %>%
#   mutate(dhw_calculated_thres = threshold) %>%
#   summarise(mean_sst = mean(sst),
#             sd_sst = sd(sst),
#             # bimodality_coef = bimodality_coefficient(sst),  # TODO modes unavailable from CRAN/latest r version
#             # bimodality_ratio = bimodality_ratio(sst),   TODO modes unavailable from CRAN/latest r version
#             
#             degree_heating_week_mmm_climatology = if_else(mmm_climatology_bool, sum(degree_heating_week_mmm_climatology), NA_real_),
#             mmm_climatology_as_at = if_else(mmm_climatology_bool, first(mmm_climatology_as_at), ymd(NA)),
#             mmm_climatology = if_else(mmm_climatology_bool, first(mmm_climatology), NA_real_),
#             
#             degree_heating_week_mmm_custom_range = if_else(mmm_custom_range_bool, sum(degree_heating_week_mmm_custom_range), NA_real_),
#             mmm_custom_range_years = if_else(mmm_custom_range_bool, first(mmm_custom_range_years), NA_character_),
#             mmm_custom_range = if_else(mmm_custom_range_bool, first(mmm_custom_range), NA_real_),
#             
#             degree_heating_week_mmm_to_date = if_else(mmm_to_date_bool, sum(degree_heating_week_mmm_to_date), NA_real_),
#             mmm_to_date_start_date = if_else(mmm_to_date_bool, first(mmm_to_date_start_date), ymd(NA)),
#             mmm_to_date = if_else(mmm_to_date_bool, first(mmm_to_date), NA_real_),
#             
#             dhw_calculated_thres = first(dhw_calculated_thres)) %>%
#   ungroup() %>%
#   
#   {if(!mmm_climatology_bool) {
#     dplyr::select(., -degree_heating_week_mmm_climatology, -mmm_climatology_as_at, -mmm_climatology)
#   } else {
#     .
#   }} %>%
#   
#   {if(!mmm_custom_range_bool) {
#     dplyr::select(., -degree_heating_week_mmm_custom_range, -mmm_custom_range_years, mmm_custom_range)
#   } else {
#     .
#   }} %>%
#   
#   {if(!mmm_to_date_bool) {
#     dplyr::select(., -degree_heating_week_mmm_to_date, -mmm_to_date_start_date, -mmm_to_date)
#   } else {
#     .
#   }}
# 
# # DHW
# # Single value
# dhw_data_processed <- dhw_data_out %>%
#   dplyr::select(-Date) %>%
#   distinct()
# 
# 
# # High Spells
# high_spells <- sst_data_plus_mmm_out %>%
#   group_by(Latitude, Longitude, end_date, mmm_climatology) %>%
#   rename(Date=Date, Q=sst) %>%
#   group_modify(~high.spells(.x,
#                             ind.days = 5,  # separate spells within this threshold are considered to be part of a single spell
#                             volume = TRUE,  # spell volume statistics
#                             threshold = .x$mmm_climatology),
#                .keep = TRUE) %>%  # mmm used as the threshold for spell statistics
#   distinct() %>%
#   ungroup() %>%
#   dplyr::select(
#     Latitude,
#     Longitude,
#     end_date,
#     high_spell_event_count=n.events, 
#     high_spell_duration_mean=avg.high.spell.duration,
#     high_spell_duration_max=max.high.spell.duration,
#     high_spell_rise_mean=avg.rise,
#     high_spell_peak_mean=avg.spell.peak,
#     high_spell_peak_sd=sd.spell.peak,
#     high_spell_threshold = mmm_climatology
#   ) %>%
#   mutate_at(.vars = vars(starts_with('high_spell')) ,~ifelse(is.nan(.) | is.infinite(.), NA, .))
# 
# 
# # Low Spells
# low_spells <- sst_data_plus_mmm_out %>%
#   group_by(Latitude, Longitude, end_date, mmm_climatology) %>%
#   rename(Date=Date, Q=sst) %>%
#   group_modify(~low.spells(.x,
#                            volume = TRUE,  # spell volume statistics
#                            threshold = .x$mmm_climatology),  # mmm used as threshold for spell statistics
#                .keep = TRUE) %>% 
#   distinct() %>%
#   ungroup() %>%
#   dplyr::select(
#     Latitude,
#     Longitude,
#     end_date,
#     low_spell_duration_mean=avg.low.spell.duration,
#     low_spell_duration_max=max.low.duration,  # TODO Check original max low spell name
#     low_spell_threshold = mmm_climatology
#   ) %>%
#   mutate_at(.vars = vars(starts_with('low_spell')) ,~ifelse(is.nan(.) | is.infinite(.), NA, .))
# 
# 
# # ..... Combine metrics data ====
# 
# # Variable selection
# var_select <- c('Latitude', 'Longitude', 'end_date', 'days_into_past',
#                 {if(pros_sst) colnames(sst_data_processed)},
#                 {if(pros_dhw) colnames(dhw_data_processed)},
#                 {if(pros_high_spells) colnames(high_spells)},
#                 {if(pros_low_spells) colnames(low_spells)}
# )
# 
# # data joining
# all_data_out <- sst_data_processed %>%
#   full_join(dhw_data_processed, by=c('Latitude', 'Longitude', 'end_date')) %>%
#   full_join(high_spells, by=c('Latitude', 'Longitude', 'end_date')) %>%
#   full_join(low_spells, by=c('Latitude', 'Longitude', 'end_date')) %>%
#   mutate(days_into_past = days) %>%
#   dplyr::select(var_select)
# # Join back to input data
# all_data_out_w_aux <- data_processed %>%
#   # rename(Latitude = lat_input,
#   #        Longitude = lon_input,
#   #        end_date = date_input) %>%
#   # mutate(end_date = as.character(date(parse_date_time(x = end_date, orders = date_format)))) %>%
#   left_join(all_data_out)
# 
# # ..... export metrics dataset ====
# write_csv(x = all_data_out_w_aux, file = paste0(str_replace(csv_file, pattern = '.csv', replacement = ''),'_coral_bleaching_metrics_data.csv'))
# 

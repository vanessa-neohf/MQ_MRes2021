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
in_dir <- 'kyle_code/data/SST/noaa/'
out_dir <- 'kyle_code/data/DHW/'

sst_file <- 'NOAA_SST_scott_reef_1985to2021.csv'


# Local data directories
local_dir_mmm_climatology = 'kyle_code/data/'

# Local files
mmm_climatology_file = 'ct5km_climatology_v3.1_20190101.nc'  # Used for NOAA's DHW product



# ..... import SST data ====
sst_data <- read_csv(file = paste0(in_dir,sst_file))

# additional_data
scott_reef_dhw_from_app <- read_csv('kyle_code/data/DHW/NOAA_scott_reef_dhw_data_2016-05-18_from_app.csv')


# ..... settings ====
lat_input <- 'Latitude'  # latitude column
lon_input <- 'Longitude'  # longitude column
date_input <- 'Date'  # date column
end_date_input <- 'end_date'  # date column indicating site date of interest i.e. the date to calculate metrics to
date_format <- 'dmy'  # date column format. e.g. 31/02/1998 = 'dmy'
days <- 84  # number of days to calculate metrics for based on the date value for each row

mmm_from_sst_bool = TRUE  # Calculate mean monthly maximum from sst data based on a start and end date
mmm_from_sst_start_year = 1985
mmm_from_sst_end_year = 1993

recenter_to_noaa50km_time = TRUE
recenter_year =  1988.2857  # NOAA recenter year

mmm_climatology_bool <- TRUE  # mean monthly maximum using NOAAs provided MMM climatology layer

pros_sst <- TRUE  # get sea surface temperature metrics
pros_dhw <- TRUE  # get degree heating weeks metrics
pros_high_spells <- TRUE  # get high spells metrics
pros_low_spells <- TRUE  # get low spells metrics

threshold <- 1  # threshold in degrees Celsius on top of MMM to calculate DHW, high spells and low spells. NOAA default is 1


# Main ====
# ..... convert lat, long and date column names ====
sst_data <- sst_data %>%
  rename(Date = date_input,
         Latitude = lat_input,
         Longitude = lon_input,
         end_date = end_date_input) %>%
  mutate(Date = date(parse_date_time(Date, orders = date_format)),
         end_date = date(parse_date_time(end_date, orders = date_format)))


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
                                                      group_vars,
                                                      continue_with_missing_sst_data = F,
                                                      recenter_to_noaa50km_time = T,
                                                      recenter_year = 1988.2857) {
  
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
  
  mmms <- mmm_data %>%
    group_by(across(c(group_vars, 'year', 'month'))) %>%
    distinct() %>%
    summarise(monthly_means = mean(sst)) %>%
    ungroup()
  
  
  if(recenter_to_noaa50km_time) {
    
    print(paste0('recentering MMMs to ',recenter_year))
    
    locations_and_months <- mmms %>% dplyr::select(all_of(c(group_vars, 'month'))) %>% distinct()
    
    mmms_recentered <- lapply(1:nrow(locations_and_months), function(x) {
      
      selected_data <- locations_and_months[x,] %>% left_join(mmms)
      
      model <- lm(monthly_means ~ year, data = selected_data)
      
      mmm_recentered <- predict(object = model, newdata = data_frame(year = recenter_year))
      
      mmm_recentered <- data_frame(mmm_recentered = mmm_recentered, recenter_year = recenter_year)
      
      out <- bind_cols(selected_data[1,group_vars], mmm_recentered)
      
    }) %>% bind_rows()
    
    out <- mmms_recentered %>%
      group_by(across(c(group_vars))) %>%
      summarise(mmm_from_sst = max(mmm_recentered),
                recenter_year = first(recenter_year)) %>%
      mutate(mmm_from_sst_year_range = mmm_from_sst_year_range) %>%
      distinct()
    
  } else {
    
    out <- mmms %>%
      group_by(across(c(group_vars, 'month'))) %>%
      summarise(monthly_means = mean(monthly_means)) %>%
      ungroup() %>%
      group_by(across(c(group_vars))) %>%
      summarise(mmm_from_sst = max(monthly_means)) %>%
      mutate(mmm_from_sst_year_range = mmm_from_sst_year_range) %>%
      distinct()
    
  }
  
  return(out)
  
  }
  

  mmm_from_sst <- 
    calculate_maximum_monthly_mean_from_sst(sst_data = sst_data,
                                            mmm_from_sst_end_year = mmm_from_sst_end_year,
                                            mmm_from_sst_start_year = mmm_from_sst_start_year,
                                            group_vars = c('Latitude', 'Longitude'),
                                            continue_with_missing_sst_data = F,
                                            recenter_to_noaa50km_time = recenter_to_noaa50km_time,
                                            recenter_year = recenter_year)

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


# Output

output_data <- sst_data_plus_mmm_and_dhw %>% 
  group_by(Latitude, Longitude) %>% 
  mutate(accum_DHW_12_weeks_kz = runner(x = degree_heating_week_mmm_from_sst, f = sum, k = days))


# Old accumulation code.
# output_data <- sst_data_plus_mmm_and_dhw %>% 
#   dplyr::select(-end_date) %>%
#   mutate(Date = as.character(Date)) %>%
#   mutate(DHW_value = degree_heating_week_mmm_from_sst + dhw_threshold) %>% 
#   mutate(DHW_value = if_else(DHW_value <= 1, 0, DHW_value)) %>% 
#   group_by(Latitude, Longitude) %>% 
#   run_by(idx = "Date", lag = 84, na_pad = FALSE) %>% 
#   mutate(accum_DHW_12weeks = runner(
#     x = DHW_value,
#     f = sum
#     )) %>% 
#   mutate(accum_DHW_12weeks = accum_DHW_12weeks/7) %>% #calculate DHW with sum of SST (84 days) divided by 7
#   ungroup(Latitude, Longitude)
  

output_data <- output_data %>% mutate(as_date(Date))

out_name <- sst_file %>% str_replace(pattern = '.csv', replacement = '')
out_name <- paste0(out_dir, out_name,'_with_mmm_and_dhw.csv')

write_csv(x = output_data, out_name)


# ..... Compare to DHW from app ====

compare_data <- scott_reef_dhw_from_app %>% 
  dplyr::select(-end_date) %>%
  left_join(output_data, 
            by = c('Latitude', 'Longitude', 'Date'), 
            suffix = c('.app', '.script'))

# MMM climatology VS MMM from SST
ggplot(data = compare_data,
       aes(y = mmm_climatology.app,
           x = mmm_from_sst)) + 
  geom_point(size = 3, shape = 21, fill = 'red') +
  geom_abline(intercept = 0, slope = 1) +
  
  labs(title = 'MMM from NOAA climatology vs MMM from SST (1985 to 1990 + 1993)',
       subtitle = 'MMM from SST is consistently ~ 0.1 C lower (see 1:1 line)',
       y = 'MMM from NOAA climatology',
       x = 'MMM from SST (1985 to 1990 + 1993)') +
  theme_minimal()


# DHW product VS DHW from MMM from SST
ggplot(data = compare_data,
       aes(y = dhw_from_noaa_layers,
           x = accum_DHW_12_weeks_kz)) + 
  geom_point(size = 3, shape = 21, fill = 'red') +
  geom_abline(intercept = 0, slope = 1) +
  
  labs(title = 'DHW from NOAA DHW layer vs DHW from MMM from SST (1985 to 1990 + 1993)',
       subtitle = 'DHW from MMM from SST is consistently ~ 0.5 dhw higher (see 1:1 line)',
       y = 'DHW from NOAA DHW layer',
       x = 'DHW from MMM from SST (1985 to 1990 + 1993)') +
  theme_minimal()


# .... Combining files from the same data source ====
csv_file_NOAA_1 <- 'kyle_code/data/coral_core_coordinates_scott_reef_NOAA_SST_data_with_mmm_and_dhw.csv'
csv_file_NOAA_2 <- 'kyle_code/data/NOAA_SST_with_mmm_and_dhw.csv'
NOAA_Scott_Reef <- read_csv(file = csv_file_NOAA_1)
NOAA_Others <- read_csv(file = csv_file_NOAA_2)
NOAA_Scott_Reef <- NOAA_Scott_Reef %>% #align column names with NOAA_Others
  rename(Site = site,
         Reef_Site = subsite) %>% 
  mutate(Data_Type = "NOAA")
NOAA_Others <- NOAA_Others %>% 
  mutate(Data_Type = if_else(Data_Type == "NOAA", "NOAA", "NOAA"))

NOAA_Combined <- bind_rows(csv_file_NOAA_1, csv_file_NOAA_2)

out_name <- csv_file_NOAA_2 %>% str_replace(pattern = 'SST', replacement = 'Combined')

#why does this not work?? <- out_name <- ('NOAA_Combined_with_mmm_and_dhw.csv')

write_csv(x = NOAA_Combined, out_name)


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

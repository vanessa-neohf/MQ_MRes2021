
# Libraries
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

# Functions ====

# Get and process user data ====

# Find closest string
# Used to find likely variable names for latitude, longitude and date.
find_closest_string <- function(target, choices) {
  dist <- adist(target, choices)
  found_pos <- which(dist == min(dist))[1]
  found <- choices[found_pos]
  return(found)
}

# Find closest date format
# Used to find likely variable names for latitude, longitude and date.
find_closest_date_format <- function(target, choices = c('ymd', 'dmy', 'mdy', 'ydm', 'ymd HM', 'ymd HMS', 'mdy HM', 'mdy HMS')) {
  format_match <- guess_formats(target, choices)
  best_match <- names(format_match)[names(format_match) %in% choices == T][1]
  return(best_match)
}


# Read and check incoming files
read_and_check_files <- function(file_path, shiny_mode = TRUE) {
  file_type <- file_ext(file_path)
  if(file_type == 'csv') {
    x <- read_csv(file_path) %>%
      select_all(~gsub("\\s+|\\.", "_", .))
    
    } else if(file_type == 'rds') {
    
      x <- read_rds(file_path)
      if(is.data.frame(x)) {
      
        x <- x %>% select_all(~gsub("\\s+|\\.", "_", .)) 
      
        } else {
        
          if(shiny_mode) {
            showNotification('loaded rds file is not a dataframe, all rds files should be in a dataframe format',
                             type='warning')
        
            } else {
              print('loaded rds file is not a dataframe, all rds files should be in a dataframe format')
              }
          }
  
      } else if(file_type %in% c('xlsx','xls')) {
        
        x <- read_excel(path = file_path) %>%
          select_all(~gsub("\\s+|\\.", "_", .))
        
        } else {
          if(shiny_mode) {
            showNotification('invalid file type, please upload either a .csv, .xlsx, or .rds file',
                             type='warning')
    } else {
      print('invalid file type, please upload either a .csv, .xlsx, or .rds file')
    }
  }
  x
}

# Select lat, long and date columns
select_target_vars <- function(data, lat_var, long_var, date_var) {
  x <- data %>% 
    select_(lat_var,
            long_var,
            date_var)
}


# Rename lat, long and dat columns
rename_target_vars <- function(data, lat_var, long_var, date_var) {
  x <- data %>% 
    rename(Latitude = lat_var,
           Longitude = long_var,
           Date = date_var) 
}


# Process incoming data
process_incoming_data <- function(data, lat_var, long_var, date_var, date_format) {
  rename_target_vars(data = data, 
                     lat_var = lat_var,
                     long_var = long_var,
                     date_var = date_var) %>%
    mutate(Latitude = as.numeric(Latitude),
           Longitude = as.numeric(Longitude),
           Date = date(parse_date_time(Date, orders = date_format))) %>%
    mutate(end_date = Date)
}

# Get NOAA data ====

# OLD Generate expanded date and location table - less generic
# generate_expanded_date_and_location_table <- function(data, days_into_past) {
#   data %>%
#     mutate(start_date = Date - days_into_past) %>%
#     group_by(row_number()) %>%
#     tidyr::complete(end_date = end_date,
#                     Date = seq.Date(max(start_date), max(Date), by='day'),
#                     Latitude=Latitude,
#                     Longitude=Longitude) %>%
#     ungroup() %>%
#     dplyr::select(Latitude, Longitude, Date, end_date) %>%
#     distinct()
# }


generate_expanded_date_and_location_table <- function(data, 
                                                      time_unit = 'day', 
                                                      units_into_past = 12*7, 
                                                      start_date_override = NA,
                                                      end_date_override = NA) {
  

  out <- data %>%
    mutate(row_number = row_number()) %>%
    group_by(row_number) %>%
    
    {if(is.Date(start_date_override)) {
      mutate(., start_date = start_date_override)
    } else {
      mutate(., start_date = seq.Date(Date, length = 2, by = paste0("-",units_into_past," ",time_unit,"s"))[2])
    }} %>%
    
    {if(is.Date(end_date_override)) {
      mutate(., end_date = end_date_override)
    } else {
      .
    }} %>%
    
    tidyr::complete(end_date = end_date,
                    Date = seq.Date(from = min(start_date), 
                                    to = max(end_date),
                                    by = time_unit),
                    Latitude=Latitude,
                    Longitude=Longitude) %>%
    ungroup() %>%
    dplyr::select(Latitude, Longitude, Date, end_date) %>%
    distinct()
  
  return(out)
}


# list files in ftp folder
list_ftp_files = function(url_folder) {
  h = new_handle(dirlistonly=TRUE)
  con = curl(url_folder, 'r', h)
  tbl = read.table(con, stringsAsFactors=FALSE, fill=TRUE)
  close(con)
  urls = tbl %>% mutate(V1 = paste0(url_folder, tbl$V1))
  return(urls)
}

# Generate datestring
generate_datestring <- function(date) {
  day <- day(date)
  day <- ifelse(day < 10, paste0('0',day), paste(day))
  month <- month(date)
  month <- ifelse(month < 10, paste0('0',month), paste(month))
  year <- year(date)
  
  out <- paste0(year, month, day)
}

# extract datestring from file path
extract_datestring <- function(file_string, prefix = '_', postfix = '.nc') {
  str_extract(string = file_string, pattern = '_[0-9]*.nc') %>%
    gsub(pattern = prefix, replacement = '', x = .) %>%
    gsub(pattern = postfix, replacement = '', x = .)
}


# convert datestring to date
datestring_to_date <- function(datestring, datestring_format = 'ymd') {
  if(datestring_format == 'ymd') {
    out <- ymd(datestring)
  } else if(datestring_format == 'ym') {
    out <- ymd(datestring, truncated = 1)
  } else {
    print('incorrect datestring format, choose from "ymd", or "ym"')
  }
  if(is.na(out)) {
    print('datestring failed to parse')
    return()
  }
  return(out)
}

# Generate dates table
generate_datestring_table = function(date_series) {
    tibble(Date = date(date_series)) %>%
    distinct() %>%
    mutate(year = year(Date)) %>%
    mutate(datestring = generate_datestring(Date))
}


# get and filter ftp url files by date
get_and_filter_urls_by_datestring = function(url_dates_table, base_url) {
  #url_datesting_grep = paste0(url_dates_table$datestring, collapse='|')
  out = lapply(url_dates_table$year %>% unique(), 
               function(year) {
                 list_ftp_files(paste0(base_url,year,'/'))
               }) %>% 
    unlist() %>% 
    data_frame() %>%
    rename('url' = '.') %>%
    filter(!grepl('.nc.md5', url)) %>%
    mutate(datestring = str_extract(url, pattern = '_[0-9]*.nc')) %>%
    mutate(datestring = str_extract(datestring, pattern = '[0-9]+')) %>%
    inner_join(url_dates_table) %>%
    #mutate(date = ymd(date)) %>%
    dplyr::select(-datestring)
  return(out)
}


##### TODO SST Monthly Mean Testing
# Individual files
get_sst_mm_urls = function(year) {
  list_ftp_files(paste0(sst_monthly_url, '2019/')) %>%
    unlist() %>%
    tibble() %>%
    filter(!grepl('.nc.md5', url))
  filter(grepl('sst_mean', url))
}
# TODO potential option for custom MMM


# Single raster
get_mmm_url <- function() {
  list_ftp_files(paste0(sst_mean_month_max_url)) %>%
    unlist() %>%
    data_frame() %>%
    filter(!grepl('.nc.md5', url))
  filter(grepl('sst_mean', url))
}


list_local_nc_files <- function(dir, ...) {
  list.files(dir, ...)
}


existing_files_bool <- function(target_list, compare_list) {
  target_list %in% compare_list
}


download_nc_to_local <- function(url_list, out_names_list, out_dir) {
  apply_range <- 1:length(url_list)
  lapply(apply_range, function(x) {
    out_file <- paste0(out_dir, out_names_list[x])
    curl_download(url=url_list[x], destfile=out_file ,quiet=TRUE, mode="wb")
   }) 
}


filter_local_files_by_datestring <- function(local_dir, url_dates_table) {
  local_files <- list_local_nc_files(local_dir, full.names=T)
  date_string_regex <- paste(url_dates_table$datestring, collapse = '|')
  local_files_in_datestring_bool <- sapply(local_files, function(file_x) {
    grepl(pattern = date_string_regex,
          x = file_x)
  })
  if(length(local_files_in_datestring_bool) == 0) {
    local_files_in_datestring_bool = FALSE
  }
  filtered_files <- local_files[local_files_in_datestring_bool]
  if(length(filtered_files) != nrow(url_dates_table)) {
    print('warning, some files missing for provided dates')
  }
  return(filtered_files)
}


# Download and load nc file as raster
download_and_load_nc <- function(nc_ftp_url_table, message='Downloading data') {
  progress_inc = 1/nrow(nc_ftp_url_table)
  withProgress(message=message, value=0, max=1, {
    out = lapply(nc_ftp_url_table$url, function(url){
      incProgress(progress_inc)
      in_file = tempfile(fileext = '.nc')
      download.file(url, in_file, mode='wb', quiet=TRUE)
      rast = raster(in_file, band=1)
    })
  })
  return(out)
}
# TODO parallelise download and load function for speedup.

# Load list of raster files from local
load_nc_locally <- function(local_nc_files, datestring_format = 'ymd') {
  out = lapply(local_nc_files, function(file_x) {
    raster_loaded = raster(file_x, band=1)
  })
  out_dates = lapply(local_nc_files, function(file_x) {
    raster_date = file_x %>%
      extract_datestring() %>%
      datestring_to_date(datestring_format = datestring_format) %>%
      as.character()
  })
  names(out) <- out_dates
  return(out)
}


# extract data from raster
# OLD WORKING VERSION - No custom radius/buffering
get_raster_data_old <- function(raster_loaded, coords_date) {
  out <- raster::extract(raster_loaded, coords_date[,c('Longitude','Latitude')]) %>% data_frame(value = as.numeric(.))
  out <- out %>% mutate(Latitude=coords_date$Latitude, 
                        Longitude=coords_date$Longitude, 
                        Date=coords_date$Date, 
                        end_date=coords_date$end_date)
  #out <- do.call('bind_rows', out)
  return(out)
}


get_raster_data <- function(raster_loaded, coords_date, ...) {
  dat <- raster::extract(raster_loaded, 
                         coords_date[,c('Longitude', 'Latitude'), ],
                         ...) #%>% 
    # tibble(value = as.numeric(.))
  out <- tibble(value = as.numeric(dat),
                Latitude=coords_date$Latitude,
                Longitude=coords_date$Longitude, 
                Date=coords_date$Date,
                end_date=coords_date$end_date
                )
  #out <- do.call('bind_rows', out)
  return(out)
}





# extract data from raster time stack
# OLD WORKING VERSION
get_raster_time_series <- function(raster_list, coords_dates, message='Processing raster data', shiny_run = TRUE,  ...) {
  dates = coords_dates$Date %>% unique()
  progress_inc = 1/length(dates)
  if(shiny_run) {
    withProgress(message=message, value=0, max=1, {
    out = lapply(1:length(raster_list), function(x) {
      incProgress(progress_inc)
      in_date = dates[x] %>% as.character()
      coords_date_in = coords_dates %>% filter(Date == in_date)
      dat = get_raster_data(raster_list[[in_date]], coords_date_in, ...)
      })
    })
    } else {
      out = lapply(1:length(raster_list), function(x) {
        in_date = dates[x] %>% as.character()
        coords_date_in = coords_dates %>% filter(Date == in_date)
        dat = get_raster_data(raster_list[[in_date]], coords_date_in, ...)
        })
    }
  # out = out[[1]]
  out = do.call('bind_rows', out)
  # out = extract(raster_time_stack@raster, coords) # %>%
  # data_frame() %>%
  # bind_cols(coords)
}


# Version of get_raster that loads and extracts the data in sequence.
# This stops the need to load all rasters into memory prior to extracting the data
get_raster_time_series_load_read_local <- function(local_nc_files, coords_dates, message='Processing raster data', datestring_format = 'ymd', shiny_run = TRUE, ...) {
  progress_inc = 1/length(local_nc_files)
  if(shiny_run) {
    withProgress(message=message, value=0, max=1, {
      out <- lapply(local_nc_files, function(file_x) {
        incProgress(progress_inc)
        raster_loaded = raster(file_x, band=1)
        raster_date = file_x %>%
        extract_datestring() %>%
          datestring_to_date(datestring_format = datestring_format) %>%
          as.character()
        coords_date_in = coords_dates %>% filter(Date == raster_date)
        dat = get_raster_data(raster_loaded, coords_date_in, ...)
        }) 
      }) 
  } else {
    out <- lapply(local_nc_files, function(file_x) {
      raster_loaded = raster(file_x, band=1)
      raster_date = file_x %>%
        extract_datestring() %>%
        datestring_to_date(datestring_format = datestring_format) %>%
        as.character()
      coords_date_in = coords_dates %>% filter(Date == raster_date)
      dat = get_raster_data(raster_loaded, coords_date_in, ...)
    }) 
  }
  out <- do.call('bind_rows', out)
}


# Testing ====

if(shiny_run == FALSE && cron_run == FALSE) {
  print('if you see this when running the ShinyApp, something has gone wrong')
  
  # Constants
  # URLS
  dhw_url = 'ftp://ftp.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/5km/v3.1/nc/v1.0/daily/dhw/'
  sst_url = 'ftp://ftp.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/coraltemp/v1.0/nc/'
  sst_monthly_url = 'ftp://ftp.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/5km/v3.1/nc/v1.0/monthly/'
  mmm_climatology_url = 'ftp://ftp.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/5km/v3.1/climatology/nc/ct5km_climatology_v3.1.nc' #Used  for NOAA's DHW product
  
  # Local data directories
  local_dir_dhw = '/Users/kylezawada/Documents/CoralBleachingMetrics_data/NOAA/DHW'
  local_dir_sst = '/Users/kylezawada/Documents/CoralBleachingMetrics_data/NOAA/SST'
  local_dir_mmm = '/Users/kylezawada/Documents/CoralBleachingMetrics_data/NOAA/SST_MMM'
  local_dir_mmm_climatology = '/Users/kylezawada/Documents/CoralBleachingMetrics_data/NOAA/MMM_Climatology'
  
  # Local files
  mmm_climatology_file = 'ct5km_climatology_v3.1_20190101.nc'  # Used for NOAA's DHW product
  
  # Example data
  example_rds_file <- "wcs_benthic_for_maina_with_date_batch_5.rds"
  example_csv_file <- 'example_lizard_island_bleaching.csv'
  
  # Test settings/inputs for app
  debug.only_first_5_observations <- TRUE
  debug.local_files <- TRUE
  
  lat_input <- 'latitude'
  lon_input <- 'longitude'
  date_input <- 'date'
  date_format <- 'dmy'
  days <- 1 #12*7
  
  mmm_custom_range_bool = TRUE  # MMM from start to end year
  mmm_custom_start_year = 1985
  mmm_custom_end_year = 2012
  
  mmm_to_date_bool <- TRUE  # MMM from start year to provided date
  mmm_to_date_start_year <- 1985
  
  mmm_climatology_bool <- TRUE  # MMM recalculated using NOAAs MMM climaotlogy layer
  
  dhw_product_bool <- TRUE  # DHW direct from NOAA layers
  
  pros_sst <- TRUE
  pros_dhw <- TRUE
  pros_high_spells <- TRUE
  pros_low_spells <- TRUE
  
  interpolation_bool = FALSE
  spatial_buffer_bool =  TRUE
  spatial_buffer_radius = 0
  threshold <- 1  # additional threshold in C on top of MMM to calculate DHW, high spells and low spells
  
  
  # Testing

  # Data upload
  print('loading testing data')
  data <- read_and_check_files(example_rds_file, shiny_mode = FALSE)
  data <- read_and_check_files(example_csv_file, shiny_mode = FALSE)
  lat_input <- 'lat'  # Change to match incoming data
  lon_input <- 'long'  # Change to match incoming data
  
  if(debug.only_first_5_observations) data <- data[1:5,]
  
  # Show uploaded data
  data %>% mutate_all(as.character) %>% head(5) %>% print()
  
  # Get target data from selected variables
  target_data <- select_target_vars(data = data, 
                                    lat_var = lat_input,
                                    long_var = lon_input,
                                    date_var = date_input)
  target_data <- rename_target_vars(data = target_data, 
                                    lat_var = lat_input,
                                    long_var = lon_input,
                                    date_var = date_input)

  # Show target data
  target_data %>% mutate(Date = as.character(Date)) %>% head(5) %>% print()
  

  # Process incoming data
  data_processed <- process_incoming_data(data = data, 
                                          lat_var = lat_input,
                                          long_var = lon_input,
                                          date_var = date_input,
                                          date_format = date_format)
  
  
  # Generate expanded dates and locations table
  # Days into the past for SST and DHW
  expanded_date_location_table <- generate_expanded_date_and_location_table(data = data_processed, time_unit = 'day', units_into_past = days)
  
  # Generate expanded years, months and locations table
  expanded_date_location_table_mmm_custom_range <- generate_expanded_date_and_location_table(data = data_processed, 
                                                                                             time_unit = 'month', 
                                                                                             units_into_past = days, 
                                                                                             start_date_override = ymd(paste0(mmm_custom_start_year, '/01/01')),
                                                                                             end_date_override = ymd(paste0(mmm_custom_end_year,'/01/01')))
  
  
  
  # Filter for start of end date minus 1 year
  expanded_date_location_table_mmm_to_date  <- generate_expanded_date_and_location_table(data = data_processed, 
                                                                                         time_unit = 'year', 
                                                                                         units_into_past = years, 
                                                                                         start_date_override = ymd(paste0(mmm_to_date_start_year, '/01/01'))) %>% 
    group_by(end_date) %>%
    filter(Date < ymd(paste0(year(end_date),'01/01')))
  
  
  # Generate datestring tables for file filtering  
  # Generate SST datestring table
  url_datestring_table_sst <- generate_datestring_table(expanded_date_location_table$Date)
  
  # Generate DHW datestring table
  url_datestring_table_dhw <- generate_datestring_table(expanded_date_location_table$end_date)
  
  # MMM_custom_range datestring table
  url_datestring_table_mmm_custom_range <- generate_datestring_table(expanded_date_location_table_mmm_custom_range$Date)
  url_datestring_table_mmm_custom_range <- url_datestring_table_mmm_custom_range %>% mutate(datestring = substr(datestring, start = 1, stop = 6))  # Extract year only
  
  # MMM_to_date datestring table
  url_datestring_table_mmm_to_date <- generate_datestring_table(expanded_date_location_table_mmm_to_date$Date)
  url_datestring_table_mmm_to_date <- url_datestring_table_mmm_to_date %>% mutate(datestring = substr(datestring, start = 1, stop = 6))  # Extract year only
  
  
  
  # Load files
  if(debug.local_files) {
    
    # Load locally
    # DHW
    local_files_dhw <- filter_local_files_by_datestring(local_dir_dhw, url_datestring_table_dhw)
    
    # SST
    local_files_sst <- filter_local_files_by_datestring(local_dir_sst, url_datestring_table_sst)
    
    # MMM Climatology
    local_files_mmm_climatology <- paste0(local_dir_mmm_climatology, '/', mmm_climatology_file)
    
    # MMM Custom range
    local_files_mmm_custom_range <- filter_local_files_by_datestring(local_dir_mmm, url_datestring_table_mmm_custom_range)
    
    # MMM to date
    local_files_mmm_to_date <- filter_local_files_by_datestring(local_dir_mmm, url_datestring_table_mmm_to_date)
    
  } else {
    
    # Download data
    # DHW
    ftp_files_filtered_dhw <- get_and_filter_urls_by_datestring(url_datestring_table_dhw, dhw_url)
    dhw_data <- download_and_load_nc(ftp_files_filtered_dhw, message='Dowloading DHW data')
    
    # SST
    ftp_files_filtered_sst <- get_and_filter_urls_by_datestring(url_datestring_table_sst, sst_url)
    sst_data <- download_and_load_nc(ftp_files_filtered_sst, message='Dowloading SST data')
    
    # MMM Climatology
    ftp_files_filtered_mmm_climatology <- tibble(url = mmm_climatology_url, Date = today(), year = year(today()))
    mmm_climatology_data <- download_and_load_nc(ftp_files_filtered_mmm_climatology, message='Dowloading MMM Climatology data')
    
    # TODO MMM custom/retrospective
    
  }
  
  
  # Extract data
  # DHW
  coords_dates <- expanded_date_location_table %>%
    dplyr::select(Latitude, Longitude, end_date) %>%
    distinct() %>%
    right_join(url_datestring_table_dhw, by = c('end_date' = 'Date')) %>%
    mutate(Date = end_date)
  
  dhw_data_out <- get_raster_time_series_load_read_local(local_nc_files = local_files_dhw, 
                                                         coords_dates = coords_dates, 
                                                         message = 'Processing DHW data', 
                                                         datestring_format = 'ymd',
                                                         shiny_run = FALSE,
                                                         method=ifelse(interpolation_bool, 'bilinear', 'simple'),
                                                         if(spatial_buffer_bool) buffer=spatial_buffer_radius
                                                         ) %>%
      rename(dhw_from_noaa_layers=value)
  
  
  # SST
  coords_dates <- expanded_date_location_table
  sst_data_out <- get_raster_time_series_load_read_local(local_nc_files = local_files_sst, 
                                                         coords_dates = coords_dates, 
                                                         message = 'Processing DHW data', 
                                                         datestring_format = 'ymd',
                                                         shiny_run = FALSE,
                                                         method=ifelse(interpolation_bool, 'bilinear', 'simple'),
                                                         if(spatial_buffer_bool) buffer=spatial_buffer_radius
                                                         ) %>%
    rename(sst=value)
  
  
  # MMM Climatology recalculation
  if(mmm_climatology_bool) {
    
    coords_dates_mmm_climatology <- expanded_date_location_table %>% mutate(Date = ymd('2019/01/01')) %>% distinct()
    mmm_climatology_data_out <- get_raster_time_series_load_read_local(local_nc_files = local_files_mmm_climatology, 
                                                                       coords_dates = coords_dates_mmm_climatology, 
                                                                       message = 'Processing MMM Climatology data', 
                                                                       datestring_format = 'ymd',
                                                                       shiny_run = FALSE,
                                                                       method=ifelse(interpolation_bool, 'bilinear', 'simple'),
                                                                       if(spatial_buffer_bool) buffer=spatial_buffer_radius
    ) %>%
      rename(mmm_climatology=value)
    mmm_climatology_data_out <- mmm_climatology_data_out %>% rename(mmm_climatology_as_at = Date)
  
  }
  
  # MMM to date
  if(mmm_to_date_bool) {
    
    coords_dates <- expanded_date_location_table_mmm_to_date
    mmm_to_date_data_out <- get_raster_time_series_load_read_local(local_nc_files = local_files_mmm_to_date, 
                                                                   coords_dates = coords_dates, 
                                                                   message = 'Processing MMM data to date', 
                                                                   datestring_format = 'ym',
                                                                   shiny_run = FALSE,
                                                                   method=ifelse(interpolation_bool, 'bilinear', 'simple'),
                                                                   if(spatial_buffer_bool) buffer=spatial_buffer_radius
    ) %>%
      rename(mmm_to_date=value)
  
    
    # Get MMM per location
    mmm_to_date_data_out <- mmm_to_date_data_out %>%
      mutate(month = month(Date)) %>%
      group_by(Latitude, Longitude, end_date, month) %>%
      summarise(monthly_mean = mean(mmm_to_date),
                mmm_to_date_start_date = min(Date)) %>%
      ungroup() %>%
      group_by(Latitude, Longitude, end_date) %>%
      summarise(mmm_to_date = max(monthly_mean),
                mmm_to_date_start_date = first(mmm_to_date_start_date))
  }  
    
  
  # MMM custom range
  if(mmm_custom_range_bool) {
    
    coords_dates <- expanded_date_location_table_mmm_custom_range
    mmm_custom_range_data_out <- get_raster_time_series_load_read_local(local_nc_files = local_files_mmm_custom_range, 
                                                                        coords_dates = coords_dates, 
                                                                        message = 'Processing MMM data to date', 
                                                                        datestring_format = 'ym',
                                                                        shiny_run = FALSE,
                                                                        method=ifelse(interpolation_bool, 'bilinear', 'simple'),
                                                                        if(spatial_buffer_bool) buffer=spatial_buffer_radius
    ) %>%
      rename(mmm_custom_range=value)
    
    
    # Get MMM per location
    mmm_custom_range_data_out <- mmm_custom_range_data_out %>%
      mutate(month = month(Date),
             mmm_custom_range_years = first(paste0(year(Date),'_to_',year(end_date)))) %>%
      group_by(Latitude, Longitude, month) %>%
      summarise(mean_monthly = mean(mmm_custom_range),
                mmm_custom_range_years = first(mmm_custom_range_years)) %>%
      ungroup() %>%
      group_by(Latitude, Longitude) %>%
      summarise(mmm_custom_range = max(mean_monthly),
                mmm_custom_range_years = first(mmm_custom_range_years))
    
  }
  
  # Add MMMs to SST and calculate DHW
  sst_data_plus_mmm_out <- sst_data_out %>% 
    
    {if(mmm_climatology_bool) {
    left_join(., mmm_climatology_data_out, by = c('Latitude', 'Longitude', 'end_date')) %>%
    mutate(hotspot = sst - mmm_climatology,
           degree_heating_day_mmm_climatology = ifelse(hotspot >= threshold, hotspot, 0),
           degree_heating_week_mmm_climatology = degree_heating_day_mmm_climatology/7) 
    } else {
        .
    }} %>%
    
    {if(mmm_custom_range_bool) {
      left_join(., mmm_custom_range_data_out, by = c('Latitude', 'Longitude')) %>%
        mutate(hotspot = sst - mmm_custom_range,
               degree_heating_day_mmm_custom_range = ifelse(hotspot >= threshold, hotspot, 0),
               degree_heating_week_mmm_custom_range = degree_heating_day_mmm_custom_range/7) 
    } else {
      .
    }} %>%
    
    {if(mmm_to_date_bool) {
      left_join(., mmm_to_date_data_out, by = c('Latitude', 'Longitude', 'end_date')) %>%
        mutate(hotspot = sst - mmm_to_date,
               degree_heating_day_mmm_to_date = ifelse(hotspot >= threshold, hotspot, 0),
               degree_heating_week_mmm_to_date = degree_heating_day_mmm_to_date/7) 
    } else {
      .
    }}
    
  
  # Output
  output_sst_data <- sst_data_plus_mmm_out %>% 
      dplyr::select(-end_date) %>%
      mutate(Date = as.character(Date))
  
  # Process data 
  # SST
  sst_data_processed <- sst_data_plus_mmm_out %>% 
      dplyr::select(-Date) %>%
      group_by(Latitude, Longitude, end_date) %>%
      mutate(dhw_calculated_thres = threshold) %>%
      summarise(mean_sst = mean(sst),
                sd_sst = sd(sst),
                # bimodality_coef = bimodality_coefficient(sst),  # TODO modes unavailable from CRAN/latest r version
                # bimodality_ratio = bimodality_ratio(sst),   TODO modes unavailable from CRAN/latest r version
                
                degree_heating_week_mmm_climatology = if_else(mmm_climatology_bool, sum(degree_heating_week_mmm_climatology), NA_real_),
                mmm_climatology_as_at = if_else(mmm_climatology_bool, first(mmm_climatology_as_at), ymd(NA)),
                mmm_climatology = if_else(mmm_climatology_bool, first(mmm_climatology), NA_real_),
                
                degree_heating_week_mmm_custom_range = if_else(mmm_custom_range_bool, sum(degree_heating_week_mmm_custom_range), NA_real_),
                mmm_custom_range_years = if_else(mmm_custom_range_bool, first(mmm_custom_range_years), NA_character_),
                mmm_custom_range = if_else(mmm_custom_range_bool, first(mmm_custom_range), NA_real_),
                
                degree_heating_week_mmm_to_date = if_else(mmm_to_date_bool, sum(degree_heating_week_mmm_to_date), NA_real_),
                mmm_to_date_start_date = if_else(mmm_to_date_bool, first(mmm_to_date_start_date), ymd(NA)),
                mmm_to_date = if_else(mmm_to_date_bool, first(mmm_to_date), NA_real_),
                
                dhw_calculated_thres = first(dhw_calculated_thres)) %>%
    ungroup() %>%
    
    {if(!mmm_climatology_bool) {
      dplyr::select(., -degree_heating_week_mmm_climatology, -mmm_climatology_as_at, -mmm_climatology)
    } else {
      .
    }} %>%
    
    {if(!mmm_custom_range_bool) {
      dplyr::select(., -degree_heating_week_mmm_custom_range, -mmm_custom_range_years, mmm_custom_range)
    } else {
      .
    }} %>%
    
    {if(!mmm_to_date_bool) {
      dplyr::select(., -degree_heating_week_mmm_to_date, -mmm_to_date_start_date, -mmm_to_date)
    } else {
      .
    }}
   
  # DHW
  # Single value
  dhw_data_processed <- dhw_data_out %>%
      dplyr::select(-Date) %>%
      distinct()
  
  # High Spells
  high_spells <- sst_data_plus_mmm_out %>%
      group_by(Latitude, Longitude, end_date, mmm_climatology) %>%
      rename(Date=Date, Q=sst) %>%
      group_modify(~high.spells(.x,
                                ind.days = 5,  # separate spells within this threshold are considered to be part of a single spell
                                volume = TRUE,  # spell volume statistics
                                threshold = .x$mmm_climatology),
                   keep = TRUE) %>%  # mmm used as the threshold for spell statistics
      distinct() %>%
      ungroup() %>%
      dplyr::select(
        Latitude,
        Longitude,
        end_date,
        high_spell_event_count=n.events, 
        high_spell_duration_mean=avg.high.spell.duration,
        high_spell_duration_max=max.high.spell.duration,
        high_spell_rise_mean=avg.rise,
        high_spell_peak_mean=avg.spell.peak,
        high_spell_peak_sd=sd.spell.peak,
        high_spell_threshold = mmm_climatology
      ) %>%
    mutate_at(.vars = vars(starts_with('high_spell')) ,~ifelse(is.nan(.) | is.infinite(.), NA, .))
  
  
  # Low Spells
  low_spells <- sst_data_plus_mmm_out %>%
      group_by(Latitude, Longitude, end_date, mmm_climatology) %>%
      rename(Date=Date, Q=sst) %>%
      group_modify(~low.spells(.x,
                               volume = TRUE,  # spell volume statistics
                               threshold = .x$mmm_climatology),  # mmm used as threshold for spell statistics
                   keep = TRUE) %>% 
    distinct() %>%
      ungroup() %>%
      dplyr::select(
        Latitude,
        Longitude,
        end_date,
        low_spell_duration_mean=avg.low.spell.duration,
        low_spell_duration_max=max.low.duration,  # TODO Check original max low spell name
        low_spell_threshold = mmm_climatology
      ) %>%
    mutate_at(.vars = vars(starts_with('low_spell')) ,~ifelse(is.nan(.) | is.infinite(.), NA, .))
  
  
  # Combine data
  # Variable selection
  var_select <- c('Latitude', 'Longitude', 'end_date', 'days_into_past',
                 {if(pros_sst) colnames(sst_data_processed)},
                 {if(pros_dhw) colnames(dhw_data_processed)},
                 {if(pros_high_spells) colnames(high_spells)},
                 {if(pros_low_spells) colnames(low_spells)}
                 )
  # data joining
  all_data_out <- sst_data_processed %>%
    full_join(dhw_data_processed, by=c('Latitude', 'Longitude', 'end_date')) %>%
    full_join(high_spells, by=c('Latitude', 'Longitude', 'end_date')) %>%
    full_join(low_spells, by=c('Latitude', 'Longitude', 'end_date')) %>%
    mutate(days_into_past = days) %>%
    dplyr::select(var_select)
  # Join back to input data
  all_data_out_w_aux <- data_processed %>%
      # rename(Latitude = lat_input,
      #        Longitude = lon_input,
      #        end_date = date_input) %>%
      # mutate(end_date = as.character(date(parse_date_time(x = end_date, orders = date_format)))) %>%
      left_join(all_data_out)
  
}


# high low spells testing

# data <- data_frame(Date = as.Date(ymd('1990/01/01'):ymd('1990/04/10')),
#                    Q = rnorm(n = 100, mean = 30, sd = 5))
# hist(data$Q)
# q9 <- quantile(data$Q, 0.9)
# q1 <- quantile(data$Q, 0.1)
# 
# mmm <- 32
# threshold <- 1
# spell_days = 7
# 
# data <- data %>% mutate(mmm = mmm,
#                         q9 = q9,
#                         q1 = q1,
#                         threshold = threshold,
#                         hotspot = Q - mmm,
#                         dhd = ifelse(hotspot >= threshold, T, F),
#                         high_spell_q9_daily = ifelse(Q >= q9, 1, 0),
#                         high_spell_q9_running_sum = RcppRoll::roll_sum(high_spell_q9_daily, spell_days, fill = NA),
#                         high_spell_mmm = ifelse(Q >= mmm + threshold, T, F),
#                         low_spell_q1 = ifelse(Q <= q1, T, F),
#                         low_spell_mmm = ifelse(Q <= mmm - threshold, T, F),
#                         
#                         high_spell_q9_area = ifelse(Q >= q9, Q, q9))
# 
# 
# ggplot(data = data, aes(x = Date, y = Q)) +
#   geom_line() +
#   geom_hline(yintercept = mmm + threshold,
#              colour = 'orange',
#              linetype = 'dashed') +
#   geom_point(data = data %>% filter(high_spell_mmm == T),
#              aes(x = Date, y = Q),
#              colour = 'orange', size = 2) +
#   
#   # geom_hline(yintercept = q9,
#   #            colour = 'red',
#   #            linetype = 'dashed') +
#   geom_point(data = data %>% filter(high_spell_q9 == T),
#              aes(x = Date, y = Q),
#              colour = 'red', size = 1) +
#   geom_ribbon(data = data,
#               aes(x = Date, ymax = high_spell_q9_area, ymin = q9),
#               fill = 'red', alpha = 0.7, colour = 'red', linetype = 'dashed') +
#   
#   geom_hline(yintercept = mmm - threshold,
#              colour = 'blue',
#              linetype = 'dashed') +
#   geom_point(data = data %>% filter(low_spell_mmm == T),
#              aes(x = Date, y = Q),
#              colour = 'blue', size = 2) +
#   
#   geom_hline(yintercept = q1,
#              colour = 'cyan',
#              linetype = 'dashed') +
#   geom_point(data = data %>% filter(low_spell_q1 == T),
#              aes(x = Date, y = Q),
#              colour = 'cyan', size = 1) +
#   
#  
#   
#   theme_minimal()
# 
# 
# high.spells(flow.ts = data,
#             quant = 0.9,
#             threshold = mmm + threshold,
#             ind.days = 1)





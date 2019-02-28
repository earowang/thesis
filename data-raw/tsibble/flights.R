# src: https://github.com/hadley/nycflights13/blob/master/data-raw/flights.R
# with minor tweaks
# https://transtats.bts.gov/Prezip/On_Time_On_Time_Performance_2017_1.zip
library(tidyverse)
library(lubridate)
library(tsibble)

flight_url <- function(year = 2017, month) {
  base_url <- "http://www.transtats.bts.gov/Prezip/"
  sprintf(paste0(base_url, "On_Time_On_Time_Performance_%d_%d.zip"), year, month)
}

download_month <- function(year = 2017, month) {
  url <- flight_url(year, month)

  temp <- tempfile(fileext = ".zip")
  download.file(url, temp)

  files <- unzip(temp, list = TRUE)
  # Only extract biggest file
  csv <- files$Name[order(files$Length, decreasing = TRUE)[1]]

  unzip(temp, exdir = "data-raw/flights", junkpaths = TRUE, files = csv)

  src <- paste0("data-raw/flights/", csv)
  dst <- paste0("data-raw/flights/", year, "-", month, ".csv")
  file.rename(src, dst)
}

download_year <- function(year = 2017) {
  months <- 1:12
  needed <- paste0(year, "-", months, ".csv")
  missing <- months[!(needed %in% dir("data-raw/flights"))]
  lapply(missing, download_month, year = year)
}

download_year(year = 2017)

get_all <- function(path) {
  col_types <- cols(
    DepTime = col_integer(),
    ArrTime = col_integer(),
    CRSDepTime = col_integer(),
    CRSArrTime = col_integer(),
    Carrier = col_character(),
    UniqueCarrier = col_character()
  )
  read_csv(path, col_types = col_types) %>%
    filter(Cancelled == 0) %>% 
    select(
      year = Year, month = Month, day = DayofMonth,
      dep_time = DepTime, sched_dep_time = CRSDepTime, dep_delay = DepDelay,
      arr_time = ArrTime, sched_arr_time = CRSArrTime, arr_delay = ArrDelay,
      carrier = Carrier,  flight_num = FlightNum, tailnum = TailNum,
      origin = Origin, dest = Dest,
      air_time = AirTime, distance = Distance,
      origin_city_name = OriginCityName, origin_state = OriginState,
      origin_state_name = OriginStateName,
      dest_city_name = DestCityName, dest_state = DestState,
      dest_state_name = DestStateName,
      taxi_out = TaxiOut, taxi_in = TaxiIn,
      carrier_delay = CarrierDelay, weather_delay = WeatherDelay,
      nas_delay = NASDelay, security_delay = SecurityDelay,
      late_aircraft_delay = LateAircraftDelay
    ) %>%
    mutate(
      flight_num = paste0(carrier, flight_num),
      origin_city_name = gsub("^(.*?),.*", "\\1", origin_city_name),
      dest_city_name = gsub("^(.*?),.*", "\\1", dest_city_name),
      hour = sched_dep_time %/% 100,
      minute = sched_dep_time %% 100,
      hour_arr = sched_arr_time %/% 100,
      minute_arr = sched_arr_time %% 100,
      # local time
      sched_dep_datetime = make_datetime(year, month, day, hour, minute),
      sched_arr_datetime = make_datetime(year, month, day, hour_arr, minute_arr)
    ) %>% 
    select(
      -year, -month, -day, -hour, -minute, -dep_time, 
      -sched_dep_time, -origin_state_name, -dest_state_name,
      -arr_time, -sched_arr_time, -hour_arr, -minute_arr
    ) %>% 
    select(flight_num, sched_dep_datetime, sched_arr_datetime, everything()) %>% 
    filter(!is.na(dep_delay), !(origin_state %in% c("PR", "TT", "VI")))
}

## only look at the operating flights
flights_full <- map_dfr(dir("data-raw/flights", full.names = TRUE), get_all)

flights <- flights_full %>% 
  drop_na(air_time) # remove cancelled flights

write_rds(flights, path = "data/flights.rds", compress = "bz2")

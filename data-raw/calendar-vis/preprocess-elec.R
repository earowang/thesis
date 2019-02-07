library(tidyverse)
library(lubridate)

make_time <- function(x) {
  hrs <- ceiling(x / 2 - 1)
  mins <- if_else(x %% 2 == 0L, 30L, 0L)
  hms::hms(hours = hrs, minutes = mins)
}

sm_files <- fs::dir_ls("data-raw/calendar-vis", glob = "*1.csv")

all_data <- sm_files %>% 
  map2_dfr(
    c(0, 1, 0, 0), 
    ~ read_csv(.x, col_names = c("id", "date", 1:48), skip = .y),
    .id = "file"
  )

elec <- all_data %>% 
  filter(id == 300) %>% 
  gather(time, kwh, `1`:`48`) %>% 
  transmute(
    id = as.integer(as.factor(file)),
    date = ymd(date),
    time = make_time(as.integer(time)),
    date_time = ymd_hms(paste(date, time)),
    kwh = as.double(kwh)
  ) %>% 
  filter(date >= ymd("20170801"), date < ymd("20180801")) %>% 
  arrange(id, date_time)

readr::write_rds(elec, path = "data/calendar-vis/elec.rds", compress = "xz")

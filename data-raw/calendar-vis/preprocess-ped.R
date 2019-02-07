library(lubridate)
library(tidyverse)

ped_full <- read_csv("data-raw/02-calendar-vis/Pedestrian_traffic_-_hourly_count.csv")
ped_loc <- read_csv("data-raw/02-calendar-vis/sensor_locations.csv") %>% 
  select(`Sensor ID`, Latitude, Longitude)

pedestrian_2016 <- ped_full %>% 
  filter(Year == 2016) %>% 
  select(-ID) %>% 
  mutate(
    Date_Time = dmy_hm(Date_Time, tz = "Australia/Brisbane"),
    Date = as_date(Date_Time),
    Month = month(Date_Time, abbr = FALSE, label = TRUE),
    Day = wday(Date_Time, abbr = FALSE, label = TRUE, week_start = 1)
  ) %>% 
  arrange(Sensor_ID, Date_Time)
# remove duplicates
pedestrian_2016 <- pedestrian_2016[!duplicated(pedestrian_2016), ]
# join sensor latlon
pedestrian_2016 <- pedestrian_2016 %>% 
  left_join(ped_loc, by = c("Sensor_ID" = "Sensor ID"))

write_rds(pedestrian_2016, 
 path = "data/02-calendar-vis/pedestrian-2016.rds",
 compress = "xz")

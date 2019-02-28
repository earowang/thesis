library(bomrang)
library(tidyverse)
library(lubridate)

newcastle_max <- get_historical(stationid = "061055", type = "max")
newcastle_min <- get_historical(stationid = "061055", type = "min")
newcastle_max13 <- newcastle_max %>% 
  as_tibble() %>%
  filter(Year == 2013) %>% 
  transmute(date = make_date(Year, Month, Day), max_temp = Max_temperature)
newcastle_min13 <- newcastle_min %>% 
  as_tibble() %>%
  filter(Year == 2013) %>% 
  transmute(date = make_date(Year, Month, Day), min_temp = Min_temperature)
newcastle_temp13 <- newcastle_max13 %>% 
  left_join(newcastle_min13)
write_rds(newcastle_temp13, "data/weather13.rds")

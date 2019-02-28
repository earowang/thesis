library(tidyverse)
library(lubridate)

# tz(smart_meter$READING_DATETIME) # UTC

customer <- read_csv("data-raw/customer.csv") %>% 
  rename_all(tolower)
# customer_nc <- read_rds("data/customer_newcastle.rds")
# customer_newcastle <- customer %>% 
#   rename_all(tolower) %>% 
#   filter(customer_key %in% customer_nc$customer_key)
#
write_rds(customer, "data/customer.rds", compress = "xz")

smart_meter <- read_csv("data/smart-meter.csv")
smart_meter13 <- smart_meter %>% 
  rename_all(tolower) %>%
  filter(
    reading_datetime < ymd("20140101"), 
    reading_datetime >= ymd_h("20130101 00"),
    customer_id %in% customer_newcastle$customer_key
  )

smart_meter13 <- smart_meter13 %>% 
  group_by(customer_id) %>% 
  mutate(reading_datetime = case_when(
    duplicated(reading_datetime) ~ reading_datetime + hours(1),
    TRUE ~ reading_datetime
  ))

smart_meter13 <- smart_meter13 %>% 
  ungroup() %>%
  select(-calendar_key) %>% 
  arrange(customer_id, reading_datetime)

write_rds(smart_meter13, "data/smart-meter13.rds", compress = "xz")

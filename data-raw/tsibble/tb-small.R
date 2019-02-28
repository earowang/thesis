library(tidyverse)

tb <- read_csv("data-raw/tb-2018-06-05.csv")

tb_small <- tb %>% 
  select(-region, -age) %>% 
  filter(
    country %in% c("Australia", "New Zealand", "United States of America")
  ) %>% 
  group_by(country, continent, gender, year) %>% 
  summarise(count = sum(count)) %>% 
  ungroup()

write_rds(tb_small, "data/tb-small.rds")

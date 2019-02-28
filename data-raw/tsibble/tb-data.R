# download and clean WHO Tuberculosis (TB)
# source: http://www.who.int/tb/country/data/download/en/
library(tidyverse)
library(countrycode)

tb_raw <- read_csv("https://extranet.who.int/tme/generateCSV.asp?ds=notifications")

tb_tidy <- tb_raw %>% 
  select(country, year, new_sp_m04:new_sp_fu) %>%
  gather(sp, count, new_sp_m04:new_sp_fu) %>%
  drop_na(count) %>% 
  separate(sp, c("new", "sp", "genderage")) %>%
  mutate(
    gender = substr(genderage, 1, 1), 
    gender = if_else(gender == "m", "Male", "Female"),
    age = substring(genderage, 2)
  ) %>% 
  select(-genderage, -new, -sp) %>% 
  filter(!(age %in% c("04", "014", "514", "u")))

age_tmp <- strsplit(gsub("[:digit:]", "", tb_tidy$age), "")
age_vec <- map_chr(age_tmp, function(x) {
    y <- paste0(x[1:2], collapse = "")
    if (y != "65") {
      return(sprintf("%s-%s", y, paste0(x[3:4], collapse = "")))
    }
    y
  })

tb_tidy <- tb_tidy %>% 
  mutate(age = age_vec)

country <- unique(tb_tidy %>% pull(country))
ctr_df <- tibble(
  country = country,
  region = countrycode(country, "country.name", "region"),
  continent = countrycode(country, "country.name", "continent")
)

tb <- tb_tidy %>% 
  left_join(ctr_df, by = "country") %>% 
  filter(!is.na(continent)) %>% 
  select(country, region, continent, gender, age, year, count)

write_csv(tb, path = paste0("data/tb-", Sys.Date(), ".csv"))

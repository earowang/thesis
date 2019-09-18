## ---- pedestrian-load
library(tidyverse)
library(lubridate)
library(tsibble)
library(mists)

pedestrian_2016 <- read_rds("data/calendar-vis/pedestrian-2016.rds")

pedestrian_2016 <- pedestrian_2016 %>% 
  as_tsibble(key = Sensor_Name, index = Date_Time) %>% 
  fill_gaps(.full = TRUE) %>% 
  mutate(
    Sensor_Name = fct_reorder(Sensor_Name, Hourly_Counts, function(x) sum(is.na(x))))

na_runs_df <- pedestrian_2016 %>%
  as_tibble() %>% 
  group_by(Sensor_Name) %>%
  summarise(na_runs = list_of_na_rle(Hourly_Counts, Date_Time))

## ---- pedestrian-rangeplot
na_runs_df %>% 
  na_rle_rangeplot(x = na_runs, y = Sensor_Name, size = 0.5) +
  xlab("Date time")

spencer_nm <- "Spencer St-Collins St (South)"
spencer <- pedestrian_2016 %>% 
  filter(Sensor_Name == spencer_nm) %>% 
  mutate(flag_na = is.na(Hourly_Counts))

## ---- spencer-spinoplot
na_runs_df %>% 
  filter(Sensor_Name == spencer_nm) %>% 
  na_rle_spinoplot(x = na_runs) +
  xlab("runs [frequency]") +
  theme(strip.text.x = element_blank())

spencer_aug <- as.ts(spencer, value = Hourly_Counts) %>% 
  na_seasplit(algorithm = "kalman") %>% 
  as_tsibble() %>% 
  mutate(index = spencer$Date_Time, flag_na = spencer$flag_na) %>% 
  rename(Date_Time = index, Hourly_Counts = value) %>% 
  filter_index("2016 Aug" ~ .)

## ---- spencer-jailbird
ggplot() +
  layer_na_rle(na_runs, data = filter(na_runs_df, Sensor_Name == spencer_nm), alpha = 0.3) +
  geom_line(aes(x = Date_Time, y = Hourly_Counts), data = filter_index(spencer, "2016 Aug" ~ .), size = 0.3) +
  geom_point(aes(x = Date_Time, y = Hourly_Counts), data = filter(spencer_aug, flag_na), colour = "#f03b20", size = 0.3) +
  xlab("Date time") +
  ylab("Hourly Counts")

## ---- spencer-imputes
hol16 <- holiday_aus(2016, state = "VIC") %>% 
  bind_rows(tibble(holiday = "AFL", date = ymd("20160930")))
workday <- fct_inorder(c("Work day", "Non-work day"))
spencer_aug %>% 
  mutate(Date = as_date(Date_Time)) %>% 
  left_join(hol16, by = c("Date" = "date")) %>% 
  mutate(
    flag_na = fct_relevel(as.factor(flag_na), c("TRUE", "FALSE")),
    Hour = hour(Date_Time),
    Day = wday(Date_Time, label = TRUE, week_start = 1),
    Workday = if_else(
      (Date %in% hol16$date) | Day %in% c("Sat", "Sun"),
      workday[2], workday[1])
  ) %>% 
  ggplot() +
  geom_line(
    aes(x = Hour, y = Hourly_Counts, group = Date, colour = flag_na),
    alpha = 0.6
  ) +
  scale_colour_manual(values = c(`TRUE` = "#af8dc3", `FALSE` = "#7fbf7b")) +
  scale_x_continuous(breaks = seq(0, 24, 6)) +
  facet_grid(~ Workday) +
  xlab("Time") +
  ylab("Hourly Counts") +
  guides(colour = guide_legend(title = "Imputed")) +
  theme(legend.position = "bottom")

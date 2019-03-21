## ---- load-elec
library(tidyverse)
library(lubridate)
library(tsibble)

elec <- read_rds("data/tsibble/smart-meter13.rds")
weather <- read_rds("data/tsibble/weather13.rds") %>% 
  group_by(date) %>% 
  mutate(avg_temp = mean(c(max_temp, min_temp))) %>% 
  ungroup() %>% 
  mutate(hot = if_else(avg_temp > 25, "Hot day", "Not hot day"))

elec_ts <- elec %>% 
  build_tsibble(
    key = id(customer_id), index = reading_datetime,
    validate = FALSE, ordered = TRUE
  )

## ---- elec-gaps
gap_df <- has_gaps(elec_ts)
# sum(gap_df$.gaps) / NROW(gap_df)

## ---- count-gaps
count_na_df <- elec_ts %>% 
  count_gaps()

count_na_df %>% 
  mutate(
    customer_id = as.factor(customer_id) %>% 
      fct_lump(50) %>% fct_reorder(.n, sum)
  ) %>% 
  ggplot(aes(x = customer_id)) +
  geom_linerange(aes(ymin = .from, ymax = .to)) +
  geom_point(aes(y = .from), size = 0.6, shape = 4) +
  geom_point(aes(y = .to), size = 0.6, shape = 4) +
  coord_flip() +
  xlab("Customer") +
  ylab("Time gaps") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )

## ---- calendar-plot
library(sugrrants)
elec_cal <- elec_ts %>% 
  summarise(avg = mean(general_supply_kwh)) %>% 
  mutate(
    date = as_date(reading_datetime), 
    time = hms::as.hms(reading_datetime, tz = "UTC")
  ) %>% 
  left_join(weather, by = "date") %>% 
  frame_calendar(x = time, y = avg, date = date)

p_cal <- elec_cal %>% 
  ggplot(aes(.time, .avg, group = date, colour = hot)) +
  geom_line() +
  scale_colour_brewer(palette = "Dark2", direction = -1) +
  theme(legend.position = "bottom")
prettify(p_cal)

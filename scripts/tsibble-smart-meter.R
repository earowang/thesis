## ---- load-elec
library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)
library(patchwork)
theme_set(theme_bw())

elec <- read_rds("data/tsibble/smart-meter13.rds")
weather <- read_rds("data/tsibble/weather13.rds") %>% 
  group_by(date) %>% 
  mutate(avg_temp = mean(c(max_temp, min_temp))) %>% 
  ungroup() %>% 
  mutate(hot = if_else(avg_temp > 25, "Hot day", "Not hot day"))

elec_ts <- elec %>% 
  build_tsibble(
    key = customer_id, index = reading_datetime,
    validate = FALSE, ordered = TRUE
  )

## ---- elec-gaps
gap_df <- has_gaps(elec_ts)
# sum(gap_df$.gaps) / NROW(gap_df)

## ---- count-gaps
count_na_df <- elec_ts %>% 
  count_gaps()

lumped_na_df <- count_na_df %>% 
  mutate(
    customer_id = as.factor(customer_id) %>% 
      fct_lump(50) %>% fct_reorder(.n, sum)
  ) 

p_49 <- lumped_na_df %>% 
  filter(customer_id != "Other") %>% 
  ggplot(aes(x = customer_id)) +
  geom_linerange(aes(ymin = .from, ymax = .to)) +
  geom_point(aes(y = .from), size = 0.6, shape = 4) +
  geom_point(aes(y = .to), size = 0.6, shape = 4) +
  coord_flip() +
  xlab("Top customers") +
  ylab("") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0, 0, -1, -1), "line")
  )

p_other <- lumped_na_df %>% 
  filter(customer_id == "Other") %>% 
  ggplot(aes(x = customer_id)) +
  geom_linerange(aes(ymin = .from, ymax = .to), alpha = 0.1) +
  geom_point(aes(y = .from), size = 1.2, shape = 4, alpha = 0.1) +
  geom_point(aes(y = .to), size = 1.2, shape = 4, alpha = 0.1) +
  coord_flip() +
  xlab("Rest") +
  ylab("Time gaps") +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )

p_49 + p_other + plot_layout(ncol = 1, heights = c(10, 1))

## ---- calendar-plot
library(sugrrants)
elec_full <- elec_ts %>% 
  summarise(avg = mean(general_supply_kwh)) %>% 
  mutate(
    date = as_date(reading_datetime), 
    time = hms::as_hms(reading_datetime)
  ) %>% 
  left_join(weather, by = "date")

elec_cal <- elec_full %>% 
  frame_calendar(x = time, y = avg, date = date)

p_cal <- elec_cal %>% 
  ggplot(aes(.time, .avg, group = date, colour = hot)) +
  geom_line() +
  scale_colour_brewer(palette = "Dark2", direction = -1, name = "") +
  theme(legend.position = "bottom")
prettify(p_cal)

## ---- elec-model
elec_train <- elec_full %>% 
  filter_index("2013-12-01" ~ "2013-12-30")
elec_test <- elec_full %>% 
  filter_index("2013-12-31" ~ .)

elec_fc <- elec_train %>% 
  model(
    `temperature` = ARIMA(log(avg) ~ avg_temp),
    `w/o temperature` = ARIMA(log(avg))
  ) %>% 
  forecast(new_data = elec_test)

## ---- elec-forecast
elec_fc %>% 
  autoplot(data = filter_index(elec_train, "2013-12-19" ~ .), level = NULL) +
  geom_line(aes(y = avg), data = elec_test, linetype = "dashed") +
  scale_colour_brewer(palette = "Dark2", direction = -1, name = "") +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%b %d") +
  theme(legend.position = "bottom") +
  xlab("Reading time") +
  ylab("Average electricity use")

## ---- elec-accuracy
accuracy(elec_fc, elec_test) %>% 
  rename(model = .model) %>% 
  select(-.type, -MASE, -ACF1) %>% 
  knitr::kable(digits = 3, booktabs = TRUE, caption = "(ref:elec-accuracy)", linesep = "") %>%
  kableExtra::kable_styling(position = "center", latex_options= "hold_position")

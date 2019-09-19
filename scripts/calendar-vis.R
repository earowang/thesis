## ---- load-02
library(ggmap)
library(forcats)
library(lubridate)
library(tidyverse)
library(sugrrants)
library(showtext)
library(tsibble)
theme_set(theme_bw())

# loading pedestrian data
pedestrian_2016 <- read_rds("data/calendar-vis/pedestrian-2016.rds")

hol16 <- holiday_aus(2016, state = "VIC") %>% 
  bind_rows(tibble(holiday = "AFL", date = ymd("20160930")))
workday <- fct_inorder(c("Work day", "Non-work day"))
# turning implicit missingness to explicit
pedestrian_2016 <- pedestrian_2016 %>% 
  as_tsibble(key = Sensor_Name, index = Date_Time) %>% 
  group_by(Sensor_Name) %>% 
  fill_gaps(.full = TRUE) %>% 
  ungroup() %>% 
  mutate(
    Year = year(Date_Time),
    Day = wday(Date_Time, label = TRUE, week_start = 1),
    Time = hour(Date_Time),
    Date = as_date(Date_Time),
    Workday = if_else(
      (Date %in% hol16$date) | Day %in% c("Sat", "Sun"),
      workday[2], workday[1])
  )

# selected sensors
sensors <- c("State Library", "Flagstaff Station", "Birrarung Marr")

## ---- ped-map
# plotting the sensor locations using ggmap
ped_loc <- pedestrian_2016 %>% 
  filter(!is.na(Latitude)) %>% 
  distinct(Longitude, Latitude, Sensor_Name) %>% 
  mutate(
    Highlight = if_else(Sensor_Name %in% sensors, Sensor_Name, "Other"),
    Highlight = factor(Highlight, levels = c(sensors, "Other")),
    Selected = if_else(Sensor_Name %in% sensors, TRUE, FALSE)
  )
# melb_map <- get_map(
#   location = c(
#     min(ped_loc$Longitude), min(ped_loc$Latitude),
#     max(ped_loc$Longitude), max(ped_loc$Latitude)
#   ),
#   zoom = 14)
# write_rds(melb_map, "data/melb_map.rds")

# display better in grayscale
sensor_cols <- c(
  "State Library" = "#5e3c99", 
  "Flagstaff Station" = "#b2abd2", 
  "Birrarung Marr" = "#e66101"
) # 4-class PuOr without #fdb863
melb_map <- read_rds("data/calendar-vis/melb-map.rds")
selected <- ped_loc %>% 
  filter(Selected)
nonselected <- ped_loc %>% 
  filter(!Selected)
ggmap(melb_map) +
  geom_point(
    data = nonselected, aes(x = Longitude, y = Latitude),
    colour = "grey70", alpha = 0.8, size = 3
  ) +
  geom_point(
    data = selected, aes(x = Longitude, y = Latitude, colour = Highlight),
    size = 6, shape = 17
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_colour_manual(name = "Sensor", values = sensor_cols, guide = "legend") +
  theme(legend.position = "bottom")

## ---- time-series-plot
# subsetting the data
subdat <- pedestrian_2016 %>% 
  filter(Sensor_Name %in% sensors) %>% 
  mutate(Sensor_Name = fct_reorder(Sensor_Name, -Latitude, na.rm = TRUE))
# conventional time series plot
subdat %>% 
  ggplot(aes(x = Date_Time, y = Hourly_Counts, colour = Sensor_Name)) +
  geom_line(size = 0.3) +
  facet_grid(
    Sensor_Name ~ ., 
    labeller = labeller(Sensor_Name = label_wrap_gen(20))
  ) +
  scale_colour_manual(name = "Sensor", values = sensor_cols, guide = "legend") +
  scale_x_datetime(date_labels = "%d %b %Y", date_minor_breaks = "1 month") +
  theme(legend.position = "none") +
  xlab("Date Time") +
  ylab("Hourly Counts")

## ---- facet-time
# time series plot faceted by sensors and day of week
subdat %>% 
  ggplot(aes(x = Time, y = Hourly_Counts, group = Date, colour = Sensor_Name)) +
  geom_line(size = 0.3) +
  facet_grid(
    Sensor_Name ~ Day, 
    labeller = labeller(Sensor_Name = label_wrap_gen(20))
  ) +
  scale_x_continuous(breaks = seq(0, 24, by = 6)) +
  scale_colour_manual(name = "Sensor", values = sensor_cols, guide = "legend") +
  theme(legend.position = "none") +
  xlab("Time") +
  ylab("Hourly Counts")

## ---- fs-2016
rdbu <- c("Work day" = "#d7191c", "Non-work day" = "#2c7bb6")
# calendar plot for Flagstaff station
fs <- subdat %>% 
  filter(Sensor_Name == "Flagstaff Station")

fs_cal <- fs %>%
  frame_calendar(x = Time, y = Hourly_Counts, date = Date)

p_fs <- fs_cal %>% 
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date, colour = Workday)) +
  geom_line() +
  scale_color_manual(values = rdbu) +
  theme(legend.position = "bottom")
prettify(p_fs)

## ---- fs-free
# calendar plot for fs street station using local scale
fs_cal_free <- fs %>% 
  frame_calendar(x = Time, y = Hourly_Counts, date = Date, scale = "free")

p_fs_free <- fs_cal_free %>% 
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date, colour = Workday)) +
  geom_line() +
  scale_color_manual(values = rdbu) +
  theme(legend.position = "bottom")
prettify(p_fs_free)

## ---- fs-polar
# calendar plot for fs street station in polar coordinates
fs_polar <- fs %>% 
  frame_calendar(x = Time, y = Hourly_Counts, date = Date, polar = TRUE)

p_fs_polar <- fs_polar %>% 
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date, colour = Workday)) +
  geom_path() +
  scale_color_manual(values = rdbu) +
  theme(legend.position = "bottom")
prettify(p_fs_polar)

## ---- overlay
# overlaying calendar plots 
subset_cal <- subdat %>% 
  frame_calendar(Time, Hourly_Counts, Date)

sensor_cols2 <- c(
  "#5e3c99" = "#5e3c99", 
  "#b2abd2" = "#b2abd2", 
  "#e66101" = "#e66101"
) # 4-class PuOr without #fdb863
p_three <- subset_cal %>% 
  ggplot() +
  geom_line(
    data = filter(subset_cal, Sensor_Name == sensors[1]),
    aes(.Time, .Hourly_Counts, group = Date, colour = sensor_cols2[1])
  ) +
  geom_line(
    data = filter(subset_cal, Sensor_Name == sensors[2]),
    aes(.Time, .Hourly_Counts, group = Date, colour = sensor_cols2[2])
  ) +
  geom_line(
    data = filter(subset_cal, Sensor_Name == sensors[3]),
    aes(.Time, .Hourly_Counts, group = Date, colour = sensor_cols2[3])
  ) +
  scale_colour_identity(
    name = "Sensor",
    breaks = names(sensor_cols2),
    labels = c(
      "State Library", 
      "Flagstaff Station",
      "Birrarung Marr"
    ),
    guide = "legend"
  ) +
  theme(legend.position = "bottom")
prettify(p_three)

## ---- facet
# calendar plots faceted by the sensors
facet_cal <- subdat %>% 
  group_by(Sensor_Name) %>% 
  frame_calendar(x = Time, y = Hourly_Counts, date = Date, nrow = 2)

p_facet <- facet_cal %>% 
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date)) +
  geom_line(aes(colour = Sensor_Name)) +
  facet_grid(
    Sensor_Name ~ ., 
    labeller = labeller(Sensor_Name = label_wrap_gen(20))
  ) +
  scale_colour_manual(name = "Sensor", values = sensor_cols) +
  theme(legend.position = "none")
prettify(p_facet, size = 3, label.padding = unit(0.1, "lines"))

## ---- scatterplot
# lagged scatterplot for fs street station in the daily calendar format
fs_cal_day <- fs %>% 
  mutate(Lagged_Counts = dplyr::lag(Hourly_Counts)) %>% 
  frame_calendar(x = Lagged_Counts, y = Hourly_Counts, date = Date, 
    calendar = "daily", width = 0.8, height = 0.8, scale = "free")

p_fs_day <- fs_cal_day %>% 
  ggplot(
    aes(x = .Lagged_Counts, y = .Hourly_Counts, group = Date, colour = Workday)
  ) +
  geom_point(size = 0.5, alpha = 0.6) +
  scale_color_manual(values = rdbu) +
  theme(legend.position = "bottom")
prettify(p_fs_day, size = 3, label.padding = unit(0.15, "lines"))

## ---- chn
# boxplots for hourly counts across all the sensors in 2016 Dec with Chinese 
# labels
font_install(source_han_serif())
showtext_auto()
# boxplots for hourly counts across all the sensors in 2016 December
pedestrian_dec <- pedestrian_2016 %>% 
  filter(Month == "December") %>% 
  frame_calendar(
    x = Time, y = Hourly_Counts, date = Date, width = 0.97, height = 0.97
  )
p_boxplot <- pedestrian_dec %>% 
  ggplot() +
  geom_boxplot(
    aes(x = .Time, y = .Hourly_Counts, group = Date_Time),
    outlier.size = 0.3, width = 0.004, position = "identity",
    colour = "grey50"
  ) +
  geom_smooth(
    aes(.Time, .Hourly_Counts, group = Date), 
    se = FALSE, method = "loess"
  )
p_chn <- prettify(
  p_boxplot, locale = "zh", abbr = FALSE, 
  size = 3, label.padding = unit(0.15, "lines"),
  label = c("label", "text", "text2"),
  family = "source-han-serif-cn"
)
p_chn
# ggsave("figure/chn-1.pdf", p_chn, width = 8, height = 8)
# showtext_auto(FALSE) 

## ---- facet-calendar
subdat %>% 
  filter_index(~ "2016-04") %>% 
  ggplot(aes(x = Time, y = Hourly_Counts, colour = Sensor_Name)) +
  geom_line() +
  facet_calendar(~ Date) +
  scale_x_continuous(breaks = c(6, 18)) +
  scale_colour_manual(name = "Sensor", values = sensor_cols, guide = "legend") +
  theme(strip.text.x = element_text(size = 7), legend.position = "bottom") +
  xlab("Time") +
  ylab("Hourly Counts")

## ---- load-elec
elec <- read_rds("data/calendar-vis/elec.rds") %>% 
  filter(date >= ymd("20180101"), date < ymd("20180701"))
rdbl <- c("Weekday" = "#d7191c", "Weekend" = "#2c7bb6")

## ---- elec-line
elec %>% 
  ggplot(aes(x = date_time, y = kwh)) +
  geom_line() +
  facet_wrap(~ id, labeller = label_both, ncol = 1)

## ---- dow
elec <- elec %>% 
  mutate(
    wday = wday(date, label = TRUE, week_start = 1),
    weekday = if_else(wday %in% c("Sat", "Sun"), "Weekend", "Weekday")
  )

elec %>% 
  group_by(date, wday, id) %>% 
  summarise(kwh = sum(kwh, na.rm = TRUE)) %>% 
  ggplot(aes(x = wday, y = kwh)) +
  geom_boxplot(colour = "black", outlier.shape = 8) +
  # lvplot::geom_lv(aes(fill = ..LV..), colour = "black", outlier.shape = 8) +
  facet_wrap(~ id, labeller = label_both) +
  xlab("Day of week") +
  ylab("kWh") +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  theme(legend.position = "bottom")

## ---- hod
avg_elec <- elec %>% 
  group_by(id, weekday, time) %>% 
  summarise(avg = mean(kwh))

ggplot(elec, aes(x = time, y = kwh)) +
  geom_point(alpha = 0.5, size = 0.1) +
  geom_line(aes(y = avg, colour = as.factor(id)), data = avg_elec, size = 1.5) +
  facet_grid(weekday ~ id) +
  scale_colour_brewer(palette = "PiYG") +
  scale_x_time(
    breaks = hms::hms(hours = seq(0, 24, by = 6)),
    labels = seq(0, 24, by = 6)
  ) +
  xlab("Time of day") +
  ylab("kWh") +
  guides(colour = "none")

## ---- calendar-elec
p_cal_elec <- elec %>% 
  frame_calendar(x = time, y = kwh, date = date, nrow = 1) %>% 
    ggplot(aes(x = .time, y = .kwh, group = date)) +
    geom_line(aes(colour = as.factor(id)), size = 0.5) +
    scale_colour_brewer(name = "", palette = "PiYG") +
    facet_grid(id ~ ., labeller = label_both) +
    theme(legend.position = "none")
prettify(p_cal_elec, size = 2.5, label.padding = unit(0.1, "lines"))

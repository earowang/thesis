## ---- load-02
library(ggmap)
library(forcats)
library(lubridate)
library(tidyverse)
library(sugrrants)
library(tsibble)
theme_set(theme_bw())

# loading pedestrian data
pedestrian_2016 <- read_rds("data/calendar-vis/pedestrian-2016.rds")

hol16 <- holiday_aus(2016, state = "VIC") %>% 
  bind_rows(tibble(holiday = "AFL", date = ymd("20160930")))
workday <- fct_inorder(c("Work day", "Non-work day"))
# turning implicit missingness to explicit
pedestrian_2016 <- pedestrian_2016 %>% 
  as_tsibble(key = id(Sensor_Name), index = Date_Time) %>% 
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
# write_rds(melb_map, "data/calendar-vis/melb-map.rds", compress = "xz")

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
    size = 6
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_colour_brewer(palette = "Dark2", name = "Sensor") +
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
  scale_colour_brewer(name = "Sensor", palette = "Dark2") +
  theme(legend.position = "bottom") +
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
  scale_x_continuous(breaks = seq(6, 23, by = 6)) +
  scale_colour_brewer(palette = "Dark2", name = "Sensor") +
  theme(legend.position = "bottom") +
  xlab("Time") +
  ylab("Hourly Counts")

## ---- fs-2016
puor <- c("Work day" = "#f1a340", "Non-work day" = "#998ec3")
# calendar plot for Flagstaff station
fs <- subdat %>% 
  filter(Sensor_Name == "Flagstaff Station")

fs_cal <- fs %>%
  frame_calendar(x = Time, y = Hourly_Counts, date = Date)

p_fs <- fs_cal %>% 
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date, colour = Workday)) +
  geom_line() +
  scale_color_manual(values = puor) +
  theme(legend.position = "bottom")
prettify(p_fs)

## ---- fs-free
# calendar plot for fs street station using local scale
fs_cal_free <- fs %>% 
  frame_calendar(x = Time, y = Hourly_Counts, date = Date, scale = "free")

p_fs_free <- fs_cal_free %>% 
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date, colour = Workday)) +
  geom_line() +
  scale_color_manual(values = puor) +
  theme(legend.position = "bottom")
prettify(p_fs_free)

## ---- fs-polar
# calendar plot for fs street station in polar coordinates
fs_polar <- fs %>% 
  frame_calendar(x = Time, y = Hourly_Counts, date = Date, polar = TRUE)

p_fs_polar <- fs_polar %>% 
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date, colour = Workday)) +
  geom_path() +
  scale_color_manual(values = puor) +
  theme(legend.position = "bottom")
prettify(p_fs_polar)

## ---- overlay
# overlaying calendar plots 
subset_cal <- subdat %>% 
  frame_calendar(Time, Hourly_Counts, Date)

sensor_cols <- c(
  "#1b9e77" = "#1b9e77", 
  "#d95f02" = "#d95f02", 
  "#7570b3" = "#7570b3"
) # Dark2
p_three <- subset_cal %>% 
  ggplot() +
  geom_line(
    data = filter(subset_cal, Sensor_Name == sensors[1]),
    aes(.Time, .Hourly_Counts, group = Date, colour = sensor_cols[1])
  ) +
  geom_line(
    data = filter(subset_cal, Sensor_Name == sensors[2]),
    aes(.Time, .Hourly_Counts, group = Date, colour = sensor_cols[2])
  ) +
  geom_line(
    data = filter(subset_cal, Sensor_Name == sensors[3]),
    aes(.Time, .Hourly_Counts, group = Date, colour = sensor_cols[3])
  ) +
  scale_colour_identity(
    name = "Sensor",
    breaks = names(sensor_cols),
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
  scale_colour_brewer(palette = "Dark2", name = "Sensor") +
  theme(legend.position = "bottom")
prettify(p_facet, size = 3, label.padding = unit(0.1, "lines"))

## ---- scatterplot
# lagged scatterplot for fs street station in the daily calendar format
fs_cal_day <- fs %>% 
  mutate(Lagged_Counts = dplyr::lag(Hourly_Counts)) %>% 
  frame_calendar(x = Lagged_Counts, y = Hourly_Counts, date = Date, 
    calendar = "daily", width = 0.95, height = 0.8)

p_fs_day <- fs_cal_day %>% 
  ggplot(
    aes(x = .Lagged_Counts, y = .Hourly_Counts, group = Date, colour = Workday)
  ) +
  geom_point(size = 0.5, alpha = 0.6) +
  scale_color_manual(values = puor) +
  theme(legend.position = "bottom")
prettify(p_fs_day, size = 3, label.padding = unit(0.15, "lines"))

## ---- boxplot
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
prettify(p_boxplot, label = c("label", "text", "text2"))

## ---- chn
# boxplots for hourly counts across all the sensors in 2016 Dec with Chinese 
# labels
showtext_auto()
prettify(
  p_boxplot, locale = "zh", abbr = FALSE, 
  size = 3, label.padding = unit(0.15, "lines"),
  label = c("label", "text", "text2"),
  family = "STKaiti"
)
showtext_auto(FALSE) 

## ---- load-elec
elec <- read_rds("data/calendar-vis/elec.rds")
puor <- c("Weekday" = "#f1a340", "Weekend" = "#998ec3")

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
  lvplot::geom_lv(aes(fill = ..LV..), colour = "black", outlier.shape = 8) +
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
  geom_line(aes(y = avg, colour = as.factor(id)), data = avg_elec, size = 1) +
  facet_grid(weekday ~ id) +
  scale_colour_brewer(palette = "Dark2") +
  scale_x_time(breaks = hms::hms(hours = c(6, 18))) +
  xlab("Time of day") +
  ylab("kWh") +
  guides(colour = "none")

## ---- h1
h1 <- elec %>% 
  filter(id == 1) %>% 
  frame_calendar(x = time, y = kwh, date = date) %>% 
    ggplot(aes(x = .time, y = .kwh, group = date)) +
    geom_line(aes(colour = weekday)) +
    scale_color_manual(name = "", values = puor) +
    theme(legend.position = "bottom")
prettify(h1)

## ---- h2
h2 <- elec %>% 
  filter(id == 2) %>% 
  frame_calendar(x = time, y = kwh, date = date) %>% 
    ggplot(aes(x = .time, y = .kwh, group = date)) +
    geom_line(aes(colour = weekday)) +
    scale_color_manual(name = "", values = puor) +
    theme(legend.position = "bottom")
prettify(h2)

## ---- h3
h3 <- elec %>% 
  filter(id == 3) %>% 
  frame_calendar(x = time, y = kwh, date = date) %>% 
    ggplot(aes(x = .time, y = .kwh, group = date)) +
    geom_line(aes(colour = weekday)) +
    scale_color_manual(name = "", values = puor) +
    theme(legend.position = "bottom")
prettify(h3)

## ---- h4
h4 <- elec %>% 
  filter(id == 4) %>% 
  frame_calendar(x = time, y = kwh, date = date) %>% 
    ggplot(aes(x = .time, y = .kwh, group = date)) +
    geom_line(aes(colour = weekday)) +
    scale_color_manual(name = "", values = puor) +
    theme(legend.position = "bottom")
prettify(h4)

## ---- load-flights-pkgs
library(lubridate)
library(tidyverse)
library(tsibble)
library(forcats)

## ---- load-flights
flights <- read_rds("data/tsibble/flights.rds")

## ---- find-duplicate
flights %>% 
  duplicates(key = id(flight_num), index = sched_dep_datetime) %>% 
  as.data.frame()
dup_lgl <- are_duplicated(flights, key = id(flight_num), 
  index = sched_dep_datetime, from_last = TRUE)

## ---- tsibble
us_flights <- flights %>% 
  filter(!dup_lgl) %>% 
  as_tsibble(
    key = id(flight_num), index = sched_dep_datetime,
    regular = FALSE, validate = FALSE
  )

## ---- tsibble-header
cat(format(us_flights)[1:2], sep = "\n")

## ---- carrier-delayed
delayed_carrier <- us_flights %>% 
  mutate(delayed = dep_delay > 15) %>%
  group_by(carrier) %>% 
  index_by(year = year(sched_dep_datetime)) %>% 
  summarise(
    Ontime = sum(delayed == 0),
    Delayed = sum(delayed)
  ) %>% 
  gather(delayed, n_flights, Ontime:Delayed)

## ----- carrier-mosaic
library(ggmosaic)
delayed_carrier %>% 
  mutate(carrier = fct_reorder(carrier, -n_flights)) %>% 
  ggplot() +
    geom_mosaic(aes(x = product(carrier), fill = delayed, weight = n_flights)) +
    scale_fill_brewer(palette = "Dark2", name = "Delayed") +
    scale_x_productlist(name = "Carrier") +
    scale_y_productlist(name = "Delayed") +
    theme(legend.position = "bottom")

## ---- sel-flights
sel_flights <- us_flights %>% 
  filter(origin %in% c("JFK", "SEA", "IAH", "KOA", "LAX"))

## ---- sel-delay
sel_delay <- sel_flights %>% 
  group_by(origin) %>% 
  index_by(sched_dep_date = as_date(sched_dep_datetime)) %>% 
  summarise(pct_delay = sum(dep_delay > 15) / n())

## ----- sel-monthly-1
sel_lst <- sel_delay %>% 
  mutate(yrmth = yearmonth(sched_dep_date)) %>% 
  nest(-origin, -yrmth)

## ---- sel-monthly-2
sel_monthly <- sel_lst %>% 
  group_by(origin) %>% 
  mutate(monthly_ma = slide_dbl(data,
    ~ mean(.$pct_delay), .size = 2, .bind = TRUE
  )) %>%
  unnest(key = id(origin))

## ----- sel-monthly-plot
sel_monthly %>% 
  ggplot() +
  geom_line(aes(x = sched_dep_date, y = pct_delay), colour = "grey80", size = 0.8) +
  geom_line(aes(x = yrmth, y = monthly_ma, colour = origin), size = 1) +
  facet_grid(origin ~ .) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_colour_brewer(palette = "Dark2") +
  theme(legend.position = "bottom", aspect.ratio = 0.2) +
  xlab("Date") +
  ylab("Departure delay")

## ---- quantile
hr_qtl <- us_flights %>% 
  index_by(dep_datehour = floor_date(sched_dep_datetime, "hour")) %>% 
  summarise(
    dep_delay = list(quantile(dep_delay, c(0.5, 0.8, 0.95))),
    qtl = list(paste0("qtl", c(50, 80, 95)))
  ) %>% 
  unnest(key = id(qtl)) %>% 
  mutate(
    hour = hour(dep_datehour), 
    wday = wday(dep_datehour, label = TRUE, week_start = 1),
    date = as_date(dep_datehour)
  )

## ---- draw-qtl-prep
break_cols <- c(
  "qtl95" = "#d7301f", 
  "qtl80" = "#fc8d59", 
  "qtl50" = "#fdcc8a"
)

qtl_label <- c(
  "qtl50" = "50%",
  "qtl80" = "80%", 
  "qtl95" = "95%" 
)

min_y <- hr_qtl %>% 
  filter(hour(dep_datehour) > 4) %>% 
  pull(dep_delay) %>%
  min()

## ---- draw-qtl
hr_qtl %>% 
  filter(hour(dep_datehour) > 4) %>% 
  ggplot(aes(x = hour, y = dep_delay, group = date, colour = qtl)) +
  geom_hline(yintercept = 15, colour = "#9ecae1", size = 2) +
  geom_line(alpha = 0.8) +
  facet_grid(
    qtl ~ wday, scales = "free_y", 
    labeller = labeller(qtl = as_labeller(qtl_label))
  ) +
  xlab("Time of day") +
  ylab("Depature delay") + 
  scale_x_continuous(limits = c(0, 23), breaks = seq(6, 23, by = 6)) +
  scale_colour_manual(values = break_cols, guide = FALSE) +
  expand_limits(y = min_y)

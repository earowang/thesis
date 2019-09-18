## ---- mists-load
library(tidyverse)
library(lubridate)
library(tsibble)
library(imputeTS)
library(mists)
library(vctrs)
theme_set(theme_bw())

## ---- miss-types
ts_airgap <- as_tsibble(tsAirgap)
ts_airgap_full <- as_tsibble(tsAirgapComplete)

# Types of temporal missings
miss_at_time <- function(.data) {
  idx <- pull(.data, !!index(.data))
  init <- floor(vec_size(.data) * 0.1)
  trend_idx <- idx[cumsum(rev(seq_len(init))) + init]
  mutate(.data, value = ifelse(idx %in% trend_idx, NA, value))
}

miss_at_regular <- function(.data) {
  idx <- pull(.data, !!index(.data))
  rnd_seas <- month(sample(idx, size = 1))
  mutate(.data, value = ifelse(month(idx) %in% rnd_seas, NA, value))
}

miss_at_occasional <- function(.data) {
  idx <- pull(.data, !!index(.data))
  rnd_idx <- sample(idx, size = round(vec_size(.data) * 0.1))
  mutate(.data, value = ifelse(idx %in% rnd_idx, NA, value))
}

miss_at_runs <- function(.data) {
  idx <- pull(.data, !!index(.data))
  n <- vec_size(.data)
  na_indices_lgl <- runif(n) < .1
  na_indices <- which(na_indices_lgl)
  na_lengths <- rpois(n = sum(na_indices_lgl), lambda = 5)
  lst_indices <- map2(na_lengths, na_indices, function(.x, .y) 0:(.x - 1) + .y)
  na_indices <- vec_c(!!!lst_indices)
  rnd_run <- idx[na_indices]
  mutate(.data, value = ifelse(idx %in% rnd_run, NA, value))
}

set.seed(2019)
ts_airgap_time <- miss_at_time(ts_airgap_full) %>%
  mutate(type = "Functional") %>%
  update_tsibble(key = type)
ts_airgap_regular <- miss_at_regular(ts_airgap_full) %>%
  mutate(type = "Periodic") %>%
  update_tsibble(key = type)
ts_airgap_occasional <- miss_at_occasional(ts_airgap_full) %>%
  mutate(type = "Occasions") %>%
  update_tsibble(key = type)
ts_airgap_runs <- miss_at_runs(ts_airgap_full) %>%
  mutate(type = "Runs") %>%
  update_tsibble(key = type)
ts_airgap_miss <-
  rbind(
    ts_airgap_time, ts_airgap_regular,
    ts_airgap_occasional, ts_airgap_runs
  ) %>% 
  mutate(type = fct_relevel(type, c("Occasions", "Periodic", "Functional", "Runs")))

## ---- tbl-x-line
ggplot(ts_airgap_miss, aes(x = index, y = value)) +
  geom_line() +
  geom_point(size = .5) +
  facet_grid(type ~ ., labeller = labeller(type = label_wrap_gen(20)))

## ---- na-rle-ex1
airgap_na_rle <- ts_airgap_miss %>% 
  as_tibble() %>% 
  group_by(type) %>% 
  summarise(na_runs = list_of_na_rle(value, index_by = index))

print(airgap_na_rle$na_runs[3:4])

## ---- na-rle-rangeplot
na_rle_rangeplot(airgap_na_rle, x = na_runs, y = fct_rev(type)) +
  xlim(range(ts_airgap_miss$index)) +
  xlab("Year month")

## ---- layer-na-rle
ggplot(ts_airgap_miss, aes(x = index, y = value)) +
  geom_line(na.rm = TRUE) +
  layer_na_rle(na_runs, data = airgap_na_rle, alpha = 0.5) +
  facet_grid(type ~ .)

## ---- imputets-gapsize
plotNA.gapsize(as.ts(ts_airgap_occasional), legend = FALSE)
plotNA.gapsize(as.ts(ts_airgap_runs))

## ---- na-rle-spinoplot
na_rle_spinoplot(airgap_na_rle, x = na_runs, facets = type) +
  xlab("runs [frequency]")

## ----- na-rle-spinoplot2
na_x1 <- airgap_na_rle$na_runs[[3]]
na_x2 <- airgap_na_rle$na_runs[[4]]
tibble(
  x = as_list_of(na_x1, na_x2), 
  y = as_list_of(na_x2, na_x1),
  facet = c("Occasions intersects Runs", "Runs intersects Occasions")
) %>% 
  na_rle_spinoplot(x = x, y = y, facets = facet) +
  scale_fill_manual(values = c(`TRUE` = "#af8dc3", `FALSE` = "#7fbf7b")) +
  xlab("runs [frequency]") +
  ylab("proportion of overlaps")

## ---- ignore
na_runs_wind <- nycflights13::weather %>% 
  group_by(origin) %>% 
  summarise_at(
    vars(contains("wind")), 
    ~ list_of_na_rle(., index_by = time_hour)
  )
na_runs_wind

na_runs_wind %>% 
  na_rle_spinoplot(wind_dir, wind_gust, origin)

na_runs_wind %>% 
  na_rle_spinoplot(wind_gust, wind_dir, origin, ncol = 1)

na_runs_wind %>% 
  mutate(start_wind_dir = na_rle_indices(wind_dir)) %>% 
  unnest(start_wind_dir) %>% 
  mutate(
    start_time = hour(start_wind_dir),
    start_mday = mday(start_wind_dir),
    start_wday = wday(start_wind_dir, label = TRUE, week_start = 1),
  ) %>% 
  ggplot(aes(x = start_time)) +
  geom_bar() +
  facet_wrap(~ origin)

## ---- load-mists
library(tidyverse)
library(lubridate)
library(tsibble)
library(imputeTS)
library(rlang)
library(sugrrants)
set.seed(2019)

ts_airgap <- as_tsibble(tsAirgap)
ts_airgap_full <- as_tsibble(tsAirgapComplete)

## ---- count-na
count_na <- function(.data, ...) {
  exprs <- enexprs(...)
  if (is_false(has_length(exprs, 1))) {
    abort("`count_na()` only accepts one variable.")
  }
  idx <- index(.data)
  grped_tbl <- group_by(as_tibble(.data), !!! key(.data))
  lst_out <- summarise(grped_tbl, na = list2(tbl_na(!!! exprs, !! idx)))
  idx_type <- class(lst_out[["na"]][[1]][[".from"]])
  out <- unnest(lst_out, na)
  class(out[[".from"]]) <- class(out[[".to"]]) <- idx_type
  tibble(!!! out)
}

tbl_na <- function(x, y) {
  if (!anyNA(x)) {
    tibble(.from = y[0], .to = y[0], .n = integer())
  } else {
    len_x <- length(x)
    na_lgl <- is.na(x)
    na_rle <- rle(na_lgl)
    lgl_rle <- na_rle$values
    na_idx <- na_rle$lengths
    to <- cumsum(na_idx)
    from <- c(1, to[-length(to)] + 1)
    na_nobs <- na_idx[lgl_rle]
    tibble(
      .from = y[from][lgl_rle],
      .to = y[to][lgl_rle],
      .n = na_nobs
    )
  }
}

## ---- miss-types-funs
# Types of temporal missings
miss_at_season <- function(.data) {
  idx <- eval_tidy(index(.data), .data)
  rnd_seas <- month(sample(idx, size = 1))
  mutate(
    .data,
    value = ifelse(month(idx) %in% rnd_seas, NA, value)
  )
}

miss_at_occasional <- function(.data) {
  idx <- eval_tidy(index(.data), .data)
  rnd_idx <- sample(idx, size = round(NROW(.data) * 0.03))
  mutate(
    .data,
    value = ifelse(idx %in% rnd_idx, NA, value)
  )
}

miss_at_level <- function(.data) {
  idx <- eval_tidy(index(.data), .data)
  init <- floor(NROW(.data) * 0.1)
  rnd_start <- idx[init]
  level_idx <- idx[cumsum(seq_len(init)) + init]
  mutate(
    .data,
    value = ifelse(idx %in% level_idx, NA, value)
  )
}

miss_at_runs <- function(.data) {
  idx <- eval_tidy(index(.data), .data)
  rnd_init <- sample(idx, size = ceiling(NROW(.data) * 0.01))
  rnd_len <- sample(5:(NROW(.data) * 0.1), size = length(rnd_init))
  rnd_run <- do.call("c", purrr::map2(rnd_init, rnd_len, ~ .x + seq_len(.y)))
  mutate(
    .data,
    value = ifelse(idx %in% rnd_run, NA, value)
  )
}

## ---- miss-types
ts_airgap_level <- miss_at_level(ts_airgap_full) %>%
  mutate(type = "missing at level") %>%
  update_tsibble(key = id(type))
ts_airgap_season <- miss_at_season(ts_airgap_full) %>%
  mutate(type = "missing at season") %>%
  update_tsibble(key = id(type))
ts_airgap_occasional <- miss_at_occasional(ts_airgap_full) %>%
  mutate(type = "missing at occasional") %>%
  update_tsibble(key = id(type))
ts_airgap_runs <- miss_at_runs(ts_airgap_full) %>%
  mutate(type = "missing at runs") %>%
  update_tsibble(key = id(type))
ts_airgap_miss <-
  rbind(
    ts_airgap_level, ts_airgap_season,
    ts_airgap_occasional, ts_airgap_runs
  )

ggplot() +
  geom_line(aes(x = index, y = value), data = ts_airgap_miss) +
  geom_rect(
    aes(xmin = .from - 1, xmax = .to + 1), ymin = -Inf, ymax = Inf,
    data = count_na(ts_airgap_miss, value), colour = "grey50", fill = "grey50",
  ) +
  facet_wrap(~ type, ncol = 1) +
  xlab("Year") +
  ylab("Passengers") +
  theme_bw()

## ---- miss-types-acf
ts_airgap_dummy <- ts_airgap_miss %>%
  mutate(dummy = ifelse(is.na(value), 1L, 0L))

phi_coef <- function(...) {
  # ref: https://en.wikipedia.org/wiki/Phi_coefficient
  tab <- table(...)
  stopifnot(all(dim(tab) == c(2, 2)))
  nominator <- prod(diag(tab)) - prod(c(tab[1, 2], tab[2, 1]))
  n1_row <- sum(tab[1, ])
  n2_row <- sum(tab[2, ])
  n1_col <- sum(tab[, 1])
  n2_col <- sum(tab[, 2])
  denominator <- sqrt(n1_row * n1_col * n2_row * n2_col)
  nominator / denominator
}

acf_binary <- function(x, lag_max = NULL) {
  if (is_null(lag_max)) {
    lag_max <- floor(10 * log10(length(x)))
  }
  purrr::map_dbl(seq_len(lag_max), ~ phi_coef(x, dplyr::lag(x, .x)))
}

ts_airgap_acf <- ts_airgap_dummy %>% 
  group_by(type) %>% 
  group_map(~ tibble(acf = acf_binary(.x$dummy), lag = seq_along(acf)))

ts_airgap_acf %>% 
  ggplot(aes(x = lag, y = acf)) +
  geom_col() +
  facet_wrap(~ type, ncol = 1) +
  ylab("Phi coefficient")

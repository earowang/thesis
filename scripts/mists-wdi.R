## ---- wdi-load
library(tidyverse)
library(mists)
library(imputeTS)
library(fable)

wdi_ts <- wdi %>% 
  tsibble::as_tsibble(key = country_code, index = year)

cutoff_grids <- 
  tibble(
    measures = seq(0.4, 0.9, 0.1), key = measures, 
    index = measures, index2 = measures
  ) %>% 
  expand(measures, key, index, index2) %>% 
  rename_all(~ paste0("na_polish_", .))

## ---- wdi-vis
wdi %>% 
  select(-year, -country_code) %>% 
  rename_at(vars(ag_lnd_frst_k2:tx_val_tech_mf_zs), ~ str_replace_all(., "_", " ")) %>% 
  visdat::vis_miss() +
  theme(axis.text.x = element_text(angle = 90))

## ---- wdi-run
polish_res01 <- polish_res00 <- vector("list", NROW(cutoff_grids))
for (i in seq_len(NROW(cutoff_grids))) {
  polish_res01[[i]] <- na_polish_autotrace(wdi_ts, cutoff = c(cutoff_grids[i, ]))
}

polish_res_df01 <- bind_rows(polish_res01, .id = ".id") %>% 
  mutate(tol = .1)

for (i in seq_len(NROW(cutoff_grids))) {
  polish_res00[[i]] <- na_polish_autotrace(wdi_ts, cutoff = c(cutoff_grids[i, ]),
    tol = 0)
}
polish_res_df00 <- bind_rows(polish_res00, .id = ".id") %>% 
  mutate(tol = 0)
polish_res <- bind_rows(polish_res_df01, polish_res_df00)
write_rds(polish_res, "data/polish-results.rds", compress = "bz2")

## ---- wdi-pass
polish_res <- read_rds("data/mists/polish-results.rds")
polish_res %>% 
  group_by(.id, iteration, tol) %>% 
  summarise(iter_loss = unique(iter_loss)) %>% 
  rename(tolerance = tol) %>% 
  ggplot(aes(x = as.factor(iteration), y = iter_loss)) +
  geom_line(aes(group = .id), size = .1, alpha = .1) +
  facet_grid(~ tolerance, scales = "free_x", labeller = "label_both") +
  xlab("Iteration") +
  ylab("Loss")

## ---- wdi-grid-plot
longer_cutoff <- cutoff_grids %>% 
  mutate(.id = as.character(1:n())) %>% 
  pivot_longer(cols = starts_with("na_polish_"),
    names_to = "polisher", values_to = "grid")

polish_res %>% 
  filter(tol == .1) %>% 
  left_join(longer_cutoff) %>% 
  pivot_longer(cols = c(eval_loss, starts_with("final")),
    names_to = "metrics_types", values_to = "metrics") %>% 
  mutate(
    metrics_types = case_when(
      metrics_types == "final_prop_na" ~ "% of missings",
      metrics_types == "final_prop_removed" ~ "% of removed obs",
      metrics_types == "eval_loss" ~ "loss")
  ) %>% 
  ggplot(aes(x = as.factor(grid), y = metrics, group = grid)) +
  ggbeeswarm::geom_quasirandom(size = .2, alpha = .3) +
  facet_grid(metrics_types ~ polisher, scales = "free_y") +
  xlab("Cutoff") +
  ylab("")

## ---- wdi-polished-vis
wdi_polished <- wdi_ts %>% 
  na_polish_auto(cutoff = .5)

as_tibble(wdi_polished) %>% 
  select(-year, -country_code) %>% 
  rename_at(vars(ag_lnd_frst_k2:tx_val_tech_mf_zs), ~ str_replace_all(., "_", " ")) %>% 
  visdat::vis_miss() +
  theme(axis.text.x = element_text(angle = 90))

## ---- wdi-polished-metrics
na_polish_metrics(wdi_ts, wdi_polished)

## ---- wdi-chn
wdi_chn <- wdi_polished %>% 
  filter(country_code == "CHN")

na_chn_vars <- names(wdi_chn)[map_lgl(wdi_chn, ~ anyNA(.) && !all(is.na(.)))]

wdi_chn_na <- wdi_chn %>% 
  select(year, !!!na_chn_vars) %>% 
  as_tibble() %>% 
  pivot_longer(ag_lnd_frst_k2:tx_val_tech_mf_zs, names_to = "vars") %>% 
  mutate(vars = str_replace_all(vars, "_", " "))

wdi_chn_na_rle <- wdi_chn_na %>% 
  group_by(vars) %>% 
  summarise(na_runs = list_of_na_rle(value, index_by = year))

## ---- wdi-chn-rangeplot
na_rle_rangeplot(wdi_chn_na_rle, x = na_runs, y = fct_rev(vars)) +
  xlab("Year")

## ---- wdi-chn-imputed
wdi_chn_tsbl <- wdi_chn_na %>% 
  as_tsibble(key = vars, index = year) %>% 
  mutate(flag_na = is.na(value))
wdi_chn_imputed <- 
  na_interpolation(as.ts(wdi_chn_tsbl, value), option = "stine") %>% 
  as_tsibble() %>% 
  rename(year = index, vars = key) %>% 
  mutate(flag_na = pull(wdi_chn_tsbl, flag_na))

ggplot() +
  layer_na_rle(na_runs, data = wdi_chn_na_rle, alpha = 0.3) +
  geom_line(aes(x = year, y = value), data = wdi_chn_imputed) +
  geom_point(aes(x = year, y = value), data = filter(wdi_chn_imputed, flag_na), colour = "#f03b20") +
  facet_grid(vars ~ ., scales = "free_y", 
    labeller = labeller(vars = label_wrap_gen(10))) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text.y = element_text(size = 8)
  )

## ---- wdi-chn-ets
wdi_chn_imputed %>% 
  model(ets = ETS(value)) %>% 
  forecast(h = "3 years") %>%
  autoplot(wdi_chn_imputed) +
  facet_grid(vars ~ ., scales = "free_y", 
    labeller = labeller(vars = label_wrap_gen(10))) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text.y = element_text(size = 8),
    legend.position = "bottom"
  )

## ---- wdi-data-dict
raw_dat <- read_csv("data-raw/mists/world-development-indicators.csv", na = "..",
  n_max = 11935)
raw_dat %>% 
  distinct(`Series Code`, `Series Name`) %>% 
  mutate(`Series Code` = janitor::make_clean_names(`Series Code`)) %>% 
  knitr::kable(booktabs = TRUE, longtable = TRUE, caption = "(ref:wdi-data-dict)", linesep = "") %>%
  kableExtra::kable_styling(position = "center", latex_options= c("hold_position", "repeat_header"))

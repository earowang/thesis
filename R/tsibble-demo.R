## ---- load-pkg
library(tidyverse)
library(tsibble)

## ---- tb-sub
tb_small <- read_rds("data/tsibble/tb-small.rds")
tb_small %>%
  filter(year > 2010) %>%
  knitr::kable(booktabs = TRUE, caption = "(ref:tb-sub-cap)", linesep = "") %>%
  kableExtra::kable_styling(position = "center", latex_options= "hold_position")

## ---- tb-print
as_tsibble(tb_small, key = id(country, gender), index = year) %>%
  filter(year > 2010) %>%
  print(n = 5)

## ---- tb-au
tb_au <- tb_small %>%
  filter(country == "Australia") %>%
  group_by(year) %>%
  summarise(count = sum(count))

## ---- animate
library(gganimate)
reveal_group <- function(data, group){
  group <- enquo(group)
  data <- as_tibble(data)
  data <- transmute(
    data, !! group,
    .dt = map(map(!! group, seq_len), function(groups, data) {
      data <- filter(data, !! group %in% groups)
      select(data, !! expr(-!! group))
    }, data
  ))
  unnest(data, .dt)
}
types <- forcats::fct_inorder(c("Sliding", "Tiling", "Stretching"))
slide_window <- slider(tb_au$year, .size = 5) %>%
  map_dfr(function(x) tibble(xmin = min(x), xmax = max(x))) %>%
  mutate(ymin = -Inf, ymax = Inf, group = row_number(), type = types[1])
slide_mean <- tb_au %>%
  mutate(ma = slide_dbl(count, ~ mean(.x), .size = 5, .align = "r")) %>%
  mutate(group = pmax(0, row_number() - 4), type = types[1])
slide_revealed <- slide_mean %>%
  reveal_group(group)

tile_window <- tiler(tb_au$year, .size = 5) %>%
  map_dfr(function(x) tibble(xmin = min(x), xmax = max(x))) %>%
  mutate(ymin = -Inf, ymax = Inf, type = types[2])
tile_window <- tile_window[c(rep(1:2, each = 5), 3:4), ] %>% 
  mutate(group = row_number())
tile_mean <- tibble(
    year = tile_dbl(tb_au$year, ~ median(.x), .size = 5),
    ma = tile_dbl(tb_au$count, ~ mean(.x), .size = 5),
    type = types[2]
  )
tile_mean <- tile_mean[c(rep(1:2, each = 5), 3:4), ] %>% 
  mutate(group = row_number())
tile_revealed <- tile_mean %>%
  reveal_group(group)

stretch_window <- stretcher(tb_au$year, .init = 5) %>%
  map_dfr(function(x) tibble(xmin = min(x), xmax = max(x))) %>%
  mutate(ymin = -Inf, ymax = Inf, group = row_number(), type = types[3])
stretch_mean <- tb_au %>%
  mutate(
    ma = stretch_dbl(count, ~ mean(.x), .init = 5),
    group = pmax(0, row_number() - 4),
    type = types[3]
  )
stretch_revealed <- stretch_mean %>%
  reveal_group(group)

window <- bind_rows(slide_window, tile_window, stretch_window)
mean <- bind_rows(slide_mean, tile_mean, stretch_mean)
revealed <- bind_rows(slide_revealed, tile_revealed, stretch_revealed)

ggplot() +
  geom_line(aes(x = year, y = count), data = tb_au, colour = "grey", size = 1.2) +
  geom_rect(aes(
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    group = group
  ), data = window, 
  fill = "#9ecae1", colour = "#9ecae1", size = 1.5, alpha = 0.6) +
  geom_point(aes(x = year, y = ma), data = mean, size = 2, colour = "#de2d26") +
  geom_line(aes(x = year, y = ma), data = revealed, size = 1.2, colour = "#de2d26") +
  xlab("Year") +
  ylab("Count") +
  ylim(c(0, max(tb_au$count))) +
  facet_wrap(~ type, ncol = 1) +
  theme_bw() +
  transition_manual(group)


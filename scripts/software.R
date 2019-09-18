## ---- software-impact
library(cranlogs)
library(tidyverse)
library(lubridate)

my_pkgs <- c("sugrrants", "tsibble", "mists")

## ---- software-downloads
sugrrants_dl <- cran_downloads(my_pkgs[1], from = "2017-07-28", to = "2019-09-20")
tsibble_dl <- cran_downloads(my_pkgs[2], from = "2018-01-09", to = "2019-09-20")
df_dl <- bind_rows(sugrrants_dl, tsibble_dl)
df_dl %>% 
  ggplot(aes(x = date, y = count)) +
  geom_line() +
  facet_grid(package ~ .)

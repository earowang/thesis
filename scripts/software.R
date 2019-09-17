## ---- software-impact
library(gh)
library(cranlogs)
library(tidyverse)
library(lubridate)

my_pkgs <- c("sugrrants", "tsibble", "mists")

## ---- software-stars
sugrrants_stars <- gh("/repos/:owner/:repo",
  owner = "earowang", repo = my_pkgs[1])$stargazers_count
tsibble_stars <- gh("/repos/:owner/:repo",
  owner = "tidyverts", repo = my_pkgs[2])$stargazers_count
mists_stars <- gh("/repos/:owner/:repo",
  owner = "earowang", repo = my_pkgs[3])$stargazers_count

## ---- software-downloads
sugrrants_dl <- cran_downloads(my_pkgs[1], from = "2017-07-28")
tsibble_dl <- cran_downloads(my_pkgs[2], from = "2018-01-09")
df_dl <- bind_rows(sugrrants_dl, tsibble_dl)
df_dl %>% 
  ggplot(aes(x = date, y = count)) +
  geom_line() +
  facet_grid(package ~ .)

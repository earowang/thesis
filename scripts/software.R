## ---- software-impact
library(gh)
library(cranlogs)
library(tidyverse)
library(ggbeeswarm)

# credits to Luke Zappia
# source: https://github.com/lazappi/phd-commits
get_commits <- function(path, distinct = TRUE, filter = TRUE,
                        users = c("earowang", "Earo Wang"),
                        from = "2016-03-30") {

    dirs <- fs::dir_ls(path, type = "directory")
    commits <- purrr::map_dfr(dirs, function(dir) {
      if (git2r::in_repository(dir)) {
        commits_list <- git2r::commits(dir)
        if (length(commits_list) > 0) {
          dir_commits <- purrr::map_dfr(commits_list, function(commit) {
            tibble::tibble(
              SHA  = commit$sha,
              Name = commit$author$name,
              When = lubridate::as_datetime(commit$author$when$time)
            )
          })
          dir_commits$Repository <- fs::path_file(dir)
          return(dir_commits)
        }
      }
    })

    if (distinct) {
      commits <- dplyr::distinct(commits, SHA, .keep_all = TRUE)
    }

    commits <- dplyr::filter(commits, When >= from)

    if (filter) {
      commits <- dplyr::filter(commits, Name %in% users)
    }
    return(commits)
}

my_pkgs <- c("sugrrants", "tsibble", "mists")

sugrrants_dl <- cran_downloads(my_pkgs[1], from = "2017-07-28")
tsibble_dl <- cran_downloads(my_pkgs[2], from = "2018-01-09")

sugrrants_stars <- gh("/repos/:owner/:repo",
  owner = "earowang", repo = my_pkgs[1])$stargazers_count
tsibble_stars <- gh("/repos/:owner/:repo",
  owner = "tidyverts", repo = my_pkgs[2])$stargazers_count
mists_stars <- gh("/repos/:owner/:repo",
  owner = "earowang", repo = my_pkgs[3])$stargazers_count

sugrrants <- get_commits("~/Rpkg/sugrrants") %>% 
  mutate(Package = my_pkgs[1])
tsibble <- get_commits("~/Rpkg/tsibble") %>% 
  mutate(Package = my_pkgs[2])
mists <- get_commits("~/Rpkg/mists") %>% 
  mutate(Package = my_pkgs[3])

## ---- software-ghcommits
pkgs <- bind_rows(sugrrants, tsibble, mists) %>% 
  distinct(SHA, .keep_all = TRUE) %>% 
  mutate(Package = fct_relevel(Package, my_pkgs))

pkgs %>% 
  ggplot(aes(x = When, y = fct_rev(Package), colour = Package)) +
  geom_quasirandom(groupOnX = FALSE, size = 0.6) +
  theme(legend.position = "none") +
  ylab("Package")
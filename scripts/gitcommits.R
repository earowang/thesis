library(tidyverse)
library(git2r)

# credits to Luke Zappia
# source: https://github.com/lazappi/phd-commits
get_commits <- function(path, distinct = TRUE, filter = TRUE,
                        users = c("earowang", "Earo Wang"),
                        from = "2016-03-30") {

  dirs <- fs::dir_ls(path, type = "directory")
  commits <- map_dfr(dirs, function(dir) {
    if (in_repository(dir)) {
      commits_list <- commits(dir)
      if (length(commits_list) > 0) {
        dir_commits <- map_dfr(commits_list, function(commit) {
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
    commits <- distinct(commits, SHA, .keep_all = TRUE)
  }

  commits <- filter(commits, When >= from)

  if (filter) {
    commits <- filter(commits, Name %in% users)
  }
  commits
}


my_pkgs <- c("sugrrants", "tsibble", "mists")

sugrrants <- get_commits("~/Rpkg/sugrrants") %>% 
  mutate(Package = my_pkgs[1])
tsibble <- get_commits("~/Rpkg/tsibble") %>% 
  mutate(Package = my_pkgs[2])
mists <- get_commits("~/Rpkg/mists") %>% 
  mutate(Package = my_pkgs[3])
pkgs <- bind_rows(sugrrants, tsibble, mists) %>% 
  distinct(SHA, .keep_all = TRUE) %>% 
  mutate(Package = fct_relevel(Package, my_pkgs))

p_pkgs <- pkgs %>% 
  group_by(Package, Week = tsibble::yearweek(When)) %>% 
  summarise(Commits = n()) %>% 
  ggplot(aes(x = Week, y = Commits)) +
  geom_smooth(se = FALSE) +
  geom_point() +
  facet_grid(Package ~ .) +
  theme_bw()

ggsave("img/pkg-commits.pdf", plot = p_pkgs, device = "pdf", width = 8, height = 4)

library(tidyverse)
library(git2r)
library(fs)
library(lubridate)
library(ggrepel)

# credits to Luke Zappia
# source: https://github.com/lazappi/phd-commits
get_commits <- function(path, distinct = TRUE, filter = TRUE,
                        users = c("earowang", "Earo Wang"),
                        from = "2016-03-30") {

  dirs <- dir_ls(path, type = "directory")
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
        dir_commits$Repository <- path_file(dir)
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
  mutate(Repository = "Package", Package = fct_relevel(Package, my_pkgs))

p_pkgs <- pkgs %>%
  group_by(Package, Week = tsibble::yearweek(When)) %>%
  summarise(Commits = n()) %>%
  ggplot(aes(x = Week, y = Commits)) +
  geom_smooth(span = 0.15, se = FALSE) +
  geom_point() +
  facet_grid(Package ~ .) +
  theme_bw()

ggsave("img/pkg-commits.pdf", plot = p_pkgs, device = "pdf", width = 8, height = 4)

types <- c("Presentation", "Analysis & writing", "Package")

talks <- map_dfr(dir_ls("~/Talks"), get_commits) %>%
  distinct(SHA, .keep_all = TRUE) %>%
  mutate(Repository = types[1])

writing <- paste0("~/Research/",
  c("phd-monash", "thesis-mid", "thesis", "paper-calendar-vis", "paper-tsibble", "paper-mists")) %>%
  map_dfr(get_commits) %>%
  distinct(SHA, .keep_all = TRUE) %>%
  mutate(Repository = types[2])

phd_commits <- bind_rows(pkgs, talks, writing)

phd_milestones <-
  tribble(
    ~ date, ~ event, ~ Repository,
    "2017-03-23", "Confirmation", types[2],
    "2018-03-12", "Mid-Candidature", types[1],
    "2019-03-27", "Pre-Submission", types[1],
    "2017-05-29", "WOMBAT", types[1],
    "2017-10-28", "MelbURN", types[1],
    "2017-12-11", "IASC", types[1],
    "2018-05-24", "R Ladies (Melb)", types[1],
    "2018-07-13", "useR!", types[1],
    "2018-07-30", "JSM18", types[1],
    "2018-10-04", "NYCR", types[1],
    "2019-01-17", "rstudio::conf", types[1],
    "2019-07-28", "JSM19", types[1],
    "2016-12-15", "Init sugrrants", types[3],
    "2017-07-20", "Init tsibble", types[3],
    "2019-02-04", "Init mists", types[3],
    "2017-07-28", "Release sugrrants", types[3],
    "2018-01-09", "Release tsibble", types[3],
    "2017-08-22", "Submit sugrrants paper", types[2],
    "2019-02-13", "Submit tsibble paper", types[2]
  ) %>%
  mutate(date = ymd(date))

phd_commits <- phd_commits %>%
  mutate(date = as_date(When)) %>%
  left_join(phd_milestones) %>%
  mutate(event = case_when(duplicated(event) ~ NA_character_, TRUE ~ event))

p_commits <- phd_commits %>%
  ggplot(aes(x = When, y = Repository, colour = Repository)) +
  ggbeeswarm::geom_quasirandom(groupOnX = FALSE, size = 0.6) +
  geom_label_repel(
    aes(label = event), data = filter(phd_commits, Repository == types[3]),
    nudge_y = 0.5, hjust = 0,
    arrow = arrow(length = unit(0.02, "npc"), type = "closed")
  ) +
  geom_label_repel(
    aes(label = event), data = filter(phd_commits, Repository == types[2]),
    nudge_y = -1, hjust = 0,
    arrow = arrow(length = unit(0.02, "npc"), type = "closed")
  ) +
  geom_label_repel(
    aes(label = event), data = filter(phd_commits, Repository == types[1]),
    nudge_y = 4, hjust = 1,
    arrow = arrow(length = unit(0.02, "npc"), type = "closed")
  ) +
  ylab("") +
  scale_colour_brewer(palette = "Set2") +
  theme_bw() +
  theme(legend.position = "none")

library(gh)
library(dplyr)
library(lubridate)
library(purrr)
usepa_members <- unlist(lapply(gh("/orgs/usepa/members", .limit = Inf), 
                               function(x) x$login))

# Get all usepa repos, grab commits for each repo, filter for user and count
usepa_repos <- unlist(lapply(gh("/orgs/usepa/repos", type = "private", .limit = Inf), 
                             function(x) x$name))
usepa_commits <- c(gh(paste0("/repos/usepa/",usepa_repos[1],"/commits"), 
                      .limit = Inf))
n <- 1
for(i in usepa_repos[-1]){
  n <- n + 1
  #R-Training repo is empty and throws an error, skips for now, but really should capture error intead of name
  if(i != "R-Training"){
    usepa_commits <- c(usepa_commits, gh(paste0("/repos/usepa/",i,"/commits"), 
                                         .limit = Inf))
    message(paste0("Added ",i, " (",n, "of", length(usepa_repos), "repos)"))
  }
}

usepa_commits <- usepa_commits[which(usepa_commits != "")]
usepa_commitsl <- lapply(usepa_commits, function(x) (unlist(x)))
usepa_commitsldf <- lapply(usepa_commitsl, function(x) as_tibble(t(unlist(x))))
usepa_commitsldfs <- lapply(usepa_commitsldf, function(x) select(x, sha, commit_date = commit.committer.date, user = commit.author.name, url))
usepa_commits_df <- do.call("rbind", usepa_commitsldfs) %>%
  mutate(repo_name = stringr::str_extract(url, "USEPA/.+/commits")) %>%
  mutate(repo_name = stringr::str_replace(repo_name, "USEPA/","")) %>%
  mutate(repo_name = stringr::str_replace(repo_name, "/commits","")) %>%
  mutate(commit_date = lubridate::date(commit_date))
  
usepa_commits_per_private_repo <- usepa_commits_df %>% 
  group_by(repo_name) %>%
  summarise(num_commits = n()) %>%
  arrange(num_commits) 

usepa_latest_commit_per_private_repo <- usepa_commits_df %>% 
  group_by(repo_name) %>%
  summarise(last_commit = max(commit_date))

usepa_private_repo_stats <- usepa_commits_per_private_repo %>%
  full_join(usepa_latest_commit_per_private_repo) %>%
  arrange(last_commit, num_commits)

readr::write_csv(usepa_private_repo_stats,"priv_repo_stats.csv")

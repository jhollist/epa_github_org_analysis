library(gh)
library(dplyr)
library(lubridate)
library(purrr)
usepa_members <- unlist(lapply(gh("/orgs/usepa/members", .limit = Inf), 
                               function(x) x$login))

# Get all usepa repos, grab commits for each repo, filter for user and count
usepa_repos <- unlist(lapply(gh("/orgs/usepa/repos", type = "all", .limit = Inf), 
                             function(x) x$name))
usepa_commits <- c(gh(paste0("/repos/usepa/",usepa_repos[1],"/commits"), 
                      .limit = Inf))
n <- 1
for(i in usepa_repos[-1:-71]){
  n <- n + 1
  #R-Training repo is empty and throws an error, skips for now, but really should capture error intead of name
  if(i != "R-Training"){
    usepa_commits <- c(usepa_commits, gh(paste0("/repos/usepa/",i,"/commits"), 
                                         .limit = Inf))
    message(paste0("Added ",i, " (",n, "of", length(usepa_repos), "repos)"))
  }
}

usepa_commits <- usepa_commits[which(usepa_commits != "")]
usepa_commitsl <- lapply(usepa_commits[-11971], function(x) (unlist(x)))
usepa_commitsldf <- lapply(usepa_commitsl, function(x) as_tibble(t(unlist(x))))
usepa_commitsldfs <- lapply(usepa_commitsldf, function(x) select(x, sha, commit_date = commit.committer.date, user = commit.author.name, url))
usepa_commits_df <- do.call("rbind", usepa_commitsldfs)

usepa_commits_df %>% 
  group_by(url) %>%
  
  arrange(commit_date) 

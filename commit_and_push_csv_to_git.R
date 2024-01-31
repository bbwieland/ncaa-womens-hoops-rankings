library(git2r)

source("/Users/ben/Desktop/Code/wbb-rankings/wbb_adjem_script.R", echo = FALSE)

repo_path <- "/Users/ben/Desktop/Code/wbb-rankings"
current_date_time <- format(Sys.time(), "%b %d, %Y at %H:%M %p %Z")

git2r::add(repo = repo_path,
           path = "landing_page.csv")

git2r::commit(repo = repo_path,
              message = paste0("Data update: ", current_date_time))

git2r::push(object = repo_path,
            credentials = cred_token())

Sys.sleep(5)

source("/Users/ben/Desktop/Code/wbb-rankings/wbb_fanmatch.R", echo = FALSE)

current_date_time <- format(Sys.time(), "%b %d, %Y at %H:%M %p %Z")

git2r::add(repo = repo_path,
           path = "fanmatch.csv")

git2r::commit(repo = repo_path,
              message = paste0("Matchups update: ", current_date_time))

git2r::push(object = repo_path,
            credentials = cred_token())

Sys.sleep(5)

source("/Users/ben/Desktop/Code/wbb-rankings/wbb_four_factors.R", echo = FALSE)

current_date_time <- format(Sys.time(), "%b %d, %Y at %H:%M %p %Z")

git2r::add(repo = repo_path,
           path = "four_factors_o.csv")

git2r::add(repo = repo_path,
           path = "four_factors_d.csv")

git2r::commit(repo = repo_path,
              message = paste0("Team stats update: ", current_date_time))

git2r::push(object = repo_path,
            credentials = cred_token())
library(tidyverse)
library(wehoop)
library(cbbdata)

current_year <- 2024
current_date <- Sys.Date()
hca <- 0.0195
avg_tempo <- 70.8
avg_ppp <- 0.923

daily_games <- wehoop::load_wbb_schedule(seasons = current_year) %>%
  filter(status_type_completed == FALSE & game_date == current_date)

team_rankings <- read_csv("https://raw.githubusercontent.com/bbwieland/ncaa-womens-hoops-rankings/main/landing_page.csv") %>%
  select(team_id,team, off_eff, def_eff, poss, net_rk)

fanmatch <- daily_games %>%
  select(game_date_time, home_id, away_id) %>%
  inner_join(team_rankings, by = c("home_id" = "team_id")) %>%
  inner_join(team_rankings, by = c("away_id" = "team_id"), suffix = c("_home","_away")) %>%
  mutate(home_ppp_est = (off_eff_home + def_eff_away) / 2 + hca,
         away_ppp_est = (off_eff_away + def_eff_home) / 2 - hca,
         poss_est = poss_home + poss_away - avg_tempo) %>%
  mutate(home_pts_est = home_ppp_est * poss_est,
         away_pts_est = away_ppp_est * poss_est,
         game_time = format(game_date_time, "%I:%M %Z")) %>%
  select(game_time, home_id, away_id, net_rk_home, team_home, 
         net_rk_away, team_away, 
         home_pts_est, away_pts_est, poss_est) %>%
  mutate(est_diff = abs(home_pts_est - away_pts_est)) %>%
  arrange(game_time) %>%
  mutate(quality = 100 - (net_rk_home + net_rk_away - 3) / 7.23,
         competitiveness = ifelse(100 - 4 * est_diff > 0,
                                  100 - 4 * est_diff,
                                  0)) %>%
  mutate(home_pts_est = case_when(
    home_pts_est > away_pts_est & home_pts_est - away_pts_est < 1 ~ home_pts_est + 0.5,
    away_pts_est > home_pts_est & away_pts_est - home_pts_est < 1 ~ home_pts_est - 0.5,
    TRUE ~ home_pts_est
  )) %>%
  mutate(away_pts_est = case_when(
    home_pts_est > away_pts_est & home_pts_est - away_pts_est < 1 ~ away_pts_est - 0.5,
    away_pts_est > home_pts_est & away_pts_est - home_pts_est < 1 ~ away_pts_est + 0.5,
    TRUE ~ away_pts_est
  )) %>% 
  arrange(-quality) %>%
  mutate(home_pts_clean = round(home_pts_est),
         away_pts_clean = round(away_pts_est)) %>%
  mutate(final_score_proj = paste(home_pts_clean, away_pts_clean, sep = "-"))

write_csv(fanmatch, "fanmatch.csv")

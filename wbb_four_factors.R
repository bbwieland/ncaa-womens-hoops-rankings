library(tidyverse)
library(wehoop)
library(cbbdata)

current_year <- 2024

scores <- load_wbb_team_box()
teams <- cbd_teams()
conferences <- cbd_torvik_ratings(year = current_year) %>%
  select(team, conf)

team_key <- teams %>%
  left_join(conferences, by = c("common_team" = "team")) %>%
  select(espn_id, team = common_team, conf) %>%
  mutate(espn_id = as.numeric(espn_id))

scores_clean <- scores %>%
  left_join(team_key, by = c("team_id" = "espn_id"),
            relationship = "many-to-many") %>%
  left_join(team_key, by = c("opponent_team_id" = "espn_id"),
            suffix = c("","_opp"),
            relationship = "many-to-many") %>%
  filter(!is.na(conf))

valid_game_ids <- scores_clean %>%
  group_by(game_id) %>%
  count() %>%
  filter(n == 2) %>%
  pull(game_id)

scores_box <- scores_clean %>%
  filter(game_id %in% valid_game_ids) %>%
  select(game_id, game_date, team_id, team, opp_id = opponent_team_id, opp_team = team_opp, assists:turnovers, points = team_score) %>%
  group_by(game_id) %>%
  mutate(obs = row_number()) %>%
  mutate(opp_offensive_rebounds = ifelse(obs == 1, lead(offensive_rebounds), lag(offensive_rebounds)),
         opp_defensive_rebounds = ifelse(obs == 1, lead(defensive_rebounds), lag(defensive_rebounds))) %>%
  ungroup() %>%
  mutate(points = as.numeric(points))

team_stats_offense <- scores_box %>%
  group_by(team_id) %>%
  summarise(games = n(),
            fgm = sum(field_goals_made),
            fga = sum(field_goals_attempted),
            fg3m = sum(three_point_field_goals_made),
            fg3a = sum(three_point_field_goals_attempted),
            fg2m = fgm - fg3m,
            fg2a = fga - fg3a,
            ftm = sum(free_throws_made),
            fta = sum(free_throws_attempted),
            to = sum(turnovers),
            ast = sum(assists),
            stl = sum(steals),
            blk = sum(blocks),
            trb = sum(total_rebounds),
            orb = sum(offensive_rebounds),
            drb = trb - orb,
            opp_orb = sum(opp_offensive_rebounds),
            opp_drb = sum(opp_defensive_rebounds),
            poss = (fga-orb) + to + 0.44 * fta,
            pts = sum(points),
            .groups = "drop")

team_stats_defense <- scores_box %>%
  group_by(opp_id) %>%
  summarise(games = n(),
            fgm = sum(field_goals_made),
            fga = sum(field_goals_attempted),
            fg3m = sum(three_point_field_goals_made),
            fg3a = sum(three_point_field_goals_attempted),
            fg2m = fgm - fg3m,
            fg2a = fga - fg3a,
            ftm = sum(free_throws_made),
            fta = sum(free_throws_attempted),
            to = sum(turnovers),
            ast = sum(assists),
            stl = sum(steals),
            blk = sum(blocks),
            trb = sum(total_rebounds),
            orb = sum(offensive_rebounds),
            drb = trb - orb,
            opp_orb = sum(opp_offensive_rebounds),
            opp_drb = sum(opp_defensive_rebounds),
            poss = (fga-orb) + to + 0.44 * fta,
            pts = sum(points),
            .groups = "drop")

four_factors_offense <- team_stats_offense %>%
  group_by(team_id) %>%
  summarise(fg_pct = fgm/fga,
            ft_pct = ftm/fta,
            fg2_pct = fg2m/fg2a,
            fg3_pct = fg3m/fg3a,
            ts_pct = pts / (2 * (fga + 0.44 * fta)),
            ast_rate = ast / fgm,
            fg3_rate = fg3a / fga,
            to_rate = to / (fga + 0.44 * fta + to),
            ft_rate = fta / fga,
            orb_rate = orb / (orb + opp_drb)) %>%
  left_join(team_key, by = c("team_id" = "espn_id")) %>%
  select(team, conf, everything()) %>%
  filter(!is.na(team) & !is.na(conf)) %>%
  distinct(team_id, .keep_all = TRUE)

four_factors_defense <- team_stats_defense %>%
  rename(team_id = opp_id) %>%
  group_by(team_id) %>%
  summarise(fg_pct = fgm/fga,
            ft_pct = ftm/fta,
            fg2_pct = fg2m/fg2a,
            fg3_pct = fg3m/fg3a,
            ts_pct = pts / (2 * (fga + 0.44 * fta)),
            ast_rate = ast / fgm,
            fg3_rate = fg3a / fga,
            to_rate = to / (fga + 0.44 * fta + to),
            ft_rate = fta / fga,
            orb_rate = orb / (orb + opp_drb)) %>%
  left_join(team_key, by = c("team_id" = "espn_id")) %>%
  select(team, conf, everything()) %>%
  filter(!is.na(team) & !is.na(conf)) %>%
  distinct(team_id, .keep_all = TRUE)

four_factors_offense_ranked <- four_factors_offense %>%
  mutate(fg_pct_rk = dense_rank(-fg_pct),
         ft_pct_rk = dense_rank(-ft_pct),
         fg2_pct_rk = dense_rank(-fg2_pct),
         fg3_pct_rk = dense_rank(-fg3_pct),
         ts_pct_rk = dense_rank(-ts_pct),
         ast_rate_rk = dense_rank(-ast_rate),
         fg3_rate_rk = dense_rank(-fg3_rate),
         to_rate_rk = dense_rank(to_rate),
         ft_rate_rk = dense_rank(-ft_rate),
         orb_rate_rk = dense_rank(-orb_rate)) %>%
  select(team, conf,
         contains("fg_pct"),
         contains("ft_pct"),
         contains("fg2_pct"),
         contains("fg3_pct"),
         contains("ts_pct"),
         contains("ast_rate"),
         contains("fg3_rate"),
         contains("to_rate"),
         contains("ft_rate"),
         contains("orb_rate"))

four_factors_defense_ranked <- four_factors_defense %>%
  mutate(fg_pct_rk = dense_rank(fg_pct),
         ft_pct_rk = dense_rank(ft_pct),
         fg2_pct_rk = dense_rank(fg2_pct),
         fg3_pct_rk = dense_rank(fg3_pct),
         ts_pct_rk = dense_rank(ts_pct),
         ast_rate_rk = dense_rank(ast_rate),
         fg3_rate_rk = dense_rank(fg3_rate),
         to_rate_rk = dense_rank(-to_rate),
         ft_rate_rk = dense_rank(ft_rate),
         orb_rate_rk = dense_rank(orb_rate)) %>%
  select(team, conf,
         contains("fg_pct"),
         contains("ft_pct"),
         contains("fg2_pct"),
         contains("fg3_pct"),
         contains("ts_pct"),
         contains("ast_rate"),
         contains("fg3_rate"),
         contains("to_rate"),
         contains("ft_rate"),
         contains("orb_rate"))

write_csv(four_factors_offense_ranked, "/Users/ben/Desktop/Code/wbb-rankings/four_factors_o.csv")
write_csv(four_factors_defense_ranked, "/Users/ben/Desktop/Code/wbb-rankings/four_factors_d.csv")
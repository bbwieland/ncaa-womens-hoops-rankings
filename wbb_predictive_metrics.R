library(tidyverse)
library(wehoop)
library(cbbdata)

current_year <- 2024

box <- load_wbb_team_box()
teams <- cbd_teams()
conferences <- cbd_torvik_ratings(year = current_year) %>%
  select(team, conf)

team_key <- teams %>%
  left_join(conferences, by = c("common_team" = "team")) %>%
  select(espn_id, team = common_team, conf) %>%
  mutate(espn_id = as.numeric(espn_id))

team_stats <- box %>%
  group_by(team_id) %>%
  summarise(games = n(),
            wins = sum(team_winner),
            losses = games - wins,
            fgm = sum(field_goals_made),
            fga = sum(field_goals_attempted),
            fg3m = sum(three_point_field_goals_made),
            fg3a = sum(three_point_field_goals_attempted),
            fg2m = fgm - fg3m,
            fg2a = fga - fg3a,
            ftm = sum(free_throws_made),
            fta = sum(free_throws_attempted),
            to = sum(turnovers),
            stl = sum(steals),
            blk = sum(blocks),
            trb = sum(total_rebounds),
            orb = sum(offensive_rebounds),
            drb = trb - orb,
            pts = sum(team_score),
            ptsa = sum(opponent_team_score),
            win_pct = wins / games,
            fg_pct = fgm / fga,
            fg2_pct = fg2m / fg2a,
            fg3_pct = fg3m / fg3a,
            ft_pct = ftm / fta, 
            poss = (fga-orb) + to + 0.44 * fta, 
            ppp = pts / poss,
            pppa = ptsa / poss,
            tempo = poss / games,
            eff = ppp - pppa,
            fg3_rate = fg3a / fga,
            ft_rate = fta / fga,
            to_rate = to / (fga + 0.44 * fta + to),
            .groups = "drop") %>%
  left_join(team_key, by = c("team_id" = "espn_id")) %>%
  select(team, team_id, conf, everything()) %>%
  filter(!is.na(conf))

team_stats_adv = team_stats %>%
  mutate(record = paste(wins, losses, sep = "-")) %>%
  select(team, team_id, conf, record, tempo, eff, fg2_pct, fg3_pct, fg3_rate, ft_rate, to_rate)

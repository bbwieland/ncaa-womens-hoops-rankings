library(tidyverse)
library(wehoop)
library(cbbdata)

current_year <- 2024

box <- load_wbb_team_box()
teams <- cbd_teams()
conferences <- cbd_torvik_ratings(year = current_year) %>%
  select(team, conf)


# Filtering to DI-DI games and adding conferences -------------------------

team_key <- teams %>%
  left_join(conferences, by = c("common_team" = "team")) %>%
  select(espn_id, team = common_team, conf) %>%
  mutate(espn_id = as.numeric(espn_id))

valid_team_ids <- unique(teams$espn_id)

scores <- box %>%
  left_join(team_key, by = c("team_id" = "espn_id"),
            relationship = "many-to-many") %>%
  left_join(team_key, by = c("opponent_team_id" = "espn_id"), suffix = c("_team","_opp"),
            relationship = "many-to-many") %>%
  filter(team_id %in% valid_team_ids & opponent_team_id %in% valid_team_ids) %>%
  distinct(paste(game_date, team_id), .keep_all = TRUE)


# Creating key variables: possessions & points per possession ------------

scores_clean <- scores %>%
  mutate(poss = (field_goals_attempted-offensive_rebounds) + turnovers + 0.44 * free_throws_attempted, 
         ppp = team_score / poss,
         pppa = opponent_team_score / poss,
         conf_game = conf_team == conf_opp) %>%
  select(game_id, 
         team_id, opponent_id = opponent_team_id,
         game_date,
         team_ppp = ppp, opponent_ppp = pppa,
         game_location = team_home_away, 
         conf_game, poss)

# Sanity check: ensuring no duplicate game_ids

scores_clean %>% group_by(game_id) %>% count() %>% pull(n) %>% max()

# Sanity check: confirming D1-D1 filtering

length(unique(c(scores_clean$team_id, scores_clean$opponent_id)))

# Home court advantage coefficients ---------------------------------------

hca_coef <- scores_clean %>%
  filter(conf_game == TRUE & game_location == "home") %>%
  summarise(home_ppp = mean(team_ppp),
            away_ppp = mean(opponent_ppp),
            overall_ppp = mean(c(team_ppp, opponent_ppp)),
            home_adj = - (home_ppp - overall_ppp),
            away_adj = - (away_ppp - overall_ppp)) %>%
  as.list()

scores_clean_hca <- scores_clean %>%
  mutate(team_ppp_adj = case_when(
    game_location == "home" ~ team_ppp + hca_coef$home_adj,
    game_location == "away" ~ team_ppp + hca_coef$away_adj
  )) %>%
  mutate(opp_ppp_adj = case_when(
    game_location == "home" ~ opponent_ppp - hca_coef$home_adj,
    game_location == "away" ~ opponent_ppp - hca_coef$away_adj
  ))

# Functionalizing the adjusted efficiency margin model --------------------

calculate_single_game_effect <- function(team_id_model, game_id_model,
                                         df = scores_clean_hca) {
  
  game <- df %>% 
    filter(team_id == team_id_model & game_id == game_id_model)
  
  opp <- game$opponent_id
  opp_season <- df %>%
    filter(team_id == opp & game_id != game_id_model)
  
  opp_ppp_off <- mean(opp_season$team_ppp_adj)
  opp_ppp_def <- mean(opp_season$opp_ppp_adj)
  
  off_coef <- game$team_ppp_adj - opp_ppp_def
  def_coef <- game$opp_ppp_adj - opp_ppp_off
  
  coefs <- data.frame(off_eff = off_coef, def_eff = def_coef, poss = game$poss)
  
  return(coefs)
}

calculate_team_coefficients <- function(team_id_season,
                                        df = scores_clean_hca) {
  
  team_games <- df %>% filter(team_id == team_id_season) %>% pull(game_id)
  team_eff_results <- map_dfr(.x = team_games, 
                              .f = ~ calculate_single_game_effect(team_id_model = team_id_season,
                                                                  game_id_model = .x))
  
  team_eff_season <- team_eff_results %>%
    summarise(off_eff = mean(off_eff) + hca_coef$overall_ppp,
              def_eff = mean(def_eff) + hca_coef$overall_ppp,
              net_eff = off_eff - def_eff,
              poss = mean(poss)) %>%
    mutate(team_id = team_id_season) %>%
    select(team_id, everything())
  
  print(paste("Calculated coefficients for team",team_id_season))
  
  return(team_eff_season)
  
}

# Calculate the team coefficients -----------------------------------------

model_team_ids <- unique(scores_clean_hca$team_id) %>% sort()

model_results <- map_dfr(.x = model_team_ids,
                         .f = ~ calculate_team_coefficients(.x))


team_records <- scores %>%
  group_by(team_id) %>%
  summarise(games = n(),
            wins = sum(team_winner),
            losses = games - wins,
            conf_games = sum(conf_team == conf_opp),
            conf_wins = sum(team_winner[conf_team == conf_opp]),
            conf_losses = conf_games - conf_wins,
            win_pct = wins / games,
            conf_win_pct = conf_wins / conf_games,
            record = paste(wins, losses, sep = "-"),
            conf_record = paste(conf_wins, conf_losses, sep = "-")) %>%
  select(team_id, games:conf_record)

team_results <- model_results %>%
  left_join(team_key, by = c("team_id" = "espn_id")) %>%
  arrange(-net_eff) %>%
  left_join(team_records, by = "team_id") %>%
  select(team, conf, record, conf_record, everything())

# Generating the tables ---------------------------------------------------

landing_page <- team_results %>%
  mutate(off_rk = row_number(-(off_eff)),
         def_rk = row_number(def_eff),
         net_rk = row_number(-(net_eff)),
         poss_rk = row_number((-poss))) %>%
  select(team, conf, record, conf_record, net_eff, net_rk, off_eff, off_rk, def_eff, def_rk, poss, poss_rk)

write_csv(landing_page, "landing_page.csv")

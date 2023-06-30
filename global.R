library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)
library(purrr)
library(tidyr)
library(lubridate)
library(extrafont)
library(ggrepel)
library(ggimage)
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
# library(anytime)
# library(runner)
library(fs)
library(scales)
library(stringi)
library(ggtext)
library(bslib)
library(DBI)
library(gt)
library(shinythemes)

Sys.setenv (TZ="America/Chicago")

aws_connect <- dbConnect(drv = RPostgres::Postgres(), dbname = Sys.getenv('aws_db'),
                         host = Sys.getenv('aws_host'),
                         port = as.integer(Sys.getenv('aws_port')),
                         user = Sys.getenv('aws_user'), password = Sys.getenv('aws_pw'),
                         options = "-c search_path=nba_prod")

today <- Sys.Date()
todayDate <- Sys.Date()
yesterday <- Sys.Date()-1
isSeasonActive <- TRUE
today <-  format(today, format = "%B %d, %Y")

# custom theme for plotly objects
theme_jacob <- function(..., base_size = 11) {
  theme(panel.grid.minor = element_blank(),
        panel.grid.major =  element_line(color = "#d0d0d0"),
        panel.background = element_rect(fill = "#f0f0f0", color = NA),
        plot.background = element_rect(fill = "#f0f0f0", color = NA),
        legend.background = element_rect(fill = '#f0f0f0', color = NA),
        legend.position = 'top',
        panel.border = element_blank(),
        strip.background = element_blank(),
        plot.margin = margin(0.5, 1, 0.5, 1, unit = "cm"),
        axis.ticks = element_blank(),
        text = element_text(family = "Gill Sans MT", size = base_size),
        axis.text = element_text(face = "bold", color = "grey40", size = base_size),
        axis.title = element_text(face = "bold", size = rel(1.2)),
        axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, unit = "cm"), angle = 90),
        plot.title = element_text(face = "bold", size = rel(1.05), hjust = 0.52, margin = margin(0, 0, .2, 0, unit = 'cm')),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 11, margin = margin(0, 0, 0.2, 0, unit = "cm"), hjust = 0.5),
        plot.caption = element_text(size = 10, margin = margin(1, 0, 0, 0, unit = "cm"), hjust = 1),
        strip.text = element_text(size = rel(1.05), face = "bold"),
        strip.text.x = element_text(margin = margin(0.1, 0, 0.1, 0, "cm")),
        ...
  )
}
theme_set(theme_jacob())

# data retrieval function
get_data <- function(table_name){
  if (isSeasonActive == TRUE){ #& as.double(Sys.time() - file_info(paste0('data/', table_name, '.csv'))$change_time, units = 'hours') > 8.0){
    # this csv gets fucked up with pbp data timestamp values (10:51) character and turns it into (10:51:00).  force char
    df <- dbReadTable(aws_connect, table_name)
    # write_csv(df, paste0('data/', table_name, '.csv'))
    return(df)
  }
  else {
    df <- suppressWarnings(read_csv(paste0('data/', table_name, '.csv'), col_types = cols(time_quarter = col_character())))
    return(df)
  }
}

# Helper Functions
get_ord_numbers <- function(df){
  new_df <- case_when(df %in% c(11, 12, 13) ~ "th",
                      df %% 10 == 1 ~ 'st',
                      df %% 10 == 2 ~ 'nd',
                      df %% 10 == 3 ~ 'rd',
                      TRUE ~ 'th')
  new_df2 <- paste0(df, new_df)
  return(new_df2)
  
}

# write_csv(contracts_value, 'data/prod_contract_value_analysis.csv')
# Loading in Data 
bans <- get_data('bans') %>%
  mutate(record = paste0(tot_wins[1], ' - ', tot_wins[2]),
         pct_change = round((avg_pts - last_yr_ppg) / avg_pts, 3) * 100,                      # time comes in UTC format
         last_updated_at = strftime(last_updated_at, format = '%B %d, %Y - %I:%M %p CST')) # this formats into CST for me

contracts_value <- get_data('contract_value_analysis') 

# future_schedule_analysis <- get_data('future_schedule_analysis') %>%
#   mutate(team = fct_reorder(team, pct_games_left_above_500))
is_playoffs_feature_flag <- get_data('feature_flags') %>%
  filter(flag == 'playoffs') %>%
  select(is_enabled) %>%
  pull()

game_types <- get_data('game_types') %>%
  group_by(type) %>%
  mutate(pct_total = round(n / sum(n), 3)) %>%
  ungroup()

injuries <- get_data('injuries') %>%
  select(Player = player, -team_acronym, Team = team, Date = date, Status = status, Injury = injury, Description = description)

injury_tracker <- get_data('injury_tracker') %>%
  mutate(player = map(player, ~gt::html(as.character(.x)))) %>%
  select(player_logo, Player = player, Status = status, `Continuous Games Missed` = continuous_games_missed,
         `Average PPG` = season_avg_ppg, `Average TS%` = season_ts_percent, `Average +/-` = season_avg_plusminus,
         `MVP Score` = player_mvp_calc_avg, player_logo)

mov <- get_data('mov') %>%
  mutate(date = as.Date(date),
         win_int = case_when(outcome == 'W' ~ 1,
                             TRUE ~ 0),
         loss_int = case_when(outcome == 'L' ~ 1,
                              TRUE ~ 0)) %>%
  group_by(team) %>%
  arrange(team, date) %>%
  mutate(win_record = cumsum(win_int),
         loss_record = cumsum(loss_int),
         row_record = paste0(win_record, ' - ', loss_record)) %>%
  ungroup() %>%
  select(-win_int, -loss_int, -win_record, -loss_record)

opp_stats <- get_data('opp_stats') %>%
  mutate(rating_text = paste0('Opponent FG%: ', fg_percent_opp * 100, '%', ' (', fg_percent_rank, ')', '<br>',
                              'Opponent 3P%: ', threep_percent_opp * 100, '%', ' (', three_percent_rank, ')', '<br>',
                              'Opponent PPG: ', ppg_opp, ' (', ppg_opp_rank, ')'))

past_schedule_analysis <- get_data('past_schedule_analysis') %>%
  mutate(team = fct_reorder(team, pct_vs_below_500))

# this makes it so the game winning shot will show instead of 'end of 4th quarter' text on plot
pbp_data_end <- get_data('pbp') %>%
  filter(time_remaining_final == 0.00) %>%
  group_by(game_description) %>%
  slice(1) %>%
  ungroup()

pbp_data <- get_data('pbp') %>%
  filter(time_remaining_final != 0.00) %>%
  rbind(pbp_data_end) %>%
  mutate(leading_team_text = case_when(scoring_team == leading_team & leading_team != 'TIE' ~ 'Leading',
                                       scoring_team != leading_team & leading_team != 'TIE' ~ 'Trailing',
                                       TRUE ~ 'TIE'
                                       )) %>%
  distinct() %>%
  group_by(game_description) %>%
  arrange(desc(time_remaining_final)) %>%
  ungroup()

rm(pbp_data_end)

preseason_odds <- get_data('preseason_odds') %>%
  mutate(team = fct_reorder(team, wins_differential))

recent_games_players <- get_data('recent_games_players') %>%
  arrange(desc(pts)) %>%
  mutate(rank = row_number(),
         player_new = map(player_new, ~gt::html(as.character(.x)))) %>%
  select(Rank = rank, player_logo, Player = player_new, pts, `TS%` = game_ts_percent, `+/-` = plusminus, Outcome = outcome, Salary = salary, pts_color, ts_color) %>%
  as_tibble()

recent_games_teams <- get_data('recent_games_teams') %>%
  select(team_logo, team, outcome, pts_scored, max_team_lead, new_loc, opp_logo, opponent, pts_scored_opp, max_opponent_lead, 
         pts_color, opp_pts_color, mov)

reddit_data <- get_data('reddit_comments') %>%
  select(`Scrape Date` = scrape_date, User = author, Flair = flair, Comment = comment, Score = score,
         `Compound Sentiment Score` = compound, Pos = pos, Neutral = neu, Neg = neg, URL = url) %>%
  mutate(Score = as.numeric(Score),
         URL = paste0("<a href='",URL,"'>",URL,"</a>")) %>%
  head(2000)

reddit_team_sentiment <- get_data('reddit_sentiment_time_series') %>%
  filter(scrape_date >= '2022-10-01') %>%
  mutate(num_comments = as.numeric(num_comments))

rolling_avg <- get_data('rolling_avg_stats')

schedule <- get_data('schedule') %>%
  select(Date = date, `Start Time (EST)` = start_time, `Home Team` = home_team_odds, `Road Team` = away_team_odds, `Average Team Rank` = avg_team_rank,
         home_team, away_team, home_moneyline_raw, home_team_logo, away_moneyline_raw, away_team_logo)

social_media_bans <- get_data('social_media_aggs')

standings <- get_data('standings')

standings_rollup <- get_data('team_record_daily_rollup')

team_adv_stats <- get_data('team_adv_stats')

team_blown_leads <- get_data('team_blown_leads') %>%
  mutate(net_comebacks = as.numeric(net_comebacks),
         team = fct_reorder(team, net_comebacks))

team_contract_analysis <- get_data('team_contracts_analysis') %>%
  mutate(team = fct_reorder(team, team_pct_salary_earned))

team_ratings <- get_data('team_ratings')

team_ratings_bans <- team_ratings %>%
  mutate(rating_text = paste0('Offensive Rating: ', ortg, ' (', ortg_rank, ')', '<br>',
                              'Defensive Rating: ', drtg, ' (', drtg_rank, ')', '<br>',
                              'Net Rating: ', nrtg, ' (', nrtg_rank, ')'))

top_scorers <- get_data('scorers')

transactions <- get_data('transactions') %>%
  select(Date = date, Transaction = transaction)

twitter_data <- get_data('twitter_comments') %>%
  select(`Tweet Date` = created_at, User = username, Tweet = tweet, Likes = likes, Retweets = retweets,
         `Compound Sentiment Score` = compound, Pos = pos, Neutral = neu, Neg = neg, URL = url) %>%
  mutate(URL = paste0("<a href='",URL,"'>",URL,"</a>")) %>%
  head(2000)

dbDisconnect(aws_connect)

aws_connect <- dbConnect(drv = RPostgres::Postgres(), dbname = Sys.getenv('aws_db'),
                         host = Sys.getenv('aws_host'),
                         port = as.integer(Sys.getenv('aws_port')),
                         user = Sys.getenv('aws_user'), password = Sys.getenv('aws_pw'),
                         options = "-c search_path=ml_models")

schedule_ml <- get_data('tonights_games_ml') %>%
  filter(proper_date == Sys.Date())
# proper_date == max(proper_date),

dbDisconnect(aws_connect)

###### Data Extraction Complete ######
# Data Manipulation ----
pbp_games_yesterday <- pbp_data %>%
  select(game_description) %>%
  distinct() %>%
  pull()

east_standings <- standings %>%
  filter(conference == 'Eastern') %>%
  select(Seed = rank, Team = team, Wins = wins, Losses = losses, `Active Injuries` = active_injuries,
         `Active Health & Safety Protocols` = active_protocols, `Last 10 Games` = last_10) 

west_standings <- standings %>%
  filter(conference == 'Western') %>%
  select(Seed = rank, Team = team, Wins = wins, Losses = losses, `Active Injuries` = active_injuries,
         `Active Health & Safety Protocols` = active_protocols, `Last 10 Games` = last_10) 

upcoming_games_count <- schedule %>%
  filter(Date >= Sys.Date()) %>%
  filter(Date == min(Date)) %>%
  count() %>%
  pull()
  
bans <- bans %>%
  mutate(upcoming_games = upcoming_games_count)

league_average_ts <- bans$league_ts_percent[1]
updated_date <- bans$last_updated_at[1]

most_recent_date <- as_tibble(bans$most_recent_game[1]) %>%
  rename(date = value)

# basically - try to return data with ml predictions if it's there.
# else return the schedule with odds no matter what
# use schedule_ml for tonight's games, schedule_tonight 
get_schedule_tonight <- function(schedule_df, schedule_df_ml){
  if ((nrow(schedule_df_ml) > 0) & (min(schedule_df$Date) == suppressWarnings(min(schedule_df_ml$proper_date)))){
    
    schedule_tonight <- schedule_df %>%
      mutate(Date = as.character(Date)) %>%
      left_join(schedule_df_ml %>% select(home_team, Date = proper_date, home_team_predicted_win_pct, away_team_predicted_win_pct)) %>%
      filter(Date == min(Date)) %>%
      select(-home_team, -away_team) %>%
      mutate(
        home_is_great_value = case_when(
          ((home_moneyline_raw >= -130 | home_moneyline_raw >= 200) & home_team_predicted_win_pct >= 0.55) |
            (home_moneyline_raw >= 170 & home_team_predicted_win_pct >= 0.50) ~ 1,
          TRUE ~ 0
        ),
        away_is_great_value = case_when(
          ((away_moneyline_raw >= -130 | away_moneyline_raw >= 200) & away_team_predicted_win_pct >= 0.55) |
            (away_moneyline_raw >= 170 & away_team_predicted_win_pct >= 0.50) ~ 1,
          TRUE ~ 0
        )
      ) %>%
      rename(`Home Predicted Win %` = home_team_predicted_win_pct, `Road Predicted Win %` = away_team_predicted_win_pct)
    
  }
  else {
    schedule_tonight <- schedule_df %>%
      select(-home_team, -away_team) %>%
      filter(Date == Sys.Date())
  }
  
  return(schedule_tonight)
  
}

schedule_tonight <- get_schedule_tonight(schedule, schedule_ml)
# bby2 <- get_schedule_tonight(schedule, schedule_ml_test)

schedule <- schedule %>%
  mutate(Date = as.character(Date)) %>%
  anti_join(schedule_tonight %>% select(Date) %>% mutate(Date = as.character(Date))) %>%
  select(-home_team, -away_team, -home_moneyline_raw, -away_moneyline_raw, -home_team_logo, -away_team_logo) %>%
  arrange(Date)

# use this to test
# schedule_tonight = data.frame()

######### Data Manipulation Complete ########
# Graphs ----
top20_plot <- function(df){
  p <- df %>%
    filter(games_played >= 1,
           season_avg_ppg >= 20) %>%
    ggplot(aes(season_avg_ppg, season_ts_percent, fill = top5_candidates)) +
    geom_point(size = 6, alpha = 0.7, pch = 21, color = 'black', aes(text = paste0(player, '<br>',
                                                                                   team, '<br>', # (', Wins, '-', Losses, ')', '<br>',
                                                                                   'PPG: ', round(season_avg_ppg, 1), '<br>',
                                                                                   'TS%: ', round(season_ts_percent * 100, 1), '%',
                                                                                   '<br>',
                                                                                   'Games Played: ', games_played))) +
    scale_y_continuous(labels = scales::percent) + 
    geom_hline(aes(yintercept = league_average_ts), alpha = 0.8, linetype = "dashed") +
    annotate(geom = 'text', label = 'League Average TS%', x = max(df$season_avg_ppg) * .95, y = league_average_ts * .98) +
    scale_fill_manual(values = c('light blue', 'orange')) +
    labs(title = 'Scoring Efficiency Tracker \n PPG vs TS% for all 20+ PPG Scorers',
         x = 'Average Points per Game',
         y = 'True Shooting Percentage',
         fill = NULL) +
    theme(legend.background = element_rect(color = "black"))
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(hoverlabel = list(bgcolor = "white"),
           legend = list(x = .78, y = 0.1))
  
}
# top20_plot(top_scorers)

top20_plot_playoffs <- function(df){
  p <- df %>%
    filter(playoffs_games_played >= 1,
           playoffs_avg_ppg >= 20) %>%
    ggplot(aes(playoffs_avg_ppg, playoffs_ts_percent, fill = top5_candidates)) +
    geom_point(size = 6, alpha = 0.7, pch = 21, color = 'black', aes(text = paste0(player, '<br>',
                                                                                   team, '<br>', # (', Wins, '-', Losses, ')', '<br>',
                                                                                   'PPG: ', round(playoffs_avg_ppg, 1), '<br>',
                                                                                   'TS%: ', round(playoffs_ts_percent * 100, 1), '%',
                                                                                   '<br>',
                                                                                   'Games Played: ', playoffs_games_played))) +
    scale_y_continuous(labels = scales::percent) + 
    geom_hline(aes(yintercept = league_average_ts), alpha = 0.8, linetype = "dashed") +
    annotate(geom = 'text', label = 'League Average TS%', x = max(df$playoffs_avg_ppg) * .95, y = league_average_ts * .98) +
    scale_fill_manual(values = c('light blue', 'orange')) +
    labs(title = 'Scoring Efficiency Tracker \n PPG vs TS% for all 20+ PPG Scorers in the NBA Playoffs',
         x = 'Average Points per Game',
         y = 'True Shooting Percentage',
         fill = NULL) +
    theme(legend.background = element_rect(color = "black"))
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(hoverlabel = list(bgcolor = "white"),
           legend = list(x = .78, y = 0.1))
  
}

# top20_plot_playoffs <- function(df){
#   p <- df %>%
#     ggplot(aes(avg_PTS, season_ts_percent, fill = Top5)) +
#     geom_point(size = 6, alpha = 0.7, pch = 21, color = 'black', aes(text = paste0(Player, ' (', Team, ')', '<br>',
#                                                                                    'Playoff PPG: ', round(avg_PTS, 1), '<br>',
#                                                                                    'Playoff TS%: ', round(season_ts_percent * 100, 1),
#                                                                                    '%', '<br>',
#                                                                                    'Playoff Games Played: ', GP_p))) +
#     scale_y_continuous(labels = scales::percent) +
#     geom_hline(aes(yintercept = league_average_ts), alpha = 0.8, linetype = "dashed") +
#     annotate(geom = 'text', label = 'League Average TS%', x = max(df$avg_PTS) * .95, y = league_average_ts * .95) +
#     scale_fill_manual(values = c('light blue', 'orange')) +
#     labs(title = 'NBA Playoffs Scoring Efficiency Tracker \n PPG vs TS% for all 20+ PPG Scorers',
#          x = 'Average Points per Game',
#          y = 'True Shooting Percentage',
#          fill = NULL) +
#     theme(legend.background = element_rect(color = "black"))
# 
#   ggplotly(p, tooltip = c('text')) %>%
#     layout(hoverlabel = list(bgcolor = "white"),
#            legend = list(x = .78, y = 0.85))
# 
# }
# top20_plot_playoffs(top_20pt_scorersp)
# player_analysis_choices <- c('PPG', 'Player Value Metric', 'TS%', 'Plus Minus')
# count_choices <- c('Top 20', 'Bottom 20', 'Full List')
# player_analysis_team_choices <- unique(rolling_avg %>% select(full_team) %>% arrange(full_team) %>% distinct() %>% pull())

player_analysis_plot_function <- function(df, plot_col, team_choice){
  
  if (plot_col == 'PPG'){
    plot_col_filter <- 'ppg_differential'
  }
  else if (plot_col == 'Player Value Metric'){
    plot_col_filter <- 'mvp_calc_differential'
  }
  else if (plot_col == 'Plus Minus'){
    plot_col_filter <- 'plusminus_differential'
  }
  else {
    plot_col_filter <- 'ts_differential'
  }
  
  # if (count_choice == 'Top 20'){
  #   count_choice_filter <- 20
  # }
  # else if (plot_col == 'Bottom 20'){
  #   count_choice_filter <- -20
  # }
  # else {
  #   count_choice_filter <- 1000
  # }
  
  p <- df %>%
    filter(full_team == !!team_choice) %>%
    # slice_max(n = count_choice_filter, order_by = .data[[plot_col_filter]]) %>%
    mutate(player = fct_reorder(player, .data[[plot_col_filter]])) %>%
    ggplot(aes(.data[[plot_col_filter]], player, fill = .data[[plot_col_filter]] > 0)) +
    geom_col(alpha = 0.4, color = 'black', aes(text = paste0(player, '<br>',
                                                             'Rolling Average Stats', '<br>',
                                                             'PPG: ', rolling_avg_pts, ' (', ppg_diff_rank, ' in differential)', '<br>',
                                                             'Player Value Metric: ', rolling_avg_mvp_calc, ' (', mvp_calc_diff_rank, ' in differential)', '<br>',
                                                             'TS%: ', rolling_avg_ts_percent * 100, '% (', ts_diff_rank, ' in differential)', '<br>',
                                                             'Plus Minus: ', rolling_avg_plusminus, ' (', plusminus_diff_rank, ')', '<br>',
                                                             '<br>',
                                                             'Season Average Stats', '<br>',
                                                             'PPG: ', season_avg_ppg, '<br>',
                                                             'Player Value Metric: ', player_mvp_calc_adj, '<br>',
                                                             'TS%: ', 100 * season_ts_percent, '%', '<br>',
                                                             'Plus Minus: ', season_avg_plusminus
                                                             )
      )
    ) +

    labs(x = paste0('Player Rolling Average ', plot_col, ' Differential'),
         y = 'Player',
         title = paste0('Rolling Average ', plot_col, ' Last 10 Games'),
         fill = NULL) +
    theme(legend.position = 'none')
  
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(hoverlabel = list(bgcolor = "white"))
}

# test_analysis_choice <- 'PPG'
# test_count_choice <- 'Top 20'
# 
# player_analysis_plot_function(rolling_avg, test_analysis_choice, test_count_choice)

df_prac <- rolling_avg %>%
  slice_max(n = 20, order_by = ppg_differential)

team_ppg_plot <- function(df){
  p <- df %>%
    filter(games_played >= 1) %>%
    ggplot(aes(season_avg_ppg, season_ts_percent, fill = top5_candidates)) +
    geom_point(size = 6, alpha = 0.7, pch = 21, color = 'black', aes(text = paste0(player, '<br>',
                                                                                   team, '<br>',
                                                                                   'PPG: ', round(season_avg_ppg, 1), '<br>',
                                                                                   'TS%: ', round(season_ts_percent * 100, 1), '%',
                                                                                   '<br>',
                                                                                   'Games Played: ', games_played))) +
    scale_y_continuous(labels = scales::percent) +
    geom_hline(aes(yintercept = league_average_ts), alpha = 0.8, linetype = "dashed") +
    annotate(geom = 'text', label = 'League Average TS%', x = max(df$season_avg_ppg) * .92, y = league_average_ts * .95) + ### change this to the actual value  ###
    scale_fill_manual(values = c('light blue', 'orange')) +
    labs(title = 'Player Efficiency Tracker \n PPG vs TS%',
         x = 'Average Points per Game',
         y = 'True Shooting Percentage',
         fill = NULL) +
    theme(legend.background = element_rect(color = "black"))
  
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(hoverlabel = list(bgcolor = "white"),
           legend = list(x = .78, y = 0.1))
  
}

# team_ppg_plot(top_scorers %>% filter(team == 'GSW'))

team_ratings_plot <- function(df){
  geom_logos <- function(mapping=NULL, data=NULL, inherit.aes=TRUE,
                         na.rm=FALSE, by="width", ...) {
    geom_image(mapping, data, inherit.aes=inherit.aes, na.rm=na.rm, ..., geom = 'logos', size = .13)
  }
  
  annotations <- data.frame(
    xpos = c(-Inf,-Inf, -Inf, -Inf, Inf, Inf, Inf, Inf),
    ypos =  c(min(df$drtg), max(df$drtg), -Inf, Inf, min(df$drtg), max(df$drtg), -Inf, Inf),
    annotateText = c("+ Defense","- Offense ", "-  Offense ", "- Defense",
                     "+ Defense", "+ Offense", "+ Offense ", "- Defense"),
    hjustvar = c(0, 0, 0, 0, 1, 1, 1, 1) ,
    vjustvar = c(1,0, 1, 0, 1, 0, 1,0),
    fillbby = c('#2ECC71', '#CD6155', '#CD6155', '#CD6155', '#2ECC71', '#2ECC71', '#2ECC71', '#CD6155')) #<- adjust
  
  df %>%
    ggplot(aes(ortg, drtg)) +
    geom_vline(xintercept = mean(df$drtg), linetype = "dashed") +
    geom_hline(yintercept = mean(df$ortg), linetype = "dashed") +
    geom_logos(aes(image = team_logo)) +
    geom_label(data = annotations, aes(x = xpos, y = ypos, hjust = hjustvar, vjust = vjustvar, label = annotateText, size = 4.5),
               fill = annotations$fillbby) +
    scale_y_reverse() +
    labs(title = 'Offensive vs Defensive Ratings',
         x = 'Offensive Rating',
         y = 'Defensive Rating') +
    theme(legend.position = 'none',
          plot.title = element_text(face = "bold", size = rel(1.35), hjust = 0.5))
  
}

team_choices <- standings %>%
  arrange(team_full) %>%
  select(team_full) %>%
  distinct() %>%
  pull()

team_choices_social <- standings %>%
  arrange(team) %>%
  select(team) %>%
  distinct() %>%
  pull()


mov_plot <- function(df){
  cols <- c('W' = 'dark green', 'L' = 'red')
  p <- df %>%
    ggplot(aes(date, mov)) +
    geom_col(alpha = 0.7, color = 'black', aes(fill = outcome, text = paste0(date, '<br>',
                                                            outcome, ' vs ', opponent, '<br>',
                                                            'Scoreline: ', pts_scored, ' - ', pts_scored_opp, '<br>',
                                                            'Margin of Victory: ', mov, '<br>',
                                                            'Record: ', row_record))) +
    scale_y_continuous(breaks = c(-50, -45, -40, -35, -30, -25, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    scale_fill_manual(values = cols) +
    labs(x = NULL,
         y = 'Margin of Victory',
         title = paste0(df$full_team[1], ' Game Log History \n 2022-23 NBA Season'),
         fill = 'Outcome')
  
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(hoverlabel = list(bgcolor = "white"))
  
}

schedule_plot <- function(df){
  p <- df %>% 
    ungroup() %>%
    mutate(Team1 = fct_reorder(Team1, avg_opp_rank)) %>%
    ggplot(aes(avg_opp_rank, Team1, fill = legend)) +
    geom_col(aes(text = paste0(Team, '<br>',
                               'Remaining Schedule Difficulty Rank: ', new_seed, '<br>',
                               'Team Rank (as of Today): ', Rank, '<br>',
                               'Average Opponent Rank in upcoming games: ', avg_opp_rank, '<br>', '<br>',
                               'Remaining Games vs Top 10 Teams: ', top10, ' (', top10_rank, ')', '<br>',
                               'Remaining Games vs Middle 10 Teams: ', mid10, ' (', mid10_rank, ')', '<br>',
                               'Remaining Games vs Bottom 10 Teams: ', bot10, ' (', bot10_rank, ')'))) +
    # annotate(geom = "text", label = "<span style='color: #F8766D;'>  Harder Upcoming Schedule</span>",
    #          x = max(df$avg_opp_rank) * .93, y = 4) +
    # annotate(geom = "text", label = "<span style='color: #00BFC4;'> Easier Upcoming \n Schedule</span>",
    #          x = max(df$avg_opp_rank) * .98, y = 24) +
    scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
    labs(y = NULL,
         x = 'Average Opponent Rank',
         title = 'Strength of Schedule Breakdown for the remaining Season',
         fill = NULL)
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(hoverlabel = list(bgcolor = "white"),
           legend = list(x = .83, y = 0.1))
}

# schedule_plot(schedule_plot_df)

regular_valuebox_function <- function(df){
  if (nrow(df) > 0){
    valueBox(value = paste0(df$wins, '-', df$losses), paste0("Win / Loss Record ",
                                                                 "(" , df$rank, " in ", df$conference, " Conference)"), icon = icon("list"), color = "blue")
  }
  else {
    valueBox(value = paste0('No Data Available'), "Team Hasn't Played Yet", icon = icon("list"), color = "blue")
  }
}

game_types_plot <- function(df, season_type = "Regular Season"){
  p <- df %>%
    filter(type == season_type) %>%
    ggplot(aes(game_type, pct_total)) +
    geom_col(position = 'dodge', color = 'black', aes(text = paste0(game_type, 's account for ', round(pct_total * 100, 1), '% of all',
                                                   ' games played.', '<br>', 'Number of Observations: ', n, '<br>', '<br>',
                                                   game_type, 's are defined as games that were decided by ', explanation))) +
    scale_y_continuous(labels = percent_format(),
                       limits = c(0, 0.5)) +
    labs(x = 'Game Type',
         y = 'Percent of Total Games',
         fill = NULL,
         title = 'Game Type Distribution for 2022-23 NBA Season') +
    theme_jacob() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = 'top')
  
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(legend = list(orientation = "h", x = 0.35, y = 1.03),
           hoverlabel = list(bgcolor = "white"))
}

reddit_comment_plot <- function(df, team_choice){
  p <- df %>%
    filter(team == team_choice) %>%
    ggplot(aes(scrape_date, num_comments, fill = game_outcome)) +
    geom_col(position = 'dodge', color = 'black', aes(text = paste0(scrape_date, "<br>",
                                                                    "Total Comments: ", num_comments, "<br>",
                                                                    "Compound Sentiment: ", avg_compound * 100, "%", "<br>",
                                                                    "Avg Compound Sentiment: ", round(mean(avg_compound), 3) * 100, "%", "<br>",
                                                                    "Previous day's Game Outcome: ", game_outcome)
                                                      )) +
    scale_x_date(date_breaks = "3 month", date_labels = "%B %Y") +
    scale_fill_manual(values = c("#AA3929", "#878484", "#46b051")) +
    labs(x = NULL,
         y = '# of Comments',
         fill = "Game Legend",
         title = 'Reddit Comments Analysis using User Flairs') +
    theme_jacob() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = 'top')
  
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(hoverlabel = list(bgcolor = "white"))
}

reddit_sentiment_plot <- function(df, team_choice){
  p <- df %>%
    filter(team == team_choice) %>%
    ggplot(aes(scrape_date, avg_compound, fill = game_outcome)) +
    geom_col(position = 'dodge', color = 'black', aes(text = paste0(scrape_date, "<br>",
                                                                    "Total Comments: ", num_comments, "<br>",
                                                                    "Compound Sentiment: ", avg_compound * 100, "%", "<br>",
                                                                    "Avg Compound Sentiment: ", round(mean(avg_compound), 3) * 100, "%", "<br>",
                                                                    "Previous day's Game Outcome: ", game_outcome)
    )) +
    scale_x_date(date_breaks = "3 month", date_labels = "%B %Y") +
    scale_fill_manual(values = c("#AA3929", "#878484", "#46b051")) +
    labs(x = NULL,
         y = 'Average Sentiment Score',
         fill = "Game Legend",
         title = 'Reddit Sentiment Analysis using User Flairs') +
    theme_jacob() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = 'top')
  
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(hoverlabel = list(bgcolor = "white"))
}


contracts_dist_plot <- function(df){
  df %>%
    select(count, salary_rank, pct_total, pvm_rank) %>%
    distinct() %>%
    ggplot(aes(count, salary_rank, fill = pvm_rank, label = scales::percent(pct_total))) +
    geom_col() +
    geom_text(aes(x = count, y = salary_rank, label = scales::percent(pct_total)), position = position_dodge(width = 1),
              hjust = -0.07,
              size = 4.5) +
    scale_x_continuous(limits = c(0, 250)) +
    labs(x = 'Number of Contracts',
         y = NULL,
         title = 'Distribution of Contracts in the 2022-23 NBA Season') +
    theme_minimal(base_size=10, base_family="Gill Sans MT") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite')
    )
}

value_plot <- function(df){
  p <- df %>%
    ggplot(aes(as.numeric(salary), player_mvp_calc_avg, fill = color_var)) +
    geom_point(aes(text = paste0(player, '<br>',
                                 team, '<br>',
                                 'Salary: $', formatC(salary, format = 'f', big.mark = ",", digits = 0), '<br>',
                                 'Player Value Metric: ', player_mvp_calc_avg, '<br>',
                                 'Games Played: ', games_played, '<br>',
                                 'Games Missed: ', games_missed, ' (Player Value Penalized ', pct_penalized * 100, '%)', '<br>',
                                 # 'Games Missed: ', games_missed, '<br>',
                                 player, ' is in the top ', percentile_rank, '% Percentile for all players in this Salary Group (',
                                 salary_rank, ')')),
               size = 2.5, shape = 21, alpha = 0.7) +
    scale_x_continuous(labels = label_dollar()) +
    scale_y_continuous(limits = c(-5, 55, breaks = c(-5, 5, 15, 25, 35, 45, 55))) +
    labs(y = 'Player Value Metric',
         x = 'Salary',
         title = 'What are the least & most valuable contracts in the 2022-23 NBA Season ?',
         fill = 'Legend') +
    scale_fill_manual(values=c("red", "green", "grey70", 'purple'))
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(hoverlabel = list(bgcolor = "white"))
}

# value_plot(contracts_value)


gp_valuebox_function <- function(df){
  if (df$upcoming_games[1] == 1){
    valueBox(
      value = format(df$upcoming_game_date[1], '%A, %B %d'), HTML(paste0("Next Gameday Date <br> <br> ", df$upcoming_games[1],
                                        " Upcoming Game")),
      icon = icon("calendar"), color = "blue"
    )
  }
  else if (df$upcoming_games[1] == 0){
    valueBox(
      value = format(Sys.Date(), '%A, %B %d'), HTML(paste0("No Upcoming Games")),
      icon = icon("calendar"), color = "blue"
    )
  }
    
  else {
    valueBox(
      value = format(df$upcoming_game_date[1], '%A, %B %d'), HTML(paste0("Next Gameday Date <br> <br> ", df$upcoming_games[1],
                                        " Upcoming Games | See Schedule")),
      icon = icon("calendar"), color = "blue"
    )
  }
}

team_contract_value_plot <- function(df){
  p <- df %>%
    ggplot(aes(team_pct_salary_earned, team, fill = win_percentage, text = paste0(team, '<br>',
                                                                           'Record: ', record, '<br>',
                                                                           'Total Contract Value Earned: ', (team_pct_salary_earned * 100), '%', '<br>',
                                                                           'Win Percentage: ', round(win_percentage * 100, 2), '%'))) +
    geom_col() +
    geom_vline(aes(xintercept = mean(team_pct_salary_earned), alpha = 0.5)) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_gradient(low = 'red', high = 'green', labels = percent_format()) +
    annotate('text', x = mean(df$team_pct_salary_earned) + 0.07, y = 3,
             label = paste0(round(mean(df$team_pct_salary_earned * 100), 2), '% (League Average)')) +
    labs(x = '% of Total Contract Value Earned',
         y = NULL,
         title = 'Which Teams are missing the most Contract Value this Season?',
         fill = 'Win %')
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(legend = list(orientation = "h", x = 0.35),
           hoverlabel = list(bgcolor = "white"))
}

vegas_plot <- function(df) {
  p <- df %>%
    ggplot(aes(wins_differential, team, fill = over_under)) +
    geom_col(aes(text = paste0(team_full, '<br>',
                               'Current W/L Projection: ', projected_wins, '-', projected_losses, '<br>',
                               "Vegas' Preseason Over/Under for Wins: ", predicted_stats, '<br>',
                               'Wins Differential: ', wins_differential))) +
    scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
    annotate(geom = "text", label = "<span style='color: #00BFC4;'>  Exceeding Expectations</span>",
             x = min(df$wins_differential) * .45, y = 24.5, size = 4.5) +
    annotate(geom = "text", label = "<span style='color: #F8766D;'> Underperforming</span>",
             x = max(df$wins_differential) * .30, y = 5.5, size = 4.5) +
    labs(x = 'Predicted Wins Differential',
         y = NULL,
         title = paste0('Vegas Preseason Over / Under Odds Tracker for the 2022-23 NBA Season')) +
    theme_jacob() +
    theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(hoverlabel = list(bgcolor = "white"))
}

# vegas_plot(preseason_odds)

advanced_sos_plot <- function(df){
  p <- df %>%
    ggplot(aes(pct_vs_below_500, team, fill = avg_win_pct_opp >= 0.505)) +
    geom_col(aes(text = paste0(team, '<br>',
                               'Record: ', record, '<br>', 
                               'Team Win %: ', round(win_pct * 100, 1), '%', '<br>',
                               'Average Opponent Win %: ', avg_win_pct_opp * 100, '%', '<br>', 
                               '<br>',
                               'Record vs Below .500 Teams: ', below_record, '<br>',
                               'Record vs Above .500 Teams: ', above_record, '<br>',
                               '<br>',
                               'Home Record: ', home_record, '<br>',
                               'Road Record: ', road_record, '<br>',
                               '<br>',
                               round(pct_vs_below_500 * 100, 1), '% of games played have been vs Below .500 Teams', '<br>',
                               round(pct_vs_above_500 * 100, 1), '% of games played have been vs Above .500 Teams'))) +
    scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
    scale_x_continuous(labels = percent_format()) +
    annotate(geom = "text", label = "<span style='color: #F8766D;'>  Faced Harder \n Competition</span>",
             x = max(df$pct_vs_below_500) * .95, y = 4) +
    annotate(geom = "text", label = "<span style='color: #00BFC4;'> Faced Easier \n Competition</span>",
             x = max(df$pct_vs_below_500) * .95, y = 16) +
    labs(x = '% of Games Played vs Below .500 Teams',
         y = NULL,
         title = paste0('Team Strength of Schedule for the 2022-23 NBA Season')) +
    theme_jacob() +
    theme(legend.position = 'none')
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(hoverlabel = list(bgcolor = "white"))
}

# advanced_sos_plot(past_schedule_analysis)

future_schedule_analysis_plot <- function(df){
  p <- df %>%
    ggplot(aes(pct_games_left_above_500, team, fill = avg_win_pct_opp >= 0.5)) +
    geom_col(aes(text = paste0(team, '<br>',
                               'Average Opp. Win % for Remaining Games: ', (avg_win_pct_opp * 100), '%', '<br>',
                               '<br>',
                               'Road Games Left: ', road_games_left_count, ' (', (pct_games_left_road * 100), '%)', '<br>',
                               'Home Games Left: ', home_games_left_count, ' (', (pct_games_left_home * 100), '%)', '<br>', 
                               '<br>',
                               '<br>',
                               'Games Left vs Below .500 Teams: ', below_500_games_left_count, ' (', (pct_games_left_below_500 * 100), '%)', '<br>',
                               'Games Left vs Above .500 Teams: ', above_500_games_left_count, ' (', (pct_games_left_above_500 * 100), '%)'))) +
    scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
    scale_x_continuous(labels = percent_format()) +
    annotate(geom = "text", label = "<span style='color: #F8766D;'>  Harder Upcoming \n Competition</span>",
             x = max(df$pct_games_left_above_500) * .985, y = 16, size = 3) +
    annotate(geom = "text", label = "<span style='color: #00BFC4;'> Easier Upcoming \n Competition</span>",
             x = max(df$pct_games_left_above_500) * .985, y = 4, size = 3) +
    labs(x = '% of Games Left vs Above .500 Teams',
         y = NULL,
         title = paste0('Future Strength of Schedule for the 2022-23 NBA Season')) +
    theme_jacob() +
    theme(legend.position = 'none')
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(hoverlabel = list(bgcolor = "white"))
  
}

# future_schedule_analysis_plot(future_schedule_analysis)

blown_leads_plot <- function(df, filter_type = 'Regular Season'){
  p <- df %>%
    filter(season_type == filter_type) %>%
    mutate(team = fct_reorder(team, net_comebacks)) %>%
    ggplot(aes(net_comebacks, team, fill = net_comebacks <= 0)) +
    geom_col(aes(text = paste0(team, '<br>',
                               '# of 10 pt Deficit Comebacks: ', team_comebacks_10pt, ' (', comeback_rank, ')', '<br>',
                               '# of 10 pt Blown Leads: ', blown_leads_10pt, ' (', blown_lead_rank, ')', '<br>',
                               'Net Comebacks: ', net_comebacks, ' (', net_rank, ')')), show.legend = FALSE) +
    scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
    labs(x = 'Net Comebacks',
         y = NULL,
         title = 'Team Comebacks & Blown Leads Analysis') +
    theme_jacob() +
    theme(legend.position = 'none')
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(hoverlabel = list(bgcolor = "white"))
}



# blown_leads_plot(team_blown_leads)

### gt table functions
gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "#585d63", weight = px(2)
      ),
      locations = cells_body(
        columns = everything(),
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(2),
       table.border.top.color = "#ffffff",
      table.border.bottom.color = "#ffffff",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(2),
      column_labels.border.top.color = "#ffffff",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(2),
      source_notes.font.size = 12,
      table.font.size = 13,
      heading.align = "left",
      ...
    ) 
}
team_gt_table <- function(df){
  df %>%
    gt() %>%
    gt_theme_538() %>%
    text_transform(
      locations = cells_body(columns = team_logo),
      fn = function(x) {
        local_image(
          filename = df$team_logo,
          height = 45
        )}) %>%
    text_transform(
      locations = cells_body(columns = opp_logo),
      fn = function(x) {
        local_image(
          filename = df$opp_logo,
          height = 45
        )}) %>%
    # https://stats.nba.com/media/img/teams/logos/BOS_logo.svg
    # text_transform(locations = cells_body(c(team_logo)),
    #                fn = function(x) {
    #                  web_image(url = x, height = 45)
    #                }) %>%
    # text_transform(locations = cells_body(c(opp_logo)),
    #                fn = function(x) {
    #                  web_image(url = x, height = 45)
    #                }) %>%
    cols_hide(columns = c(pts_color, opp_pts_color)) %>%
    cols_label(team_logo = "", opp_logo = "", new_loc = "", pts_scored = "PTS", max_team_lead = "MAX LEAD",
               pts_scored_opp = 'PTS', max_opponent_lead = "MAX LEAD", mov = 'MARGIN OF VICTORY', opponent = "OPP") %>%
    tab_style(
      style = cell_fill(color = "#9362DA"),
      locations = cells_body(
        columns = c(pts_scored),
        rows = pts_color == 1)) %>%
    tab_style(
      style = cell_fill(color = "#3fb7d9"),
      locations = cells_body(
        columns = c(pts_scored),
        rows = pts_color == 2)) %>%
    tab_style(
      style = cell_fill(color = "#e04848"),
      locations = cells_body(
        columns = c(pts_scored),
        rows = pts_color == 3)) %>%
    tab_style(
      style = cell_fill(color = "#9362DA"),
      locations = cells_body(
        columns = c(pts_scored_opp),
        rows = opp_pts_color == 1)) %>%
    tab_style(
      style = cell_fill(color = "#3fb7d9"),
      locations = cells_body(
        columns = c(pts_scored_opp),
        rows = opp_pts_color == 2)) %>%
    tab_style(
      style = cell_fill(color = "#e04848"),
      locations = cells_body(
        columns = c(pts_scored_opp),
        rows = opp_pts_color == 3)) %>%
    tab_header(
      title = md("Team Stats"),
      subtitle = paste0("From ", length(pbp_games_yesterday), " Games Played on ", format(most_recent_date$date, "%A, %B %d"))) %>%
    opt_align_table_header(align = "center") %>%
    cols_align(
      align = "center",
      columns = everything()
    )
}
# team_gt_table(recent_games_teams)

player_gt_table <- function(df){
  df %>%
    gt() %>%
    gt_theme_538() %>%
    text_transform(
      locations = cells_body(c(player_logo)),
      fn = function(x){
        web_image(url = x, height = 34)
      }
    ) %>%
    cols_hide(columns = c(pts_color, ts_color)) %>%
    cols_label(player_logo = "") %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )) %>%
    fmt_currency(columns = c(Salary), currency = 'USD', decimals = 0) %>%
    fmt_percent(columns = c(`TS%`)) %>%
    tab_style(
      style = cell_fill(color = "#9362DA"),
      locations = cells_body(
        columns = c(pts),
        rows = pts_color == 1 # color rdef IF number > 80
      )
    ) %>%
    tab_style(
      style = cell_fill(color = "#3fb7d9"),
      locations = cells_body(
        columns = c(pts),
        rows = pts_color == 2 # color rdef IF number > 80
      )
    ) %>%
    tab_style(
      style = cell_fill(color = "#e04848"),
      locations = cells_body(
        columns = c(pts),
        rows = pts_color == 3 # color rdef IF number > 80
      )
    ) %>%
    data_color(
      columns = c(`TS%`),
      colors = scales::col_numeric(
        palette = c("white", "#19e661"),
        domain = NULL)) %>%
    tab_header(
      title = md("Top Performers by **Points** Scored"),
      subtitle = paste0("From ", length(pbp_games_yesterday), " Games Played on ", format(most_recent_date$date, "%A, %B %d"))) %>%
    opt_align_table_header(align = "center") %>%
    cols_align(
      align = "center",
      columns = everything()
    )
  
}

injured_players_gt_table <- function(df){
  df %>%
    gt() %>%
    gt_theme_538() %>%
    text_transform(
      locations = cells_body(c(player_logo)),
      fn = function(x){
        web_image(url = x, height = 34)
      }
    ) %>%
    # cols_hide(columns = c(pts_color, ts_color)) %>%
    cols_label(player_logo = "") %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )) %>%
    fmt_percent(columns = c(`Average TS%`)) %>%
    tab_header(
      title = md("Injury Report"),
      subtitle = paste0("")) %>%
    opt_align_table_header(align = "center") %>%
    cols_align(
      align = "center",
      columns = everything()
    )
  
}

schedule_gt_table <- function(df){
  df %>%
    gt() %>%
    gt_theme_538() %>%
    text_transform(locations = cells_body(c(team_logo_home)),
                   fn = function(x) {
                     web_image(url = x, height = 45)
                   }) %>%
    text_transform(locations = cells_body(c(team_logo_away)),
                   fn = function(x) {
                     web_image(url = x, height = 45)
                   }) %>%
    cols_hide(columns = c(home_is_great_value, away_is_great_value)) %>%
    cols_label(Date = "DATE", `Start Time (EST)` = "START TIME (EST)", team_logo_home = "", `Home Team` = 'Home Team',
              `Home Predicted Win %` = "Home Predicted Win %", team_logo_away = "",
               `Road Team` = 'Road Team', `Road Predicted Win %` = "Road Predicted Win %", `Average Team Rank` = 'Average Team Rank') %>%
    tab_style(
      style = cell_fill(color = "#9362DA"),
      locations = cells_body(
        columns = c(`Home Team`),
        rows = home_is_great_value == 1)) %>%
    tab_style(
      style = cell_fill(color = "#9362DA"),
      locations = cells_body(
        columns = c(`Road Team`),
        rows = away_is_great_value == 1)) %>%
    tab_header(
      title = md("Upcoming Schedule"),
      subtitle = paste0(bans$upcoming_games[1],  " Upcoming Games on ", format(most_recent_date$date, "%A, %B %d"))) %>%
    opt_align_table_header(align = "center") %>%
    cols_align(
      align = "center",
      columns = everything()
    )
}

# pbp choices
pbp_games_yesterday <- unique(pbp_data$game_description)

get_leading_times <- function(df){
  team_colors <- df %>%
    select(scoring_team, scoring_team_color) %>%
    distinct()
  
  try <- df %>%
    select(scoring_team, quarter, time_quarter, leading_team_text, time_remaining_final) %>%
    filter(scoring_team != 'TIE') %>%
    mutate(prev_time = lag(time_remaining_final),
           prev_time = replace_na(prev_time, 48.0),
           time_difference = round(60 * (prev_time - time_remaining_final)),
           time_difference = replace_na(time_difference, 0))
  
  last_record_team <- tail(try, 1)
  
  mydf <- data.frame(scoring_team = as.character(),
                     Trailing_time = as.numeric(),
                     Leading_time = as.numeric(),
                     Tied_time = as.numeric())
  
  # there was never a tie.
  try <- try %>%
    add_row(scoring_team = last_record_team$scoring_team, quarter = last_record_team$quarter, time_quarter = as.character(0:00),
            leading_team_text = last_record_team$leading_team_text, time_remaining_final = last_record_team$time_remaining_final,
            prev_time = time_remaining_final, time_difference = round(60 * (prev_time - 0))) %>%
    group_by(scoring_team, leading_team_text) %>%
    summarize(time = sum(time_difference)) %>%
    ungroup() %>%
    pivot_wider(names_from = leading_team_text,
                names_glue = '{leading_team_text}_{.value}',
                values_from = time) %>%
    mutate(TIE_time = if("TIE_time" %in% colnames(.)) TIE_time else 0)
  
  df2 <- mydf %>%
    full_join(try) %>%
    mutate(Leading_time = replace_na(Leading_time, 0),
           TIE_time = replace_na(TIE_time, 0),
           Trailing_time = replace_na(Trailing_time, 0),
           opp_leadtime = rev(Trailing_time),
           opp_tiedtime = rev(TIE_time),
           tot_leadtime = Leading_time + opp_leadtime,
           tot_trailtime = rev(tot_leadtime),
           tot_tiedtime = TIE_time + opp_tiedtime,
           tot_time = tot_leadtime + tot_trailtime + tot_tiedtime,
           pct_leadtime = round(tot_leadtime / tot_time, 3),
           pct_tiedtime = round(tot_tiedtime / tot_time, 3)) %>%
    left_join(team_colors) %>%
    mutate(text = paste0("<span style='color:", scoring_team_color, "';>", scoring_team, '</span> led for ',
                         pct_leadtime * 100, ' % of the Game'),
           tied_text = paste0('The teams were tied for ', (pct_tiedtime * 100), ' % of the Game')) %>%
    select(text, tied_text)
  
  
  return(df2)
  
}

game_event_plot <- function(df){
  df_val <- tail(df, 1) %>%
    select(quarter) %>%
    pull()
  
  get_breaks <- function(df){
    if (df_val == '1st OT'){
      breaks = c(48, 36, 24, 12, 0, -5)
      return(breaks)
    }
    else if (df_val == '2nd OT'){
      breaks = c(48, 36, 24, 12, 0, -5, -10)
      return(breaks)
    }
    else if (df_val == '3rd OT'){
      breaks = c(48, 36, 24, 12, 0, -5, -10, -15)
      return(breaks)
    }
    else {
      breaks = c(48, 36, 24, 12, 0)
      return(breaks)
    }
  }
  
  get_labels <- function(df){
    if (df_val == '1st OT'){
      labels = c("1st Quarter", "2nd Quarter", "3rd Quarter", "4th Quarter", "End 4th", "1st OT")
      return(labels)
    }
    else if (df_val == '2nd OT'){
      labels = c("1st Quarter", "2nd Quarter", "3rd Quarter", "4th Quarter", "End 4th", "1st OT", "2nd OT")
      return(labels)
    }
    else if (df_val == '3rd OT'){
      labels = c("1st Quarter", "2nd Quarter", "3rd Quarter", "4th Quarter", "End 4th", "1st OT", "2nd OT", "3rd OT")
      return(labels)
    }
    else if (df_val == '4th OT'){
      labels = c("1st Quarter", "2nd Quarter", "3rd Quarter", "4th Quarter", "End 4th", "1st OT", "2nd OT", "3rd OT", "4th OT")
      return(labels)
    }
    else {
      labels = c("1st Quarter", "2nd Quarter", "3rd Quarter", "4th Quarter", "End 4th")
      return(labels)
    }
  }
  
  lead_text <- function(df){
    if (abs(min(df$margin_score)) > abs(max(df$margin_score))){
      return (min(df$margin_score) * .9) # if the score is mainly negative lets set y to be down at the bottom
    }
    else {
      return (max(df$margin_score) * .9) # if the score is mainly positive lets set y to be at the top.  
    }
  }
  
  team_names <- unique(df$game_description)
  
  lead_times <- get_leading_times(df)
  y_loc <- lead_text(df)
  
  p <- df %>%
    ggplot(aes(time_remaining_final, margin_score)) +
    geom_point(aes(color = scoring_team_color, text = paste0(time_quarter, ' in the ', quarter, '<br>',
                                                             play, '<br>',
                                                             leading_team, ' Leading ', score_home, '-', score_away)),
               show.legend = FALSE) +
    geom_line(alpha = 0.4) +
    geom_hline(yintercept = 0, alpha = 0.5) +
    scale_x_reverse(breaks = get_breaks(df),
                    labels = get_labels(df)) +
    scale_color_identity() +
    annotate(geom = "text", label = lead_times$text[1],
             x = 36, y = y_loc, hjust = 0) +
    annotate(geom = "text", label = lead_times$text[2],
             x = 36, y = y_loc * .92, hjust = 0) +
    annotate(geom = "text", label = lead_times$tied_text[1],
             x = 36, y = y_loc * .84, hjust = 0) +
    labs(x = NULL,
         y = 'Score Differential',
         title = paste0(df$away_fill[1], ' Vs. ', df$home_fill[1])) +
    theme(legend.position = 'none')
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(hoverlabel = list(bgcolor = "white")) %>%
    style(textposition = "left")
  
  
}

sentiment_analysis_header = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 1, 'Scrape Date'),
      th(rowspan = 1, 'User'),
      th(rowspan = 1, 'Flair'),
      th(rowspan = 1, 'Comment'),
      th(colspan = 4, 'Sentiment Analysis'),
      th(rowspan = 1, 'URL'),
    ),
    tr(
      lapply(rep(c('Compound Sentiment Score', 'Pos', 'Neutral', 'Neg'), 1), th)
    )
  )
))

gc()

# test dataframe
# schedule_tonight <- tibble(
#   Date = c('2022-09-30', '2022-09-30', '2022-09-30'),
#   `Start Time (EST)` = c('6:30pm', '7:30pm', '8:30pm'),
#   `Home Team` = c('Golden State Warriors (-230)', 'Atlanta Hawks (-310)', 'Boston Celtics (-170)'),
#   `Road Team` = c('Brooklyn Nets (+170)', 'Chicago Bulls (+340)', 'Los Angeles Lakers (+170)'),
#   `Average Team Rank` = c(2.5, 4.5, 6),
#   home_moneyline_raw = c(-230, -310, -170),
#   home_team_logo = c('aaa', 'bbb', 'ccc'),
#   home_team_predicted_win_pct = c(.449, .73, .76),
#   away_moneyline_raw = c(170, 340, 170),
#   away_team_logo = c('xxx', 'yyy', 'zzz'),
#   away_team_predicted_win_pct = c(.76, .499, .501)
# ) %>%
#   mutate(
#     home_is_great_value = case_when(
#       ((home_moneyline_raw >= -130 | home_moneyline_raw >= 200) & home_team_predicted_win_pct >= 0.55) |
#         (home_moneyline_raw >= 170 & home_team_predicted_win_pct >= 0.50) ~ 1,
#       TRUE ~ 0
#     ),
#     away_is_great_value = case_when(
#       ((away_moneyline_raw >= -130 | away_moneyline_raw >= 200) & away_team_predicted_win_pct >= 0.55) |
#         (away_moneyline_raw >= 170 & away_team_predicted_win_pct >= 0.50) ~ 1,
#       TRUE ~ 0
#     )
#   ) %>%
#   rename(`Home Predicted Win %` = home_team_predicted_win_pct, `Road Predicted Win %` = away_team_predicted_win_pct)
# 
# datatable(schedule_tonight, rownames = FALSE,
#           options = list(pageLength = 10,
#                          columnDefs = list(list(visible=FALSE, targets = c(5, 6, 8, 9, 11, 12)))),
#           caption = htmltools::tags$caption(
#             style = 'caption-side: bottom;',
#             htmltools::em('Win % Predictions created via Logistic Regression ML Model')
#           )
# ) %>%
#   formatPercentage('Home Predicted Win %', 1) %>%
#   formatPercentage('Road Predicted Win %', 1) %>%
#   formatStyle("Road Team", valueColumns = "away_is_great_value", backgroundColor = styleEqual(c(1), c('#4BD33A'))) %>%
#   formatStyle("Home Team", valueColumns = "home_is_great_value", backgroundColor = styleEqual(c(1), c('#4BD33A')))

library(nbastatR)
library(tidyverse)
library(rvest)
library(xml2)
library(stringr)
library(lubridate)
library(extrafont)
library(ggrepel)
library(ggimage)
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(anytime)
library(runner)
library(fs)
library(scales)
library(jsonlite)
library(httr)
library(stringi)
library(ggtext)
library(bslib)
library(RMySQL)
library(gt)
library(shinythemes)

Sys.setenv (TZ="America/Los_Angeles")
#### AWS CONNECTION #####

aws_connect <- dbConnect(drv = RMySQL::MySQL(), dbname = Sys.getenv('aws_db'),
                         host = Sys.getenv('aws_host'),
                         port = as.integer(Sys.getenv('aws_port')),
                         user = Sys.getenv('aws_user'), password = Sys.getenv('aws_pw'))

today <- Sys.Date()
todayDate <- Sys.Date()
yesterday <- Sys.Date()-1
isSeasonActive <- TRUE
today <-  format(today, format = "%B %d, %Y")
updated_date <- strftime(Sys.time(), format = "%B %d, %Y - %I:%M %p %Z")

# custom theme
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


# data retrieval functions
get_gamelogs_data <- function(){
  if (isSeasonActive == TRUE & as.double(Sys.time() - file_info('data/gameLogs.csv')$change_time, units = 'hours') > 8.0){
    gameLogs <- dbReadTable(aws_connect, "aws_gamelogs")
    write_csv(gameLogs, 'data/gameLogs.csv')
    return(gameLogs)
  }
  else {
    df <- read_csv('data/gameLogs.csv')
    return(df)
  }
}

get_injuries_data <- function(){
  if (isSeasonActive == TRUE & as.double(Sys.time() - file_info('data/injuryData.csv')$change_time, units = 'hours') > 8.0){
    injuries <- dbReadTable(aws_connect, "aws_injuries")
    write_csv(injuries, 'data/injuryData.csv')
    return(injuries)
  }
  else {
    df <- read_csv('data/injuryData.csv')
    return(df)
  }
}

get_playbyplay_data <- function(){
  if (isSeasonActive == TRUE & as.double(Sys.time() - file_info('data/playbyplay_df.csv')$change_time, units = 'hours') > 8.0){
    playbyplay_df <- dbReadTable(aws_connect, "aws_raw_pbp_yesterday")
    write_csv(playbyplay_df, 'data/playbyplay_df.csv')
    return(playbyplay_df)
  }
  else {
    df <- read_csv('data/playbyplay_df.csv')
    return(df)
  }
}

get_team_opponent_shooting_data <- function(){
  if (isSeasonActive == TRUE & as.double(Sys.time() - file_info('data/opponent_shooting.csv')$change_time, units = 'hours') > 8.0){
    opponent_shooting <- dbReadTable(aws_connect, "aws_opponent_shooting")
    write_csv(opponent_shooting, 'data/opponent_shooting.csv')
    return(opponent_shooting)
  }
  else {
    df <- read_csv('data/opponent_shooting.csv')
    return(df)
  }
}

get_odds <- function(){
  if (isSeasonActive == TRUE & as.double(Sys.time() - file_info('data/new_odds.csv')$change_time, units = 'hours') > 8.0){
    odds_df <- dbReadTable(aws_connect, "aws_new_odds;")
    write_csv(odds_df, 'data/new_odds.csv')
    return(odds_df)
  }
  else {
    odds_df <- read_csv('data/new_odds.csv')
    return(odds_df)
  }
}

get_player_advanced_stats <- function(){
  if (isSeasonActive == TRUE & as.double(Sys.time() - file_info('data/player_advanced_stats.csv')$change_time, units = 'hours') > 8.0){
    stats_df <- dbReadTable(aws_connect, "aws_adv_player_stats")
    write_csv(stats_df, 'data/player_advanced_stats.csv')
    return(stats_df)
  }
  else {
    stats_df <- read_csv('data/player_advanced_stats.csv')
    return(stats_df)
  }
}



get_schedule <- function(){
  if (isSeasonActive == TRUE & as.double(Sys.time() - file_info('data/schedule.csv')$change_time, units = 'hours') > 8.0){
    currentmonth <- tolower(format(Sys.Date(), "%B"))
    nextmonth <- tolower(format(Sys.Date() + 30, "%B"))
    year <- 2021
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, 
                  "_games-", currentmonth, ".html")
    webpage <- read_html(url)
    col_names <- webpage %>% 
      html_nodes("table#schedule > thead > tr > th") %>% 
      html_attr("data-stat")    
    col_names <- c("game_id", col_names)
    dates <- webpage %>% 
      html_nodes("table#schedule > tbody > tr > th") %>% 
      html_text()
    dates <- as.data.frame(dates) %>%
      filter(dates != 'Playoffs')
    game_id <- webpage %>% 
      html_nodes("table#schedule > tbody > tr > th") %>%
      html_attr("csk")
    game_id <- game_id[!is.na(game_id)]
    data <- webpage %>% 
      html_nodes("table#schedule > tbody > tr > td") %>% 
      html_text() %>%
      matrix(ncol = length(col_names) - 2, byrow = TRUE)
    month_df1 <- as.data.frame(cbind(game_id, dates, data), stringsAsFactors = FALSE)
    names(month_df1) <- col_names
    
    write_csv(month_df1, 'data/schedule.csv')
    return(month_df1)
  }
  else {
    schedule <- read_csv('data/schedule.csv')
    return(schedule)
  }
}

get_contracts <- function(){
  url <- 'https://www.basketball-reference.com/contracts/players.html'
  webpage <- read_html(url)
  col_names <- webpage %>%
    html_table() %>%
    as.data.frame() %>%
    filter(Var.2 != 'Player',
           Salary != 'Salary') %>%
    select(Var.2, Var.3, Salary, Var.11) %>%
    rename(Player = Var.2, Team = Var.3, Salary = Salary, Total_Salary_Owed = Var.11) %>%
    mutate(Player = stri_trans_general(Player,  id = "Latin-ASCII"),
           Total_Salary_Owed = case_when(Total_Salary_Owed == "" ~ '0',
                                         TRUE ~ Total_Salary_Owed),
           Player = replace(Player, Player == 'J.J. Redick', 'JJ Redick'),
           Player = replace(Player, Player == 'T.J. Leaf', 'TJ Leaf'),
           Player = replace(Player, Player == 'Marcus Morris', 'Marcus Morris Sr.'),
           Player = replace(Player, Player == 'Danuel House', 'Danuel House Jr.'),
           Player = replace(Player, Player == 'Robert Williams', 'Robert Williams III'),
           Player = replace(Player, Player == 'Otto Porter', 'Otto Porter Jr.'),
           Player = replace(Player, Player == 'Kevin Knox', 'Kevin Knox II'),
           Player = replace(Player, Player == 'Lonnie Walker', 'Lonnie Walker IV'),
           Player = replace(Player, Player == 'Sviatoslav Mykhailiuk', 'Svi Mykhailiuk'),
           Player = replace(Player, Player == 'Harry Giles', 'Harry Giles III'),
           Player = replace(Player, Player == 'Wesley Iwundu', 'Wes Iwundu'),
           Player = replace(Player, Player == 'James Ennis', '	James Ennis III'),
           Salary2 = gsub("\\$", "", Salary),
           Salary3 = as.numeric(gsub("\\,", "", Salary2)),
           Salary4 = gsub("\\$", "", Total_Salary_Owed),
           Salary5 = as.numeric(gsub("\\,", "", Salary4)),
           Team = replace(Team, Team == 'BRK', 'BKN'),
           Team = replace(Team, Team == 'PHO', 'PHX'),
           Team = replace(Team, Team == 'CHO', 'CHA')) %>%
    select(Player, Team, Salary3, Salary5) %>%
    group_by(Player) %>%
    distinct() %>%
    ungroup() %>%
    rename(Salary = Salary3, `Total Salary Owed` = Salary5)
}

get_clean_foul_data <- function(df){
  df <- df %>%
    pivot_longer(descriptionPlayHome:descriptionPlayVisitor, names_to = 'fouls') %>%
    select(value, idGame) %>%
    filter(!is.na(value)) %>%
    filter(str_detect(value, regex('foul', ignore_case = TRUE))) %>%
    filter(!str_detect(value, 'Turnover')) %>%
    filter(!str_detect(value, 'FLAGRANT')) %>%
    separate(value, c("A", "B", "referee"), sep = "[(]") %>%
    mutate(referee = str_sub(referee, 1, str_length(referee)-1)) %>%
    separate(A, c('F', 'G', 'H'), sep = " ") %>%
    mutate(G = str_replace(G, "Jr.", ""),
           G = str_replace(G, "Sr.", ""),
           G = str_replace(G, "III", ""),
           G = str_replace(G, "Hill", ""),
           H = str_replace(H, "Offensive", "OFFENSIVECHARGE")) %>%
    unite("foul_type", G:H, sep = "") %>%
    select(referee, foul_type, idGame) %>%
    mutate(foul_type = toupper(foul_type),
           foul_type = str_replace(foul_type, "PERSONALTAKE", "PERSONAL"))
  
  return(df)
  
}

get_ord_numbers <- function(df){
  new_df <- case_when(df %in% c(11, 12, 13) ~ "th",
                      df %% 10 == 1 ~ 'st',
                      df %% 10 == 2 ~ 'nd',
                      df %% 10 == 3 ~ 'rd',
                      TRUE ~ 'th')
  new_df2 <- paste0(df, new_df)
  return(new_df2)
  
}


# Loading in Data 
acronyms <- read_csv('data/acronyms.csv')
injury_data <- get_injuries_data()
gameLogs <- get_gamelogs_data()
dataBREFTeamJoined <- read_csv('data/dataBREFTeamJoined.csv')
team_ratings <- read_csv('data/team_ratings.csv')
schedule <- get_schedule()
transactions <- read_csv('data/transactions.csv')
contracts <- read_csv('data/contracts.csv')
pbp_data <- get_playbyplay_data()
over_under <- read_csv('data/nba_odds.csv')
opponent_shooting <- read_csv('data/opponent_shooting.csv')
odds_df <- get_odds() %>%
  mutate(date = as.Date(date))
adv_stats <- read_csv('data/player_advanced_stats.csv')

dbDisconnect(aws_connect)


###### Data Extraction Complete ######
# Data Manipulation ----

full_team_names <- dataBREFTeamJoined %>%
  distinct(nameTeam, slugTeamBREF) %>%
  mutate(slugTeamBREF = replace(slugTeamBREF, slugTeamBREF == 'BRK', 'BKN')) %>%
  mutate(slugTeamBREF = replace(slugTeamBREF, slugTeamBREF == 'PHO', 'PHX')) %>%
  mutate(slugTeamBREF = replace(slugTeamBREF, slugTeamBREF == 'CHO', 'CHA')) %>%
  rename(Team = slugTeamBREF, FullName = nameTeam)

injury_data <- injury_data %>%
  left_join(full_team_names)

injury_data_count <- injury_data %>%
  group_by(Team) %>%
  summarise('Active Injuries' = n())

salary_Merge <- contracts %>%
  select(Player, Salary)

GP <- gameLogs %>%
  filter(Type == 'Regular Season') %>%
  group_by(Player) %>%
  summarise(GP = n())

GP_p <- gameLogs %>%
  filter(Type == 'Playoffs') %>%
  group_by(Player) %>%
  summarise(GP_p = n())

team_gp_df <- gameLogs %>%
  filter(Type == 'Regular Season') %>%
  select(Team, GameID, Opponent, Date) %>%
  distinct() %>%
  group_by(Team) %>%
  count() %>%
  rename(team_gp = n) %>%
  ungroup()

gameLogs_Two <- gameLogs %>%
  select(-isB2BFirst, -isB2B, -isB2BSecond, -Season) %>%
  group_by(Player, Type) %>%
  mutate(game_ts_percent = PTS / (2 * (FGA + (0.44 * FTA))),
         game_ts_percent = round(game_ts_percent, 3),
         season_ts_percent = sum(PTS) / (2 * (sum(FGA) + (0.44 * sum(FTA)))),
         season_ts_percent = round(season_ts_percent, 3),
         MVPCalc = (mean(PTS) + (0.5 * mean(PlusMinus)) + (2 * mean(STL + BLK)) + (0.5 * mean(TRB)) -
                      (1.5 * mean(TOV)) + (1.5 * mean(AST))),
         Date = as.Date(Date),
         MVPCalc = round(MVPCalc, 1),
         season_ppg = round(mean(PTS), 1)) %>%
  ungroup() %>%
  mutate(MVPCalc_game = (PTS + (0.5 * PlusMinus) + (2 * STL + BLK) + (0.5 * TRB) - TOV + AST)) %>%
  left_join(contracts %>% select(Player, Salary)) %>%
  left_join(GP) %>%
  left_join(GP_p) %>%
  left_join(full_team_names) %>%
  left_join(team_gp_df) %>%
  left_join(adv_stats) %>%
  mutate(games_missed = team_gp - GP,
         team_gp_var = 10 * round((team_gp / 72), 1),
         games_missed_penalty = case_when(games_missed == 0 ~ 1,
                                          games_missed <= team_gp_var ~ 1,
                                          games_missed > team_gp_var & MVPCalc >= 0 ~ (1 - (0.03 * (games_missed - team_gp_var))),
                                          games_missed > team_gp_var & MVPCalc < 0 ~ (1 + (0.03 * (games_missed - team_gp_var)))),
         pct_games_played = round(GP / team_gp, 3),
         abc3 = (MVPCalc),
         MVPCalc_adj = case_when(team_gp == GP ~ abc3,
                          team_gp - GP == team_gp_var ~ abc3,
                          team_gp - GP < team_gp_var ~ abc3,
                          team_gp - GP > team_gp_var ~ (abc3 * games_missed_penalty)),
         MVPCalc_adj = round(MVPCalc_adj, 1),
         abc_penalty = MVPCalc_adj - abc3,
         new_MVPCalc_adj = round(case_when(abs(abc_penalty) / abc3 >= 0.3 ~ abc3 * .67,
                                     abs(abc_penalty) / abc3 < 0.3 ~ MVPCalc_adj), 1),
         new_penalty = abc3 - new_MVPCalc_adj) %>%
  select(-team_gp_var, -abc3, -MVPCalc_adj, -games_missed_penalty, -pct_games_played)

league_average_ts <- gameLogs_Two %>%
  filter(Type == 'Regular Season') %>%
  mutate(ts = round((sum(PTS) / (2 * (sum(FGA) + (0.44 * sum(FTA))))), 3)) %>%
  select(ts) %>%
  distinct() %>%
  pull()

league_average_ts_playoffs <- gameLogs_Two %>%
  filter(Type == 'Playoffs') %>%
  mutate(ts = round((sum(PTS) / (2 * (sum(FGA) + (0.44 * sum(FTA))))), 3)) %>%
  select(ts) %>%
  distinct() %>%
  pull()


player_teams <- gameLogs_Two %>%
  filter(Type == 'Regular Season') %>%
  select(Player, Team, Date) %>%
  group_by(Player, Team) %>%
  filter(Date == max(Date)) %>%
  distinct() %>%
  ungroup() %>%
  group_by(Player) %>%
  filter(Date == max(Date)) %>%
  ungroup() %>%
  select(-Date) %>%
  rename(new_team = Team)

rm(gameLogs, team_gp_df, salary_Merge, adv_stats, GP_p)

gameLogs_yesterday <- gameLogs_Two %>%
  filter(Date == max(Date))

total_season_high <- gameLogs_Two %>%
  group_by(Player) %>%
  mutate(season_high = max(PTS)) %>%
  select(Player, season_high) %>%
  distinct() %>%
  ungroup()

player_season_high <- gameLogs_Two %>%
  filter(Type == 'Regular Season') %>%
  group_by(Player) %>%
  mutate(avg_ppg = round(mean(PTS), 1)) %>%
  select(Player, avg_ppg, season_ts_percent) %>%
  distinct() %>%
  ungroup() %>%
  left_join(total_season_high)

player_png <- read_csv('data/player_png.csv')

top_15_yesterday <- gameLogs_yesterday %>%
  select(Player, Team, PTS, game_ts_percent, Outcome, Salary) %>%
  top_n(15, PTS) %>%
  left_join(player_png) %>%
  arrange(desc(PTS)) %>%
  rename(`TS%` = game_ts_percent) %>%
  mutate(Rank = row_number()) %>%
  select(Rank, Player, Team, PTS, `TS%`, Outcome, Salary, logo) %>%
  left_join(player_season_high) %>%
  mutate(ppg_difference = PTS - avg_ppg,
         ts_difference = `TS%` - season_ts_percent,
         pts_color = case_when(PTS == season_high ~ 1,
                               PTS != season_high & ppg_difference >= 10 ~ 2,
                               ppg_difference <= -10 ~ 3,
                               TRUE ~ 0),
         ts_color = case_when(ts_difference >= 0.25 ~ 1,
                              ts_difference < 0.25 & ts_difference >= 0.15 ~ 2,
                              `TS%` <= 0.40 ~ 3,
                              TRUE ~ 0),
         Player = paste0(
           "<span style='font-size:16px; color:royalblue;'>",
           Player,
           "</span>",
           " <span style='font-size:12px; color:grey;'>",
           word(Team, start = -1), "</span>"),
         Player = map(Player, ~gt::html(as.character(.x)))
  ) %>%
  select(Rank, logo, Player, everything(), -Team, -avg_ppg, -season_high, -ppg_difference, -ts_difference, -season_ts_percent)

rm(player_season_high, player_png, total_season_high)

team_Wins_Yesterday <- gameLogs_yesterday %>%
  filter(Date == max(Date)) %>%
  group_by(Team, Opponent, Location, GameID, Outcome, Date) %>%
  summarise(Team_PTS_Scored = sum(PTS)) %>%
  ungroup() %>%
  rename('Team Points Scored' = Team_PTS_Scored) %>%
  select(-GameID, Team, Outcome, Location, 'Team Points Scored', Opponent, Date) %>%
  arrange(Outcome)

recent_Bans2 <- team_Wins_Yesterday %>%
  group_by(Location, Outcome) %>%
  summarise(Count = n()) %>%
  filter(Location == 'H') %>%
  ungroup() %>%
  mutate(WinP = Count / sum(Count),
         WinP = format(round(WinP, 2), nsmall = 2)) %>%
  mutate(Outcome = replace(Outcome, Outcome == 'L', 'Road Wins'),
         Outcome = replace(Outcome, Outcome == 'W', 'Home Wins')) %>%
  select(-Location) %>%
  rename(Location = Outcome, 'Win Percentage' = WinP) %>%
  pivot_wider(names_from = Location, values_from = c(Count, 'Win Percentage'))

recent_Bans <- team_Wins_Yesterday %>%
  summarise(NumberofGames = n(),
            Date = max(Date)) %>%
  mutate(NumberofGames = NumberofGames / 2) %>%
  rename('Number of Games' = NumberofGames) %>%
  bind_cols(recent_Bans2)

rm(recent_Bans2)

home_road_winpercent <- gameLogs_Two %>%
  filter(Type == 'Regular Season') %>%
  group_by(Location, Outcome) %>%
  distinct(GameID) %>%
  filter(Location == 'H') %>%
  summarise(n = n()) %>%
  mutate('Win Percentage' = round(n / sum(n) * 100, 1)) %>%
  rename(Wins = n) %>%
  mutate(Outcome = replace(Outcome, Outcome == 'L', 'Away'),
         Outcome = replace(Outcome, Outcome == 'W', 'Home')) %>%
  ungroup() %>%
  select(-Location) %>%
  rename(Location = Outcome) %>%
  pivot_wider(names_from = Location, values_from = c(Wins, 'Win Percentage')) %>%
  mutate(Home = 'Home',
         Away = 'Away',
         'Total Games' = sum(Wins_Away + Wins_Home)) %>%
  rename('Road Wins' = Wins_Away, 'Home Wins' = Wins_Home, 'Win Percentage Road' = 'Win Percentage_Away',
         'Win Percentage Home' = 'Win Percentage_Home')

rm(team_Wins_Yesterday)

win_streak <- gameLogs_Two %>%
  mutate(win = case_when(Outcome == 'W' ~ 1,
                         TRUE ~ 0)) %>%
  group_by(Team, GameID, win) %>%
  distinct(GameID) %>%
  ungroup() %>%
  group_by(Team) %>%
  mutate(streak = streak_run(win)) %>%
  filter(GameID == max(GameID)) %>%
  ungroup() %>%
  mutate(type = case_when(win == 0 ~ 'Losing',
                          TRUE ~ 'Winning'),
         winbb = case_when(win == 0 ~ 0,
                           TRUE ~ as.double(streak))) %>%
  select(Team, winbb) %>%
  rename(`Win Streak` = winbb)

team_wins <- gameLogs_Two %>%
  filter(Type == 'Regular Season') %>%
  group_by(Team, Outcome) %>%
  distinct(GameID) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = Outcome, values_from = n) %>%
  select(Team, W, L) %>%
  left_join(injury_data_count) %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  rename(Wins = W, Losses = L) %>%
  left_join(win_streak) %>%
  left_join(full_team_names) %>%
  mutate(WinPercentage = (Wins / (Wins + Losses)),
         projected_wins = WinPercentage * 72,
         projected_wins = round(projected_wins, 0),
         projected_losses = 72 - projected_wins) %>%
  arrange(FullName)

rm(injury_data_count, win_streak)

conferences <- dataBREFTeamJoined %>%
  select(nameConference, slugTeamBREF) %>%
  rename(Conference = nameConference, Team = slugTeamBREF) %>%
  mutate(Team = replace(Team, Team == 'BRK', 'BKN')) %>%
  mutate(Team = replace(Team, Team == 'PHO', 'PHX')) %>%
  mutate(Team = replace(Team, Team == 'CHO', 'CHA'))

rm(dataBREFTeamJoined)

last_10_wins <- gameLogs_Two %>%
  filter(Type == 'Regular Season') %>%
  select(Team, GameID, Date, Outcome) %>%
  distinct() %>%
  group_by(Team) %>%
  top_n(10, Date) %>%
  group_by(Team, Outcome) %>%
  count() %>%
  pivot_wider(names_from = Outcome,
              values_from = n) %>%
  ungroup() %>%
  mutate(L = replace_na(L, 0),
         W = replace_na(W, 0),
         `Last 10 Games` = paste0(W, '-', L)) %>%
  select(Team, `Last 10 Games`) %>%
  rename(Team1 = Team) %>%
  left_join(acronyms) %>%
  select(-Team1)

east_standings <- team_wins %>%
  left_join(conferences) %>%
  filter(Conference == 'Eastern') %>%
  select(-Conference, projected_wins, projected_losses) %>%
  mutate(Seed = min_rank(desc(WinPercentage))) %>%
  select(-WinPercentage) %>%
  select(Seed, FullName, Wins, Losses, `Win Streak`, 'Active Injuries') %>%
  rename(Team = FullName) %>%
  mutate(Seed = replace(Seed, Team == 'New York Knicks', 4),
         Seed = replace(Seed, Team == 'Atlanta Hawks', 5),
         Seed = replace(Seed, Team == 'Indiana Pacers', 9),
         Seed = replace(Seed, Team == 'Washington Wizards', 8)) %>%
  arrange(Seed) %>%
  left_join(last_10_wins)

west_standings <- team_wins %>%
  left_join(conferences) %>%
  filter(Conference == 'Western') %>%
  select(-Conference, projected_wins, projected_losses) %>%
  mutate(Seed = min_rank(desc(WinPercentage))) %>%
  select(-WinPercentage) %>%
  select(Seed, FullName, Wins, Losses, `Win Streak`, 'Active Injuries') %>%
  rename(Team = FullName) %>%
  mutate(Seed = replace(Seed, Team == 'Los Angeles Clippers', 4),
         Seed = replace(Seed, Team == 'Portland Trail Blazers', 6),
         Seed = replace(Seed, Team == 'Los Angeles Lakers', 7),
         Seed = replace(Seed, Team == 'Memphis Grizzlies', 8),
         Seed = replace(Seed, Team == 'Golden State Warriors', 9)) %>%
  arrange(Seed) %>%
  left_join(last_10_wins)

top_20pt_scorers <- gameLogs_Two %>%
  filter(Type == 'Regular Season',
         !(Player == 'James Harden' & Team == 'HOU')) %>%
  group_by(Player) %>%
  filter(mean(PTS) >= 20) %>%
  transmute(avg_PTS = mean(PTS), season_ts_percent, MVPCalc, Team, GP) %>%
  distinct() %>%
  ungroup() %>%
  arrange(desc(MVPCalc)) %>%
  mutate(Rank = row_number(),
         Top5 = case_when(Rank <= 5 ~ 'Top 5 MVP Candidate',
                          TRUE ~ 'Other')) %>%
  left_join(player_teams) %>%
  select(-Team) %>%
  rename(Team = new_team) %>%
  left_join(team_wins) %>%
  distinct()

top5 <- top_20pt_scorers %>%
  filter(Top5 == 'Top 5 MVP Candidate') %>%
  select(Player, Top5)


top_20pt_scorersp <- gameLogs_Two %>%
  filter(Type == 'Playoffs',
         !(Player == 'James Harden' & Team == 'HOU')) %>%
  group_by(Player) %>%
  filter(mean(PTS) >= 20) %>%
  transmute(avg_PTS = mean(PTS), season_ts_percent, MVPCalc, Team, GP_p) %>%
  distinct() %>%
  ungroup() %>%
  arrange(desc(MVPCalc)) %>%
  left_join(player_teams) %>%
  left_join(top5) %>%
  mutate(Top5 = replace_na(Top5, 'Other')) %>%
  select(-Team) %>%
  rename(Team = new_team) %>%
  left_join(team_wins) %>%
  distinct()

team_ppg_scorers <- gameLogs_Two %>%
  filter(Type == 'Regular Season') %>%
  group_by(Player) %>%
  transmute(avg_PTS = mean(PTS), season_ts_percent, MVPCalc, Team, GP) %>%
  mutate(season_ts_percent = round(season_ts_percent, 3)) %>%
  distinct() %>%
  ungroup() %>%
  arrange(desc(MVPCalc)) %>%
  left_join(top5) %>%
  mutate(Top5 = replace_na(Top5, 'Other')) %>%
  left_join(full_team_names) %>%
  left_join(team_wins) %>%
  select(Player:Losses)

rm(top5)


team_ratings_logo <- team_ratings %>%
  mutate(logo = case_when(Team == 'ATL' ~ 'logos/atl.png',
                          Team == 'BOS' ~ 'logos/bos.png',
                          Team == 'BKN' ~ 'logos/bkn1.png',
                          Team == 'CHA' ~ 'logos/cha.png',
                          Team == 'CHI' ~ 'logos/chi1.png',
                          Team == 'CLE' ~ 'logos/cle.png',
                          Team == 'DAL' ~ 'logos/dal1.png',
                          Team == 'DEN' ~ 'logos/den2.png',
                          Team == 'DET' ~ 'logos/det.png',
                          Team == 'GSW' ~ 'logos/gsw1.png',
                          Team == 'HOU' ~ 'logos/hou3.png',
                          Team == 'IND' ~ 'logos/ind1.png',
                          Team == 'LAC' ~ 'logos/lac1.png',
                          Team == 'LAL' ~ 'logos/lal2.png',
                          Team == 'MEM' ~ 'logos/mem1.png',
                          Team == 'MIA' ~ 'logos/mia1.png',
                          Team == 'MIL' ~ 'logos/mil1.png',
                          Team == 'MIN' ~ 'logos/min.png',
                          Team == 'NOP' ~ 'logos/nop.png',
                          Team == 'NYK' ~ 'logos/nyk.png',
                          Team == 'OKC' ~ 'logos/okc1.png',
                          Team == 'ORL' ~ 'logos/orl1.png',
                          Team == 'PHI' ~ 'logos/phi.png',
                          Team == 'PHX' ~ 'logos/phx1.png',
                          Team == 'POR' ~ 'logos/por1.png',
                          Team == 'SAC' ~ 'logos/sac.png',
                          Team == 'SAS' ~ 'logos/sas1.png',
                          Team == 'TOR' ~ 'logos/tor1.png',
                          Team == 'UTA' ~ 'logos/uta1.png',
                          Team == 'WAS' ~ 'logos/was.png'))


######### Data Manipulation Complete ########
# Graphs ----
top20_plot <- function(df){
  p <- df %>%
    filter(GP >= 5) %>%
    ggplot(aes(avg_PTS, season_ts_percent, fill = Top5)) +
    geom_point(size = 6, alpha = 0.7, pch = 21, color = 'black', aes(text = paste0(Player, '<br>',
                                                                                   Team, ' (', Wins, '-', Losses, ')', '<br>',
                                                                                   'PPG: ', round(avg_PTS, 1), '<br>',
                                                                                   'TS%: ', round(season_ts_percent * 100, 1), '%',
                                                                                   '<br>',
                                                                                   'Games Played: ', GP))) +
    scale_y_continuous(labels = scales::percent) + 
    geom_hline(aes(yintercept = league_average_ts), alpha = 0.8, linetype = "dashed") +
    annotate(geom = 'text', label = 'League Average TS%', x = max(df$avg_PTS) * .95, y = league_average_ts * .99) +
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
# top20_plot(top_20pt_scorers)

top20_plot_playoffs <- function(df){
  p <- df %>%
    ggplot(aes(avg_PTS, season_ts_percent, fill = Top5)) +
    geom_point(size = 6, alpha = 0.7, pch = 21, color = 'black', aes(text = paste0(Player, ' (', Team, ')', '<br>',
                                                                                   'Playoff PPG: ', round(avg_PTS, 1), '<br>',
                                                                                   'Playoff TS%: ', round(season_ts_percent * 100, 1),
                                                                                   '%', '<br>',
                                                                                   'Playoff Games Played: ', GP_p))) +
    scale_y_continuous(labels = scales::percent) + 
    geom_hline(aes(yintercept = league_average_ts), alpha = 0.8, linetype = "dashed") +
    annotate(geom = 'text', label = 'League Average TS%', x = max(df$avg_PTS) * .95, y = league_average_ts * .98) +
    scale_fill_manual(values = c('light blue', 'orange')) +
    labs(title = 'NBA Playoffs Scoring Efficiency Tracker \n PPG vs TS% for all 20+ PPG Scorers',
         x = 'Average Points per Game',
         y = 'True Shooting Percentage',
         fill = NULL) +
    theme(legend.background = element_rect(color = "black"))
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(hoverlabel = list(bgcolor = "white"),
           legend = list(x = .78, y = 0.85))
  
}
# top20_plot_playoffs(top_20pt_scorersp)

team_ppg_plot <- function(df){
  p <- df %>%
    filter(GP >= 5) %>%
    ggplot(aes(avg_PTS, season_ts_percent, fill = Top5)) +
    geom_point(size = 6, alpha = 0.7, pch = 21, color = 'black', aes(text = paste0(Player, '<br>',
                                                                                   Team, ' (', Wins, '-', Losses, ')', '<br>',
                                                                                   'PPG: ', round(avg_PTS, 1), '<br>',
                                                                                   'TS%: ', round(season_ts_percent * 100, 1), '%',
                                                                                   '<br>',
                                                                                   'Games Played: ', GP))) +
    scale_y_continuous(labels = scales::percent) +
    geom_hline(aes(yintercept = league_average_ts), alpha = 0.8, linetype = "dashed") +
    annotate(geom = 'text', label = 'League Average TS%', x = max(df$avg_PTS) * .92, y = league_average_ts * .98) +
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

team_ratings_plot <- function(df){
  geom_logos <- function(mapping=NULL, data=NULL, inherit.aes=TRUE,
                         na.rm=FALSE, by="width", ...) {
    geom_image(mapping, data, inherit.aes=inherit.aes, na.rm=na.rm, ..., geom = 'logos', size = .13)
  }
  
  annotations <- data.frame(
    xpos = c(-Inf,-Inf, -Inf, -Inf, Inf, Inf, Inf, Inf),
    ypos =  c(min(df$DRTG), max(df$DRTG), -Inf, Inf, min(df$DRTG), max(df$DRTG), -Inf, Inf),
    annotateText = c("+ Defense","- Offense ", "-  Offense ", "- Defense",
                     "+ Defense", "+ Offense", "+ Offense ", "- Defense"),
    hjustvar = c(0, 0, 0, 0, 1, 1, 1, 1) ,
    vjustvar = c(1,0, 1, 0, 1, 0, 1,0),
    fillbby = c('#2ECC71', '#CD6155', '#CD6155', '#CD6155', '#2ECC71', '#2ECC71', '#2ECC71', '#CD6155')) #<- adjust
  
  df %>%
    ggplot(aes(ORTG, DRTG)) +
    geom_vline(xintercept = mean(df$DRTG), linetype = "dashed") +
    geom_hline(yintercept = mean(df$ORTG), linetype = "dashed") +
    geom_logos(aes(image = logo)) +
    geom_label(data = annotations, aes(x = xpos, y = ypos, hjust = hjustvar, vjust = vjustvar, label = annotateText, size = 4.5),
               fill = annotations$fillbby) +
    scale_y_reverse() +
    labs(title = 'Offensive vs Defensive Ratings',
         x = 'Offensive Rating',
         y = 'Defensive Rating') +
    theme(legend.position = 'none',
          plot.title = element_text(face = "bold", size = rel(1.35), hjust = 0.5))
  
}

team_choices <- unique(team_wins$FullName)

opp_pts <- gameLogs_Two %>%
  filter(Type == 'Regular Season') %>%
  group_by(Team, GameID, Date) %>%
  summarise(tot_pts_opp = sum(PTS)) %>%
  rename(Opponent = Team)
  
team_mov <- gameLogs_Two %>%
  filter(Type == 'Regular Season') %>%
  group_by(Team, GameID, Date) %>%
  summarise(tot_pts = sum(PTS)) %>%
  left_join(full_team_names) %>%
  left_join(opp_pts) %>%
  filter(Team != Opponent) %>%
  mutate(mov = tot_pts - tot_pts_opp,
         outcome = case_when(mov > 0 ~ 'W',
                             TRUE ~ 'L'),
         outcome_wins = case_when(mov > 0 ~ 1,
                                 TRUE ~ 0),
         outcome_loss = case_when(mov < 0 ~ 1, 
                                  TRUE ~ 0)) %>%
  ungroup() %>%
  group_by(Team) %>%
  mutate(standing_w = cumsum(outcome_wins),
         standing_l = cumsum(outcome_loss),
         standings = paste0(standing_w, '-', standing_l)) %>%
  select(Team:outcome, standings) %>%
  ungroup()

rm(opp_pts)

mov_plot <- function(df){
  cols <- c('W' = 'dark green', 'L' = 'red')
  p <- df %>%
    ggplot(aes(Date, mov)) +
    geom_col(alpha = 0.7, aes(fill = outcome, text = paste0(Date, '<br>',
                                                           outcome, ' vs ', Opponent, '<br>',
                                                           'Scoreline: ', tot_pts, ' - ', tot_pts_opp, '<br>',
                                                           'Margin of Victory: ', mov, '<br>',
                                                           'Record: ', standings))) +
    scale_y_continuous(breaks = c(-50, -45, -40, -35, -30, -25, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    scale_fill_manual(values = cols) +
    labs(x = NULL,
         y = 'Margin of Victory',
         title = paste0(df$FullName[1], ' Game Log History \n 2020-2021 NBA Season'),
         fill = 'Outcome')
  
  
  ggplotly(p, tooltip = c('text')) %>%
   layout(hoverlabel = list(bgcolor = "white"))
  
}


team_rank <- team_wins %>%
  arrange(desc(WinPercentage)) %>%
  mutate(Rank = row_number()) %>%
  select(FullName, Rank) %>%
  rename(Team = FullName) %>%
  mutate(Team = replace(Team, Team == 'LA Clippers', 'Los Angeles Clippers'))

opponent_rank <- team_wins %>%
  arrange(desc(WinPercentage)) %>%
  mutate(opp_Rank = row_number()) %>%
  select(FullName, opp_Rank) %>%
  rename(Team = FullName) %>%
  mutate(Team = replace(Team, Team == 'LA Clippers', 'Los Angeles Clippers')) %>%
  rename(Opponent = Team)


get_schedule_clean <- function(df){
    if (nrow(odds1) > 0) {
      schedule_main <- df %>%
        select(date_game, game_start_time, visitor_team_name, visitor_pts, home_team_name) %>%
        rename(Date = date_game, `Start Time (EST)` = game_start_time, `Team 1` = visitor_team_name, Vs = visitor_pts,
               `Team 2` = home_team_name) %>%
        mutate(Vs = replace_na(Vs, 'Vs'),
               Date = as.Date(Date, format = '%a, %b %d, %Y'),
               `Day of Week` = wday(Date, label = TRUE, abbr = FALSE),
               `Start Time (EST)` = str_replace_all(`Start Time (EST)`, 'p', ' PM'),
               bb = substr(`Start Time (EST)`, 0, 4),
               b = parse_date_time(`Start Time (EST)`, '%I:%M %p')) %>%
        filter(Date >= todayDate) %>%
        arrange(Date, b) %>%
        select(Date, `Day of Week`, `Start Time (EST)`, `Team 1`, Vs, `Team 2`) %>%
        left_join(team_rank, by = c(`Team 1` = 'Team')) %>%
        left_join(opponent_rank, by = c(`Team 2` = 'Opponent')) %>%
        mutate('Average Rank of Teams' = (Rank + opp_Rank) / 2) %>%
        select(-Rank, -opp_Rank) %>%
        left_join(odds1) %>%
        left_join(odds2) %>%
        mutate(team1 = case_when(Date == todayDate ~ new_team1,
                                 TRUE ~ `Team 1`),
               team2 = case_when(Date == todayDate ~ new_team2,
                                 TRUE ~ `Team 2`)) %>%
        select(Date, `Day of Week`, `Start Time (EST)`, team1, `Vs`, team2, `Average Rank of Teams`) %>%
        rename(`Road Team` = team1, `Home Team` = team2) %>%
        distinct()
      
      return(schedule_main)
    }
    else {
      schedule_main <- df %>%
        select(date_game, game_start_time, visitor_team_name, visitor_pts, home_team_name) %>%
        rename(Date = date_game, `Start Time (EST)` = game_start_time, `Team 1` = visitor_team_name, Vs = visitor_pts,
               `Team 2` = home_team_name) %>%
        mutate(Vs = replace_na(Vs, 'Vs'),
               Date = as.Date(Date, format = '%a, %b %d, %Y'),
               `Day of Week` = wday(Date, label = TRUE, abbr = FALSE),
               `Start Time (EST)` = str_replace_all(`Start Time (EST)`, 'p', ' PM'),
               bb = substr(`Start Time (EST)`, 0, 4),
               b = parse_date_time(`Start Time (EST)`, '%I:%M %p')) %>%
        filter(Date >= todayDate) %>%
        arrange(Date, b) %>%
        select(Date, `Day of Week`, `Start Time (EST)`, `Team 1`, Vs, `Team 2`) %>%
        left_join(team_rank, by = c(`Team 1` = 'Team')) %>%
        left_join(opponent_rank, by = c(`Team 2` = 'Opponent')) %>%
        mutate('Average Rank of Teams' = (Rank + opp_Rank) / 2) %>%
        select(-Rank, -opp_Rank)
      
      return(schedule_main)
    }
}
  

odds1 <- odds_df %>%
  filter(date == todayDate) %>%
  select(team:moneyline) %>%
  mutate(new_moneyline = case_when(moneyline > 0 ~ paste0('+', as.character(moneyline)),
                                   TRUE ~ as.character(moneyline)),
         new_team1 = paste0(team, ' (', new_moneyline, ')')) %>%
  select(`Team 1` = team, new_team1)


odds2 <- odds_df %>%
  filter(date == todayDate) %>%
  select(team:moneyline) %>%
  mutate(new_moneyline = case_when(moneyline > 0 ~ paste0('+', as.character(moneyline)),
                                   TRUE ~ as.character(moneyline)),
         new_team2 = paste0(team, ' (', new_moneyline, ')')) %>%
  select(`Team 2` = team, new_team2)

schedule_main <- get_schedule_clean(schedule)

rm(odds1, odds2, team_rank, opponent_rank, schedule)

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
    valueBox(value = paste0(df$`Wins`, '-', df$`Losses`), paste0("Win / Loss Record ",
                                                                 "(" , df$new_seed, ")"), icon = icon("list"), color = "purple")
  }
  else {
    valueBox(value = paste0('No Data Available'), "Team Hasn't Played Yet", icon = icon("list"), color = "purple")
  }
}

last_season_valuebox_function <- function(df){
  if (nrow(df) > 0){
    valueBox(value = paste0(df$`Wins`, '-', df$`Losses`), "Last Season's Win / Loss Record", icon = icon("list"), color = "purple")
  }
  else {
    valueBox(value = paste0('No Data Available'), "Team Hasn't Played Yet", icon = icon("list"), color = "purple")
  }
}

game_types <- team_mov %>%
  filter(mov >= 1) %>%
  mutate(clutch_game = case_when(mov<= 5 ~ 'Clutch Game',
                                 mov >= 5 & mov <= 10 ~ '10 Pt Game',
                                 mov > 10 ~ 'Blowout Game',
                                 TRUE ~ 'Help'),
         season_date = case_when(Date <= '2020-07-29' ~ 'Pre-Bubble',
                                 Date > '2020-07-29' ~ 'Bubble',
                                 TRUE ~ 'Help')) %>%
  select(GameID, Date, clutch_game, season_date, mov) %>%
  group_by(clutch_game) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(pct_total = n / sum(n)) %>%
  ungroup() %>%
  mutate(explanation = case_when(clutch_game == 'Blowout Game' ~ 'more than 10 points',
                                 clutch_game == 'Clutch Game' ~ '5 points or less',
                                 TRUE ~ 'between 6 - 10 points'))

game_types_plot <- function(df){
  p <- df %>%
    ggplot(aes(clutch_game, pct_total)) +
    geom_col(position = 'dodge', aes(text = paste0(clutch_game, 's account for ', round(pct_total * 100, 1), '% of all',
                                                   ' games played.', '<br>', 'Number of Observations: ', n, '<br>', '<br>',
                                                   clutch_game, 's are defined as games that were decided by ', explanation))) +
    scale_y_continuous(labels = percent_format()) +
    labs(x = 'Game Type',
         y = 'Percent of Total Games',
         fill = NULL,
         title = 'Game Type Distribution for 2020-21 NBA Season') +
    theme_jacob() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = 'top')
  
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(legend = list(orientation = "h", x = 0.35, y = 1.03),
           hoverlabel = list(bgcolor = "white"))
}

contract_df <- gameLogs_Two %>%
  filter(Type == 'Regular Season') %>%
  group_by(Player) %>%
  filter(Date == max(Date)) %>%
  select(Player, Team, MVPCalc, new_MVPCalc_adj, Salary, GP, FullName, games_missed, team_gp) %>%
  distinct() %>%
  mutate(salary_rank = case_when(Salary >= 30000000 ~ "$30+ M",
                                 Salary >= 25000000 & Salary < 30000000 ~ "$25-30 M",
                                 Salary >= 20000000 & Salary < 25000000 ~ "$20-25 M",
                                 Salary >= 15000000 & Salary < 20000000 ~ "$15-20 M",
                                 Salary >= 10000000 & Salary < 15000000 ~ "$10-15 M",
                                 Salary >= 5000000  & Salary < 10000000 ~ "$5-10 M",
                                 TRUE ~ "< $5 M")) %>%
  group_by(salary_rank) %>%
  mutate(rankingish = round(percent_rank(MVPCalc), 3),
         pvm_rank = round(mean(MVPCalc), 4),
         rankish_text = rankingish * 100) %>%
  ungroup() %>%
  mutate(total = n()) %>%
  group_by(salary_rank) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  mutate(pct_total = count / total,
         pct_total = round(pct_total, 3),
         salary_rank = fct_reorder(salary_rank, count),
         color_var = case_when(rankish_text >= 60 & Salary >= 25000000 ~ 'Superstars',
                               rankish_text >= 90 ~ 'Great Value',
                               rankish_text < 90 & rankish_text >= 20 ~ 'Normal',
                               TRUE ~ 'Bad Value')) %>%
  left_join(player_teams) %>%
  select(-Team) %>%
  rename(Team = new_team) %>%
  distinct()

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
         title = 'Distribution of Contracts in the 2020-21 NBA Season') +
    theme_minimal(base_size=10, base_family="Gill Sans MT") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite')
    )
}

value_plot <- function(df){
  p <- df %>%
    ggplot(aes(as.numeric(Salary), MVPCalc, fill = color_var)) +
    geom_point(aes(text = paste0(Player, '<br>',
                                 FullName, '<br>',
                                 'Salary: $', formatC(Salary, format = 'f', big.mark = ",", digits = 0), '<br>',
                                 'Player Value Metric: ', MVPCalc, '<br>',
                                 'Games Played: ', GP, '<br>',
                                 'Games Missed: ', games_missed, '<br>',
                                 Player, ' is in the top ', rankish_text, '% Percentile for all players in this Salary Group (',
                                 salary_rank, ')')),
               size = 2.5, shape = 21, alpha = 0.7) +
    scale_x_continuous(labels = label_dollar()) +
    scale_y_continuous(limits = c(-5, 55, breaks = c(-5, 5, 15, 25, 35, 45))) +
    labs(y = 'Player Value Metric',
         x = 'Salary',
         title = 'What are the least & most valuable contracts in the 2020-21 NBA Season ?',
         fill = 'Legend') +
    scale_fill_manual(values=c("red", "green", "grey70", 'purple'))
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(hoverlabel = list(bgcolor = "white"))
}

league_avg_ppg <- gameLogs_Two %>%
  filter(Type == 'Regular Season') %>%
  group_by(Team, GameID, Date) %>%
  summarise(total_pts = sum(PTS)) %>%
  ungroup() %>%
  group_by(Team) %>%
  summarise(avg_pts = mean(total_pts)) %>% # this is cutoff for avg ppg by team 
  ungroup() %>%
  summarise(league_avg = mean(avg_pts)) %>%
  mutate(league_avg = round(league_avg, 1),
         last_yr_ppg = 111.8,
         pct_change = (league_avg / last_yr_ppg) * 100 - 100,
         pct_change = round(pct_change, 2))

upcoming_games_function <- function(df){
  if (nrow(df) == 0) {
    df1 <- todayDate %>%
      as.data.frame() %>%
      mutate(Date = todayDate,
             `Day of Week` = wday(todayDate, label = TRUE),
             gp = 0) %>%
      select(Date:gp)
    return(df1)
  }
  else {
    df2 <- df %>%
    filter(Date == min(Date)) %>%
      group_by(Date, `Day of Week`) %>%
      count() %>%
      rename(gp = n) %>%
      ungroup()
    return(df2)
  }
}

upcoming_games <- upcoming_games_function(schedule_main)


main_bans <- upcoming_games %>%
  cbind(home_road_winpercent) %>% 
  cbind(league_avg_ppg) %>%
  mutate(full_date = format(Date, '%B %d, %Y'),
         record = paste0(`Home Wins`, " - ", `Road Wins`)) 

rm(league_avg_ppg, home_road_winpercent)

gp_valuebox_function <- function(df){
  if (df$gp == 1){
    valueBox(
      value = df$full_date, HTML(paste0("Next Gameday Date <br> <br> ", df$gp,
                                             " Upcoming Game")),
      icon = icon("calendar"), color = "blue"
    )
  }
  else {
    valueBox(
      value = df$full_date, HTML(paste0("Next Gameday Date <br> <br> ", df$gp,
                                             " Upcoming Games | See Schedule")),
      icon = icon("calendar"), color = "blue"
    )
  }
}

contract_value <- contracts %>%
  left_join(GP) %>%
  mutate(GP = replace_na(GP, 0)) %>%
  group_by(Team) %>%
  mutate(team_gp = max(GP),
         team_max_value = (sum(Salary) * team_gp),
         team_achieved_value = (sum(Salary * GP))) %>%
  ungroup() %>%
  mutate(team_missing_value = team_max_value - team_achieved_value,
         team_pct_achieved = team_achieved_value / team_max_value,
         team_pct_missing = team_missing_value / team_max_value,
         team_pct_achieved = round(team_pct_achieved, 3),
         team_pct_missing = round(team_pct_missing, 3),
         player_achieved_value = GP * Salary,
         player_missing_value = ((team_gp * Salary) - player_achieved_value),
         player_pct_missing = player_missing_value / (team_gp * Salary),
         player_pct_missing = round(player_pct_missing, 3))

team_contract_value <- contract_value %>%
  select(Team, team_gp:team_pct_missing) %>%
  distinct() %>%
  left_join(team_wins) %>%
  mutate(Team = fct_reorder(Team, team_pct_missing))

rm(contract_value)

team_contract_value_plot <- function(df){
  p <- df %>%
    ggplot(aes(team_pct_missing, Team, fill = WinPercentage, text = paste0(FullName, '<br>',
                                                                          'Record: ', Wins, '-', Losses, '<br>',
                                                                          'Total Contract Value Missing: ', (team_pct_missing * 100),
                                                                          '%',
                                                                           '<br>',
                                                                          'Win Percentage: ', round(WinPercentage, 2)))) +
    geom_col() +
    geom_vline(aes(xintercept = mean(team_pct_missing), alpha = 0.5)) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_gradient(low = 'red', high = 'green') +
    annotate('text', x = mean(df$team_pct_missing) + 0.07, y = 3,
             label = paste0(round(mean(df$team_pct_missing * 100), 2), '% (League Average)')) +
    labs(x = '% of Total Contract Value Missing',
         y = NULL,
         title = 'Which Teams are missing the most Contract Value this Season from Injuries, COVID-related Absences, or DNPs?',
         fill = 'Legend')
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(legend = list(orientation = "h", x = 0.35),
           hoverlabel = list(bgcolor = "white"))
}

rating_bans <- team_ratings %>%
  left_join(full_team_names) %>%
  arrange(desc(NRTG)) %>%
  mutate(NRTG_rank = row_number()) %>%
  arrange(desc(ORTG)) %>%
  mutate(ORTG_rank = row_number()) %>%
  arrange(DRTG) %>%
  mutate(DRTG_rank = row_number(),
         NRTG_real_text = get_ord_numbers(NRTG_rank),
         DRTG_real_text = get_ord_numbers(DRTG_rank),
         ORTG_real_text = get_ord_numbers(ORTG_rank),
         rating_text = paste0('Offensive Rating: ', ORTG, ' (', ORTG_real_text, ')', '<br>',
                              'Defensive Rating: ', DRTG, ' (', DRTG_real_text, ')', '<br>',
                              'Net Rating: ', NRTG, ' (', NRTG_real_text, ')'))
  

rm(team_ratings, contracts, GP)

over_under <- over_under %>%
  left_join(team_wins, by = c("Team" = "FullName")) %>%
  mutate(new_expected_winpct = round((over_under_wins / 72), 3),
         over = case_when(WinPercentage >= new_expected_winpct ~ 'Over',
                          TRUE ~ 'Under'),
         wins_difference = projected_wins - over_under_wins,
         Team.y = fct_reorder(Team.y, wins_difference),
         wins_odds_pre = 72 - over_under_wins) %>%
  distinct()

vegas_plot <- function(df) {
  p <- df %>%
    ggplot(aes(wins_difference, Team.y, fill = over)) +
    geom_col(aes(text = paste0(Team, '<br>',
                               'Current W/L Projection: ', projected_wins, '-', projected_losses, '<br>',
                               "Vegas' Preseason Over/Under for Wins: ", over_under_wins, '<br>',
                               'Wins Differential: ', wins_difference))) +
    scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
    annotate(geom = "text", label = "<span style='color: #00BFC4;'>  Exceeded Expectations</span>",
             x = min(df$wins_difference) * .45, y = 24.5, size = 4.5) +
    annotate(geom = "text", label = "<span style='color: #F8766D;'> Underperforming</span>",
             x = max(df$wins_difference) * .30, y = 5.5, size = 4.5) +
    labs(x = 'Predicted Wins Differential',
         y = NULL,
         title = paste0('Vegas Preseason Over / Under Odds Tracker for the 2020-21 NBA Season')) +
    theme_jacob() +
    theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(hoverlabel = list(bgcolor = "white"))
}

opponent_shooting_bans <- opponent_shooting %>%
  select(nameTeam, pctFGOpponent, pctFG3Opponent, ftaOpponent, pctFGOpponentRank, pctFG3OpponentRank, ftaOpponentRank) %>%
  mutate(pctFG_real_text = get_ord_numbers(pctFGOpponentRank),
         pct3FG_real_text = get_ord_numbers(pctFG3OpponentRank),
         FTA_real_text = get_ord_numbers(ftaOpponentRank),
         rating_text = paste0('Opponent FG%: ', pctFGOpponent * 100, '%', ' (', pctFG_real_text, ')', '<br>',
                              'Opponent 3P%: ', pctFG3Opponent * 100, '%', ' (', pct3FG_real_text, ')', '<br>',
                              'Opponent FTA/g: ', ftaOpponent, ' (', FTA_real_text, ')')) %>%
  rename(FullName = nameTeam) %>%
  mutate(FullName = replace(FullName, FullName == 'LA Clippers', 'Los Angeles Clippers'))

west_seed <- west_standings %>%
  select(Seed, Team) %>%
  mutate(conference = 'West',
         suffix_seed = get_ord_numbers(Seed),
         new_seed = paste0(suffix_seed, " in West")) %>%
  select(Team, new_seed)

east_seed <- east_standings %>%
  select(Seed, Team) %>%
  mutate(conference = 'East',
         suffix_seed = get_ord_numbers(Seed),
         new_seed = paste0(suffix_seed, " in East")) %>%
  select(Team, new_seed) %>%
  rbind(west_seed) %>%
  rename(FullName = Team)

team_wins <- team_wins %>%
  left_join(east_seed)

rm(west_seed, east_seed)

opp_winpercent <- gameLogs_Two %>%
  filter(Type == 'Regular Season') %>%
  select(Team, Date, Outcome, Opponent) %>%
  distinct() %>%
  left_join(team_wins %>% select(Team, WinPercentage)) %>%
  select(Team, WinPercentage) %>%
  rename(Opponent = Team, opp_WinPercentage = WinPercentage) %>%
  distinct()

team_opponents <- gameLogs_Two %>%
  filter(Type == 'Regular Season') %>%
  select(Team, Date, Outcome, Location, Opponent) %>%
  distinct() %>%
  left_join(team_wins %>% select(Team, WinPercentage)) %>%
  left_join(opp_winpercent) %>%
  rename(team_WinPercentage = WinPercentage) %>%
  group_by(Team) %>%
  mutate(avg_opp_winpercent = round(mean(opp_WinPercentage), 3),
         team_WinPercentage = round(team_WinPercentage, 3),
         opp_WinPercentage = round(opp_WinPercentage, 3))

homeroad_standings <- team_opponents %>%
  select(Team, Location, Outcome) %>%
  group_by(Team, Location, Outcome) %>%
  count() %>%
  pivot_wider(names_from = c(Location, Outcome),
              values_from = n) %>%
  ungroup() %>%
  replace(is.na(.), 0) %>%
  mutate(home_record = paste0(H_W, '-', H_L),
         road_record = paste0(A_W, '-', A_L))

advanced_standings <- team_opponents %>%
  mutate(divider = case_when(opp_WinPercentage >= 0.50 ~ 'Above .500',
                             TRUE ~ 'Below .500')) %>%
  group_by(Team, Outcome, divider) %>%
  count() %>%
  pivot_wider(names_from = c(divider, Outcome),
              values_from = n) %>%
  ungroup() %>%
  mutate(`Above .500_L` = replace_na(`Above .500_L`, 0),
         `Below .500_W` = replace_na(`Below .500_W`, 0),
         `Above .500_W` = replace_na(`Above .500_W`, 0),
         `Below .500_L` = replace_na(`Below .500_L`, 0),
         `Below .500 Record` = paste0(`Below .500_W`, '-', `Below .500_L`),
         `Above .500 Record` = paste0(`Above .500_W`, '-', `Above .500_L`),
         prop_gp_above500 = round((`Above .500_W` + `Above .500_L`) / (`Above .500_L` + `Below .500_W` +
                                                                         `Below .500_L` + `Above .500_W`), 3),
         prop_gp_below500 = round((`Below .500_W` + `Below .500_L`) / (`Above .500_L` + `Below .500_W` +
                                                                         `Below .500_L` + `Above .500_W`), 3)) %>%
  left_join(homeroad_standings %>% select(Team, home_record, road_record)) %>%
  select(-`Above .500_L`, -`Below .500_W`, -`Below .500_L`, -`Above .500_W`) %>%
  left_join(team_opponents %>% select(Team, avg_opp_winpercent)) %>%
  distinct() %>%
  left_join(team_wins %>% select(Team, WinPercentage)) %>%
  mutate(Team = fct_reorder(Team, prop_gp_below500))

rm(homeroad_standings, team_opponents, opp_winpercent)

advanced_sos_plot <- function(df){
  p <- df %>%
    ggplot(aes(prop_gp_below500, Team, fill = avg_opp_winpercent >= 0.5)) +
    geom_col(aes(text = paste0(Team, '<br>',
                               'Team Win %: ', round(WinPercentage * 100, 1), '%', '<br>',
                               'Average Opponent Win %: ', avg_opp_winpercent * 100, '%', '<br>', 
                               '<br>',
                               'Record vs Below .500 Teams: ', `Below .500 Record`, '<br>',
                               'Record vs Above .500 Teams: ', `Above .500 Record`, '<br>',
                               '<br>',
                               'Home Record: ', home_record, '<br>',
                               'Road Record: ', road_record, '<br>',
                               '<br>',
                               round(prop_gp_below500 * 100, 1), '% of games played have been vs Below .500 Teams', '<br>',
                               round(prop_gp_above500 * 100, 1), '% of games played have been vs Above .500 Teams'))) +
    scale_fill_manual(values = c('#00BFC4', '#F8766D')) +
    scale_x_continuous(labels = percent_format()) +
    annotate(geom = "text", label = "<span style='color: #F8766D;'>  Faced Harder \n Competition</span>",
             x = max(df$prop_gp_below500) * .95, y = 4) +
    annotate(geom = "text", label = "<span style='color: #00BFC4;'> Faced Easier \n Competition</span>",
             x = max(df$prop_gp_below500) * .995, y = 16) +
    labs(x = '% of Games vs Below .500 Teams',
         y = NULL,
         title = paste0('Team Strength of Schedule for the 2020-21 NBA Season')) +
    theme_jacob() +
    theme(legend.position = 'none')
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(hoverlabel = list(bgcolor = "white"))
}

team_opp_ppg <- gameLogs_Two %>%
  select(Team, GameID, Date, PTS, threePFGMade, threePAttempted) %>%
  group_by(Team, GameID, Date) %>%
  summarise(opp_pts = sum(PTS), opp_tot_threes_made = sum(threePFGMade), opp_tot_threes_attempted = sum(threePAttempted)) %>%
  mutate(opp_threepct = round(opp_tot_threes_made / opp_tot_threes_attempted, 3)) %>%
  rename(Opp = Team)

team_png <- read_csv('data/team_png.csv')

opp_png <- team_png %>%
  select(Opponent = Team, opp_logo = logo)

team_avg_ppg <- gameLogs_Two %>%
  select(Team, GameID, Date, PTS, threePFGMade, threePAttempted, Location) %>%
  group_by(Team, GameID, Date) %>%
  summarise(pts = sum(PTS), tot_threes_made = sum(threePFGMade), tot_threes_attempted = sum(threePAttempted)) %>%
  left_join(gameLogs_Two %>% select(Team, GameID, Location)) %>%
  distinct() %>%
  mutate(threepct = round(tot_threes_made / tot_threes_attempted, 3)) %>%
  left_join(team_opp_ppg) %>%
  filter(Team != Opp) %>%
  ungroup() %>%
  group_by(Team) %>%
  mutate(avg_ppg = round(mean(pts), 1),
         avg_opp_ppg = round(mean(opp_pts), 1),
         pts_difference = pts - avg_ppg,
         opp_pts_difference = opp_pts - avg_opp_ppg,
         Outcome = case_when(pts > opp_pts ~ 'W',
                             TRUE ~ 'L'),
         max_season_pts = max(pts),
         max_opp_season_pts = max(opp_pts)) %>%
  ungroup() %>%
  filter(Date == max(Date)) %>%
  arrange(desc(GameID)) %>%
  mutate(pts_color = case_when(pts == max_season_pts ~ 1, # best all season
                               pts != max_season_pts & pts_difference >= 10 ~ 2, # pretty good # very bad
                               pts_difference <= -10 ~ 3,
                               TRUE ~ 0),
         opp_pts_color = case_when(opp_pts_difference <= -10 ~ 2, # pretty bad
                                   opp_pts_difference >= 10 ~ 3, # very good
                                   TRUE ~ 0)) %>%
  select(Team, Outcome, pts, Opp, opp_pts, opp_pts_color, pts_color, Location) %>%
  rename(PTS = pts, Opponent = Opp, `Opponent PTS` = opp_pts) %>%
  left_join(team_png %>% select(Team, logo)) %>%
  left_join(opp_png) %>%
  mutate(mov = PTS - `Opponent PTS`,
         new_loc = case_when(Location == 'H' ~ 'Vs.',
                             TRUE ~ '@')) %>%
  arrange(desc(mov)) %>%
  select(logo, Team, Outcome, PTS, new_loc, opp_logo, opp_pts_color, everything(), -Location) %>%
  filter(Outcome == 'W')

rm(team_opp_ppg, team_png, opp_png)

most_recent_date <- gameLogs_Two %>%
  select(Date) %>%
  filter(Date == max(Date)) %>%
  distinct()
  

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
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left",
      ...
    ) 
}
team_gt_table <- function(df){
  df %>%
    gt() %>%
    gt_theme_538() %>%
    text_transform(locations = cells_body(c(logo)),
                   fn = function(x) {
                     web_image(url = x, height = 45)
                   }) %>%
    text_transform(locations = cells_body(c(opp_logo)),
                   fn = function(x) {
                     web_image(url = x, height = 45)
                   }) %>%
    cols_hide(columns = c(pts_color, opp_pts_color)) %>%
    cols_label(logo = "", opp_logo = "", new_loc = "", `Opponent PTS` = 'OPP. PTS', mov = 'MARGIN OF VICTORY') %>%
    tab_style(
      style = cell_fill(color = "#9362DA"),
      locations = cells_body(
        columns = c(PTS),
        rows = pts_color == 1)) %>%
    tab_style(
      style = cell_fill(color = "#3fb7d9"),
      locations = cells_body(
        columns = c(PTS),
        rows = pts_color == 2)) %>%
    tab_style(
      style = cell_fill(color = "#B9564A"),
      locations = cells_body(
        columns = c(PTS),
        rows = pts_color == 3)) %>%
    tab_style(
      style = cell_fill(color = "#B9564A"),
      locations = cells_body(
        columns = c(`Opponent PTS`),
        rows = opp_pts_color == 2)) %>%
    tab_style(
      style = cell_fill(color = "#3fb7d9"),
      locations = cells_body(
        columns = c(`Opponent PTS`),
        rows = opp_pts_color == 3)) %>%
    tab_header(
      title = md("Team Stats"),
      subtitle = paste0("From ", recent_Bans$`Number of Games`, " Games Played on ", format(most_recent_date$Date, "%A, %B %d"))) %>%
    opt_align_table_header(align = "center") %>%
    cols_align(
      align = "center",
      columns = everything()
    )
}
# team_gt_table(team_avg_ppg)

player_gt_table <- function(df){
  df %>%
    gt() %>%
    gt_theme_538() %>%
    text_transform(
      locations = cells_body(c(logo)),
      fn = function(x){
        web_image(url = x, height = 34)
      }
    ) %>%
    cols_hide(columns = c(pts_color, ts_color)) %>%
    cols_label(logo = "") %>%
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
        columns = c(PTS),
        rows = pts_color == 1 # color rdef IF number > 80
      )
    ) %>%
    tab_style(
      style = cell_fill(color = "#3fb7d9"),
      locations = cells_body(
        columns = c(PTS),
        rows = pts_color == 2 # color rdef IF number > 80
      )
    ) %>%
    tab_style(
      style = cell_fill(color = "#B9564A"),
      locations = cells_body(
        columns = c(PTS),
        rows = pts_color == 3 # color rdef IF number > 80
      )
    ) %>%
    data_color(
      columns = c(`TS%`),
      colors = scales::col_numeric(
        palette = c("white", "#19e661"),
        domain = NULL)) %>%
    tab_header(
      title = md("Top Performers by **PTS** Scored"),
      subtitle = paste0("From ", recent_Bans$`Number of Games`, " Games Played on ", format(most_recent_date$Date, "%A, %B %d"))) %>%
    opt_align_table_header(align = "center") %>%
    cols_align(
      align = "center",
      columns = everything()
    )
  
}
# player_gt_table(top_15_yesterday)

rm(acronyms, conferences, full_team_names, gameLogs_Two, last_10_wins, opponent_shooting, player_teams,
   upcoming_games, yesterday, todayDate)

# play by play stuff
team_colors <- read_csv('data/team_colors.csv')

gameLog_gameids_yesterday_text <- gameLogs_yesterday %>%
  group_by(GameID) %>%
  select(GameID, FullName, Location) %>%
  distinct() %>%
  pivot_wider(names_from = Location, values_from = FullName) %>%
  mutate(text = paste0(H, ' Vs. ', A)) %>%
  select(GameID, text)

pbp_event_df <- pbp_data %>%
  pivot_longer(cols = descriptionPlayHome:descriptionPlayVisitor,
               names_to = 'location',
               values_to = 'play') %>%
  group_by(idGame) %>%
  mutate(event_play = row_number()) %>%
  ungroup() %>%
  select(idGame, slugTeamPlayer1, location, play, event_play, timeQuarter, timeRemaining, numberPeriod, scoreAway:marginScore) %>%
  mutate(marginScore2 = scoreHome - scoreAway) %>%
  filter(!is.na(scoreAway),
         !is.na(scoreHome),
         !is.na(marginScore2),
         !is.na(play)) %>%
  select(-marginScore) %>%
  group_by(idGame) %>%
  mutate(prev_value = lag(marginScore2),
         prev_value = replace_na(prev_value, 0),
         score_change = marginScore2 - prev_value,
         pos_neg = case_when(score_change > 0 ~ 1,
                             TRUE ~ -1),
         group = cumsum(pos_neg != lag(pos_neg, default = first(pos_neg)))) %>% 
  ungroup() %>%
  group_by(pos_neg, idGame, group) %>%
  mutate(counter = row_number()) %>%
  ungroup() %>%
  group_by(idGame, group) %>%
  mutate(run = cumsum(abs(score_change))) %>%
  ungroup() %>%
  rename(GameID = idGame, Team = slugTeamPlayer1) %>%
  left_join(gameLogs_yesterday %>% select(GameID, Team, Outcome)) %>%
  distinct() %>%
  mutate(numberPeriod = get_ord_numbers(numberPeriod),
         numberPeriod = case_when(numberPeriod == '5th' ~ '1st OT',
                                  numberPeriod == '6th' ~ '2nd OT', 
                                  TRUE ~ numberPeriod),
         timeQuarter = as.character(timeQuarter),
         #timeQuarter = str_sub(timeQuarter, 1, nchar(timeQuarter) - 3))
         location = case_when(location == 'descriptionPlayHome' ~ 'Home',
                              TRUE ~ 'Away'),
         leading_team = case_when(location == 'Home' & scoreHome > scoreAway ~ 'Leading',
                                  location == 'Home' & scoreHome < scoreAway ~ 'Trailing',
                                  location == 'Away' & scoreHome > scoreAway ~ 'Trailing',
                                  location == 'Away' & scoreHome < scoreAway ~ 'Leading',
                                  TRUE ~ 'Tied'),
         timeRemaining2 = case_when(numberPeriod == '1st' ~ 48,
                                    numberPeriod == '2nd' ~ 36,
                                    numberPeriod == '3rd' ~ 24,
                                    numberPeriod == '4th' ~ 12,
                                    numberPeriod == '1st OT' ~ 0,
                                    numberPeriod == '2nd OT' ~ -5),
         new_time = round(sapply(strsplit(timeQuarter,":"),
                                 function(x) {
                                   x <- as.numeric(x)
                                   x[1]+x[2]/60
                                 }
         ), 3),
         time_differential = 12 - new_time,
         new_time3 = case_when(numberPeriod == '2nd OT' ~ -10 + new_time,
                               numberPeriod == '1st OT' ~ -5 + new_time,
                               TRUE ~ timeRemaining2 - time_differential)) %>%
  left_join(team_colors) %>%
  left_join(gameLog_gameids_yesterday_text) %>%
  mutate(my_title_span = paste0("<span style='color:", primary_color, "';>", nameTeam, "</span>", " (", Outcome, ")"))

get_leading_times <- function(df){
  try <- df %>%
    select(Team, numberPeriod, timeQuarter, leading_team:new_time3) %>%
    mutate(prev_time = lag(new_time3),
           prev_time = replace_na(prev_time, 48.0),
           time_difference = round(60 * (prev_time - new_time3)),
           time_difference = replace_na(time_difference, 0))
  last_record_team <- tail(try, 1)
  
  try <- try %>%
    add_row(Team = last_record_team$Team, numberPeriod = last_record_team$numberPeriod, timeQuarter = as.character(0:00),
            leading_team = last_record_team$leading_team, timeRemaining2 = 12, new_time = 0,
            time_differential = timeRemaining2 - new_time, new_time3 = new_time, prev_time = last_record_team$new_time3, 
            time_difference = round(60 * (prev_time - new_time3))) %>%
    group_by(Team, leading_team) %>%
    summarize(time = sum(time_difference)) %>%
    ungroup() %>%
    pivot_wider(names_from = leading_team,
                names_glue = '{leading_team}_{.value}',
                values_from = time) %>%
    mutate(opp_leadtime = rev(Trailing_time),
           opp_tiedtime = rev(Tied_time),
           tot_leadtime = Leading_time + opp_leadtime,
           tot_trailtime = rev(tot_leadtime),
           tot_tiedtime = Tied_time + opp_tiedtime,
           tot_time = tot_leadtime + tot_trailtime + tot_tiedtime,
           pct_leadtime = round(tot_leadtime / tot_time, 3),
           pct_tiedtime = round(tot_tiedtime / tot_time, 3)) %>%
    left_join(team_colors) %>%
    mutate(text = paste0("<span style='color:", primary_color, "';>", Team, '</span> led for ',
                         pct_leadtime * 100, ' % of the Game'),
           tied_text = paste0('The teams were tied for ', (pct_tiedtime * 100), '% of the Game')) %>%
    select(text, tied_text)
  
  return(try)
  
}

game_event_plot <- function(df){
  df_val <- tail(df, 1) %>%
    select(numberPeriod) %>%
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
    else {
      labels = c("1st Quarter", "2nd Quarter", "3rd Quarter", "4th Quarter", "End 4th")
      return(labels)
    }
  }
  
  team_names <- unique(df$my_title_span)
  
  lead_times <- get_leading_times(df)
  
  p <- df %>%
    ggplot(aes(new_time3, marginScore2)) +
    geom_point(aes(color = primary_color, text = paste0(timeQuarter, ' in the ', numberPeriod, ' Quarter', '<br>',
                                                        play, '<br>',
                                                        Team, ' ', leading_team, ' ', scoreHome, '-', scoreAway)),
               show.legend = FALSE) +
    geom_line(alpha = 0.4) +
    geom_hline(yintercept = 0, alpha = 0.5) +
    scale_x_reverse(breaks = get_breaks(df),
                    labels = get_labels(df)) +
    scale_color_identity() +
    annotate(geom = "text", label = lead_times[1, 1]$text,
             x = max(df$new_time3) * .8, y = min(df$marginScore2) * .65) +
    annotate(geom = "text", label = lead_times[2, 1]$text,
             x = max(df$new_time3) * .8, y = min(df$marginScore2) * .78) +
    labs(x = 'Quarter',
         y = 'Score Differential',
         title = paste0(team_names[2], ' vs ', team_names[1])) +
    theme(legend.position = 'none')
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(hoverlabel = list(bgcolor = "white"))
  
}

rm(gameLogs_yesterday, pbp_data)



get_leading_times <- function(df){
  team_colors <- df %>%
    select(scoring_team, scoring_team_color) %>%
    distinct()
  try <- df %>%
    select(team, quarter, time_quarter, time_remaining_final) %>%
    mutate(prev_time = lag(time_remaining_final),
           prev_time = replace_na(prev_time, 48.0),
           time_difference = round(60 * (prev_time - time_remaining_final)),
           time_difference = replace_na(time_difference, 0))
  last_record_team <- tail(try, 1)
  
  mydf <- data.frame(Team = as.character(),
                     Trailing_time = as.numeric(),
                     Leading_time = as.numeric(),
                     Tied_time = as.numeric())
  
  # there was never a tie.
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
                values_from = time)
  
  df2 <- mydf %>%
    full_join(try) %>%
    mutate(Leading_time = replace_na(Leading_time, 0),
           Tied_time = replace_na(Tied_time, 0),
           Trailing_time = replace_na(Trailing_time, 0),
           opp_leadtime = rev(Trailing_time),
           opp_tiedtime = rev(Tied_time),
           tot_leadtime = Leading_time + opp_leadtime,
           tot_trailtime = rev(tot_leadtime),
           tot_tiedtime = Tied_time + opp_tiedtime,
           tot_time = tot_leadtime + tot_trailtime + tot_tiedtime,
           pct_leadtime = round(tot_leadtime / tot_time, 3),
           pct_tiedtime = round(tot_tiedtime / tot_time, 3)) %>%
    left_join(team_colors) %>%
    mutate(text = paste0("<span style='color:", scoring_team_color, "';>", Team, '</span> led for ',
                         pct_leadtime * 100, ' % of the Game'),
           tied_text = paste0('The teams were tied for ', (pct_tiedtime * 100), '% of the Game')) %>%
    select(text, tied_text)
  
  
  return(df2)
  
}
###
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
                values_from = time)
  
  df2 <- mydf %>%
    full_join(try) %>%
    mutate(Leading_time = replace_na(Leading_time, 0),
           Tied_time = replace_na(Tied_time, 0),
           Trailing_time = replace_na(Trailing_time, 0),
           opp_leadtime = rev(Trailing_time),
           opp_tiedtime = rev(Tied_time),
           tot_leadtime = Leading_time + opp_leadtime,
           tot_trailtime = rev(tot_leadtime),
           tot_tiedtime = Tied_time + opp_tiedtime,
           tot_time = tot_leadtime + tot_trailtime + tot_tiedtime,
           pct_leadtime = round(tot_leadtime / tot_time, 3),
           pct_tiedtime = round(tot_tiedtime / tot_time, 3)) %>%
    left_join(team_colors) %>%
    mutate(text = paste0("<span style='color:", scoring_team_color, "';>", scoring_team, '</span> led for ',
                         pct_leadtime * 100, ' % of the Game'),
           tied_text = paste0('The teams were tied for ', (pct_tiedtime * 100), '% of the Game')) %>%
    select(text, tied_text)
  
  
  return(df2)
  
}



####

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
                                                             leading_team, ' ', score_home, '-', score_away)),
               show.legend = FALSE) +
    geom_line(alpha = 0.4) +
    geom_hline(yintercept = 0, alpha = 0.5) +
    scale_x_reverse(breaks = get_breaks(df),
                    labels = get_labels(df)) +
    scale_color_identity() +
    annotate(geom = "text", label = lead_times$text[1],
             x = 44, y = y_loc) +
    annotate(geom = "text", label = lead_times$text[2],
             x = 44, y = y_loc * .95) +
    labs(x = NULL,
         y = 'Score Differential',
         title = paste0(df$away_fill[1], ' Vs. ', df$home_fill[1])) +
    theme(legend.position = 'none')
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(hoverlabel = list(bgcolor = "white"))
  
  
}
df <- pbp_data %>%
  filter(home_team == 'UTA')

game_event_plot(df)

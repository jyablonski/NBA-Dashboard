source('global.R')
source('content/body_standings.R')
source('content/body_recent.R')
source('content/body_team_plots.R')
source('content/body_schedule.R')
source('content/body_social_media_analysis.R')
# source('content/body_player_analysis.R')
source('content/body_about.R')

ui <- fluidPage(
  theme = shinytheme("sandstone"),  #sandstone, cosmo, 
  # tags$head(# includeHTML(("google-analytics.html")),
  #           tags$link(rel="icon", href="favicon.png")),
  # titlePanel(
  #   windowTitle = "2021-22 NBA Season Dashboard",
  #   title = tags$head(tags$link(rel="icon", 
  #                               href="favicon.ico", 
  #                               type="image/x-icon")
  #   )),
  tags$head(includeScript("js/navbar.js")),
  tags$head(tags$link(rel="icon", type = "image/x-icon", href="favicon.ico")),
  tags$style(type = "text/css", ".selectize-input {background-color:#F0F0F0;
                      color:#000000;
                      border-color:#000000;
                      border-style:solid;
                      border-width:2px;
                      border-radius:5%;
                      font-size:15px;}"),
  tags$style(type = "text/css", ".container-fluid {padding-left: 0px; padding-right: 0px !important;}"),
  tags$style(type = "text/css", ".navbar {margin-bottom: 0px; padding-left: 15px;"),
  tags$style(type = "text/css", ".content {padding: 0px;}"),
  tags$style(type = "text/css", ".row {margin-left: 0px; margin-right: 0px;"),
  tags$style(HTML("#final_text {text-align: center;}
                                div.box-header {
                                  text-align: center;}")),
  tags$style(HTML(".col-sm-12 { padding: 5px; margin-bottom: 0px; }")),
  tags$style(HTML(".col-sm-6 { padding: 5px; margin-bottom: 0px; }")),
  tags$style(HTML(".box.box-solid.box-primary>.box-header {color:#fff;
  background:#666666}
  .box.box-solid.box-primary
  {
  border-bottom-color:#666666;
  border-left-color:#666666;
  border-right-color:#666666;
  border-top-color:#666666;
  }")),
  navbarPage(' 2022-23 NBA Season Dashboard',
             tabPanel("Overview", dashboardPage(title = "Overview",
                                                header = dashboardHeader(disable = TRUE),
                                                sidebar = dashboardSidebar(disable = TRUE),
                                                body = body_standings)),
             tabPanel("Recent Games", dashboardPage(title = " Recent Games",
                                                    header = dashboardHeader(disable = TRUE),
                                                    sidebar = dashboardSidebar(disable = TRUE),
                                                    body = body_recent)),
             tabPanel("Team Plots", dashboardPage(title = "Team Plots",
                                                  header = dashboardHeader(disable = TRUE),
                                                  sidebar = dashboardSidebar(disable = TRUE),
                                                  body = body_team_plots)),
             tabPanel("Schedule", dashboardPage(title = "Schedule",
                                                header = dashboardHeader(disable = TRUE),
                                                sidebar = dashboardSidebar(disable = TRUE),
                                                body = body_schedule)),
             tabPanel("Social Media Analysis", dashboardPage(title = "Social Media Analysis",
                                                header = dashboardHeader(disable = TRUE),
                                                sidebar = dashboardSidebar(disable = TRUE),
                                                body = body_social_media_analysis)),
             navbarMenu(title = "API"),
             # tabPanel("Player Analysis", dashboardPage(title = "Player Analysis",
             #                                                 header = dashboardHeader(disable = TRUE),
             #                                                 sidebar = dashboardSidebar(disable = TRUE),
             #                                                 body = body_player_analysis)),
             tabPanel("About", dashboardPage(title = "About",
                                             header = dashboardHeader(disable = TRUE),
                                             sidebar = dashboardSidebar(disable = TRUE),
                                             body = body_about))
  )
)
# Server Logic
server <- function(input, output, session) {
  
  ################
  #              #
  #   OVERVIEW   #   
  #              #   
  ################
  
  output$today_date <- renderText({
    paste0('Data Updated as of ', updated_date)
    
  })
  
  output$bans_avg_pts <- renderValueBox({
    valueBox(
      value = bans$avg_pts[1], HTML(paste0("League Average Points Scored <br> <br> ",
                                           bans$pct_change[1], "% difference from Last Season")),
      icon = icon("caret-up"), color = "blue"
    )
  })
  
  output$bans_active_protocols <- renderValueBox({
    valueBox(
      value = bans$sum_active_protocols[1], HTML(paste0("Active Players in COVID Protocols <br> <br>",
                                            bans$protocols_text[1], "")),
      icon = icon("caret-up"), color = "blue"
    )
  })
  
  output$bans_date <- renderValueBox({
    gp_valuebox_function(bans)
  })
  
  output$bans_homeroad <- renderValueBox({
    valueBox(
      value = bans$record[1], HTML(paste0("League Wide Home - Road Win Record <br> <br> ",  
                                          (bans$win_pct[1] * 100), "% - ", (bans$win_pct[2] * 100),
                                          '% Win Percentage Splits')),
      icon = icon("chart-bar"), color = "blue"
    )
  })
  
  
  output$east_standings_table <- DT::renderDataTable(east_standings, rownames = FALSE, 
                                                     options = list(searching = FALSE,
                                                                    pageLength = 15,
                                                                    lengthChange = FALSE, info = FALSE,
                                                                    paging = FALSE))
  output$west_standings_table <- DT::renderDataTable(west_standings, rownames = FALSE,
                                                     options = list(searching = FALSE,
                                                                    pageLength = 15, 
                                                                    lengthChange = FALSE, info = FALSE,
                                                                    paging = FALSE))
  
  output$contract_value_output <- renderPlotly({
    value_plot(contracts_value)
  })
  
  output$team_contract_value_output <- renderPlotly({
    team_contract_value_plot(team_contract_analysis)
  })
  
  output$top20_plot_output <- renderPlotly({
    # if (input$select_ppg_plot_choice == 'Regular Season') {
    #   top20_plot(top_scorers)
    # }
    # else {
    top20_plot(top_scorers)
    # }
  })
  
  
  output$team_plot_output <- renderPlot({
    
    team_ratings_plot(team_ratings)
  })
  
  
  ################
  #              #
  # RECENT GAMES #   
  #              #   
  ################
  
  selected_game_event <- reactive({
    pbp_data %>%
      filter(game_description == input$select_game)
  })
  
  output$yesterday_event_output <- renderPlotly({
    game_event_plot(selected_game_event()) 
  })
  

  output$top_15 <- render_gt(player_gt_table(recent_games_players))
    
                                       
  output$recent_team_wins <- render_gt(team_gt_table(recent_games_teams))


  ################
  #              #
  #  TEAM PLOTS  #   
  #              #   
  ################
  
  selected_team_transactions <- reactive({
    transactions %>%
      filter(str_detect(Transaction, input$select_team)) %>%
      arrange(desc(Date))
  })
  
  output$transactions_table <- DT::renderDataTable(selected_team_transactions())
  
  
  selected_team_ppg <- reactive({
    top_scorers %>%
      filter(full_team == input$select_team)
  })
  
  output$team_ppg_plot_output <- renderPlotly({
    team_ppg_plot(selected_team_ppg()) 
  })
  
  
  selected_team_mov <- reactive({
    mov %>%
      filter(full_team == input$select_team)
  })
  
  
  output$team_mov_output <- renderPlotly({
    # df <- selected_team()
    mov_plot(selected_team_mov())
  })
  
  selected_team_injury <- reactive({
    injuries %>%
      filter(Team == input$select_team)
  })
  
  output$injury_table <- DT::renderDataTable(selected_team_injury(), rownames = FALSE,
                                             options = list(searching = FALSE,
                                                            pageLength = 15, 
                                                            lengthChange = FALSE, info = FALSE,
                                                            paging = FALSE))
  
  selected_team_bans <- reactive({
    standings %>%
      filter(team_full == input$select_team)
  })
  
  selected_team_rating_bans <- reactive({
    team_ratings_bans %>%
      filter(team == input$select_team)
  })
  
  selected_team_defensive_rating_bans <- reactive({
    opp_stats %>%
      filter(team == input$select_team)
  })
  
  
  
  
  output$regular_wins <- renderValueBox({
    regular_valuebox_function(selected_team_bans())
  })
  
  output$last_season_wins <- renderValueBox({
    # last_season_valuebox_function(selected_team_last_season())
    regular_valuebox_function(selected_team_bans())
  })
  
  output$team_ratings_rank <- renderValueBox({
    valueBox(
      value = "Team Ratings", HTML(selected_team_rating_bans()$rating_text),
      icon = icon("caret-up"), color = "blue"
    )
  })
  
  output$team_defensive_ratings_rank <- renderValueBox({
    valueBox(
      value = "Team Defensive Metrics", HTML(selected_team_defensive_rating_bans()$rating_text),
      icon = icon("caret-up"), color = "blue"
    )
  })
  
  ###############################
  #                             #
  #    SOCIAL MEDIA ANALYSIS    #   
  #                             #   
  ###############################
  
  output$reddit_table <- DT::renderDataTable(reddit_data, rownames = FALSE,
                                             options = list(pageLength = 10))
  
  output$twitter_table <- DT::renderDataTable(twitter_data, rownames = FALSE,
                                              options = list(pageLength = 10),
                                              caption = htmltools::tags$caption(
                                                style = 'caption-side: bottom;',
                                                htmltools::em("Data collected from Twitter's API can include anything within the past 7 days.")
                                              )
  )
  
  selected_social_media <- reactive({
    if (input$select_social_media == 'Reddit Comments') {
      reddit_data
    }
    else {
      twitter_data
    }
  })
  
  selected_reddit_plot <- reactive({
    if (input$select_reddit_plot_choice == 'Reddit Comments') {
      reddit_comment_plot(reddit_team_sentiment, input$select_team_social)
    }
    else {
      reddit_sentiment_plot(reddit_team_sentiment, input$select_team_social)
    }
  })
  
  output$reddit_plot_output <- renderPlotly({
    selected_reddit_plot()
  })
  
  output$bans_reddit <- renderValueBox({
    valueBox(
      value = social_media_bans$reddit_tot_comments[1], HTML(paste0("Total Reddit Comments Scraped <br> <br> ",
                                                                    social_media_bans$reddit_pct_difference[1], "% difference from average")),
      icon = icon("caret-up"), color = "blue"
    )
  })
  
  output$bans_twitter <- renderValueBox({
    valueBox(
      value = social_media_bans$twitter_tot_comments[1], HTML(paste0("Total Tweets Scraped <br> <br> ",
                                                                     social_media_bans$twitter_pct_difference[1], "% difference from average")),
      icon = icon("caret-up"), color = "blue"
    )
  })
  
  
  
  output$social_media_table <- DT::renderDataTable(datatable(selected_social_media(), rownames = FALSE,
                                                   options = list(pageLength = 10),
                                                   escape = FALSE) %>%
                                                   # container = sentiment_analysis_header) %>%
                                                     formatPercentage(c('Compound Sentiment Score',
                                                                        'Pos',
                                                                        'Neutral',
                                                                        'Neg')
                                                                      )
  )

  #############################
  #                           #
  #   PLAYER VALUE ANALYSIS   #   
  #                           #   
  #############################
  
  output$player_analysis_table <- renderPlotly({
    player_analysis_plot_function(rolling_avg,
                                  input$select_player_plot_choice,
                                  # input$select_player_plot_count_choice,
                                  input$select_player_plot_team_choice)
  })
  
  
  ################
  #              #
  #   SCHEDULE   #   
  #              #   
  ################
  
  output$schedule_table <- DT::renderDataTable(schedule, rownames = FALSE,
                                               options = list(pageLength = 10))
  
  output$schedule_table <- DT::renderDataTable({
    if ((input$select_schedule == "Tonight's Games") & (("Home Predicted Win %" %in% names(schedule_tonight)))) {
      # render_gt(schedule_gt_table(schedule))
      datatable(
        schedule_tonight, rownames = FALSE,
        options = list(pageLength = 10,
                       columnDefs = list(list(visible=FALSE, targets = c(5, 6, 8, 9, 11, 12)))),
      caption = htmltools::tags$caption(
        style = 'caption-side: bottom;',
        htmltools::em('Win % Predictions created via Logistic Regression ML Model | Green Cell Coloring indicates great Odds Value')
        )
      ) %>%
        formatPercentage('Home Predicted Win %', 1) %>%
        formatPercentage('Road Predicted Win %', 1) %>%
        formatStyle("Road Team", valueColumns = "away_is_great_value", backgroundColor = styleEqual(c(1), c('#4BD33A'))) %>%
        formatStyle("Home Team", valueColumns = "home_is_great_value", backgroundColor = styleEqual(c(1), c('#4BD33A')))
    }
    else if (input$select_schedule == "Tonight's Games" & (!("Home Predicted Win %" %in% names(schedule_tonight))) & (nrow(schedule_tonight) > 0)) {
      datatable(schedule_tonight, rownames = FALSE,
                options = list(pageLength = 10))
    }
    else if (input$select_schedule == "Tonight's Games" & nrow(schedule_ml) == 0) {
      datatable(data.frame(`No Data` = "No Data Available for Tonight's Games"))
      }
    else { # if the ml pipeline failed for whatever reason just do this.
      datatable(schedule, rownames = FALSE,
      options = list(pageLength = 10))
    }
  })
  
  output$game_types_output <- renderPlotly({
    game_types_plot(game_types) #input$select_game_types)
  })
  
  output$schedule_plot_output <- renderPlotly({
    if (input$select_choice == 'Vegas Preseason Over/Under Odds') {
      vegas_plot(preseason_odds)
    }
    # else if (input$select_choice == 'Future Strength of Schedule') {
    #   future_schedule_analysis_plot(future_schedule_analysis)
    # }
    else if (input$select_choice == 'Team Comebacks Analysis (Regular Season)') {
      blown_leads_plot(team_blown_leads)
    }
    # else if (input$select_choice == 'Team Comebacks Analysis (Playoffs)') {
    #   blown_leads_plot(team_blown_leads, 'Playoffs')
    # }
    else {
      advanced_sos_plot(past_schedule_analysis)
    }
  })

  ################
  #              #
  #    ABOUT     #   
  #              #   
  ################
  
}



# Run the App
shinyApp(ui, server)

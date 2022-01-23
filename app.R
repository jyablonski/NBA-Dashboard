source('global.R')
source('content/body_standings.R')
source('content/body_recent.R')
source('content/body_team_plots.R')
source('content/body_schedule.R')
source('content/body_social_media_analysis.R')
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
  navbarPage(' 2021-22 NBA Season Dashboard',
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
    top20_plot(top_scorers)
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
      icon = icon("caret-up"), color = "purple"
    )
  })
  
  output$team_defensive_ratings_rank <- renderValueBox({
    valueBox(
      value = "Team Defensive Metrics", HTML(selected_team_defensive_rating_bans()$rating_text),
      icon = icon("caret-up"), color = "purple"
    )
  })
  
  ################
  #              #
  #   SCHEDULE   #   
  #              #   
  ################
  
  output$schedule_table <- DT::renderDataTable(schedule, rownames = FALSE,
                                               options = list(pageLength = 10))
  
  
  output$game_types_output <- renderPlotly({
    game_types_plot(game_types)
  })
  
  output$schedule_plot_output <- renderPlotly({
    if (input$select_choice == 'Vegas Preseason Over/Under Odds') {
      vegas_plot(preseason_odds)
    }
    else if (input$select_choice == 'Future Strength of Schedule') {
      future_schedule_analysis_plot(future_schedule_analysis)
    }
    else if (input$select_choice == 'Team Comebacks Analysis') {
      blown_leads_plot(team_blown_leads)
    }
    else {
      advanced_sos_plot(past_schedule_analysis)
    }
  })
  
  ###############################
  #                             #
  #    SOCIAL MEDIA ANALYSIS    #   
  #                             #   
  ###############################
  
  output$reddit_table <- DT::renderDataTable(reddit_data, rownames = FALSE,
                                               options = list(pageLength = 25))
  
  output$twitter_table <- DT::renderDataTable(twitter_data, rownames = FALSE,
                                               options = list(pageLength = 25))
  
  selected_social_media <- reactive({
    if (input$select_social_media == 'Reddit Comments') {
      reddit_data
    }
    else {
      twitter_data
    }
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

  

  output$social_media_table <- DT::renderDataTable(selected_social_media(), rownames = FALSE,
                                                  options = list(pageLength = 25),
                                                  escape = FALSE)
  
  
  ################
  #              #
  #    ABOUT     #   
  #              #   
  ################
  
}



# Run the App
shinyApp(ui, server)

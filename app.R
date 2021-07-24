source('global.R')
source('content/body_standings.R')
source('content/body_recent.R')
source('content/body_team_plots.R')
source('content/body_about.R')
source('content/body_schedule.R')


ui <- fluidPage(
  theme = shinytheme("sandstone"),  #sandstone, cosmo, 
  tags$head(includeHTML(("google-analytics.html")),
            tags$style(HTML( ".selectize-input {background-color:#F0F0F0;
                      color:#000000;
                      border-color:#000000;
                      border-style:solid;
                      border-width:2px;
                      border-radius:5%;
                      font-size:15px;}"))),
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
  navbarPage(' 2020-2021 NBA Season Dashboard',
             tabPanel("Overview", dashboardPage(title = "Overview",
                                                header = dashboardHeader(disable = TRUE),
                                                sidebar = dashboardSidebar(disable = TRUE),
                                                body = body_standings)),
             tabPanel("Most Recent Games", dashboardPage(title = "Most Recent Games",
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
             tabPanel("About", dashboardPage(title = "About",
                                             header = dashboardHeader(disable = TRUE),
                                             sidebar = dashboardSidebar(disable = TRUE),
                                             body = body_about))
  )
)
# Server Logic
server <- function(input, output, session) {
  
  output$today_date <- renderText({
    paste0('Data Updated as of ', updated_date)
    
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
  
  selected_team_transactions <- reactive({
    transactions %>%
      filter(str_detect(Event, input$select_team))
  })
  
  output$transactions_table <- DT::renderDataTable(selected_team_transactions())
  
  output$schedule_table <- DT::renderDataTable(schedule_main, rownames = FALSE,
                                               options = list(pageLength = 20))

  output$recent_date <- renderValueBox({
    valueBox(
      value = format(recent_Bans$Date, format = "%B %d, %Y"), "Most Recent Game Date", icon = icon("calendar"), color = "purple"
    )
  })
  
  output$recent_games_played <- renderValueBox({
    valueBox(
      value = recent_Bans$`Number of Games`, "Games Played", icon = icon("list"), color = "purple"
    )
  })
  
  output$recent_home_wins <- renderValueBox({
    valueBox(
      value = recent_Bans$`Count_Home Wins`, "Home Wins", icon = icon("music note list"), color = "red"
    )
  })
  
  output$recent_road_wins <- renderValueBox({
    valueBox(
      value = recent_Bans$`Count_Road Wins`, "Road Wins", icon = icon("music note list"), color = "red"
    )
  })

  output$top_15 <- render_gt(player_gt_table(top_15_yesterday))
    
                                       
  output$recent_team_wins <- render_gt(team_gt_table(team_avg_ppg))
  
  output$top20_plot_output <- renderPlotly({
    if (input$select_ppg_choice == 'Regular Season') {
      top20_plot(top_20pt_scorers) 
    }
    else {
      top20_plot_playoffs(top_20pt_scorersp)
    }
  })
  
  output$schedule_plot_output <- renderPlotly({
    if (input$select_choice == 'Vegas Preseason Over/Under Odds') {
      vegas_plot(over_under)
    }
    else {
      advanced_sos_plot(advanced_standings)
    }
  })
  
  selected_team_injury <- reactive({
    injury_data %>%
      filter(FullName == input$select_team)
  })
  
  selected_game_event <- reactive({
    pbp_event_df %>%
      filter(text == input$select_game)
  })
  
  output$yesterday_event_output <- renderPlotly({
    game_event_plot(selected_game_event()) 
  })
  
  output$team_plot_output <- renderPlot({

    team_ratings_plot(team_ratings_logo)
  })
  
  
  selected_team_ppg <- reactive({
    team_ppg_scorers %>%
      filter(FullName == input$select_team)
  })

  output$team_ppg_plot_output <- renderPlotly({
    team_ppg_plot(selected_team_ppg()) 
  })
  
  
  selected_team <- reactive({
    team_mov %>%
      filter(FullName == input$select_team)
  })
  
  
  selected_team_injury <- reactive({
    injury_data %>%
      filter(FullName == input$select_team)
  })
  
  output$team_mov_output <- renderPlotly({
    df <- selected_team()
    mov_plot(df)
  })
  
  output$injury_table <- DT::renderDataTable(selected_team_injury(), rownames = FALSE,
                                                 options = list(searching = FALSE,
                                                                pageLength = 15, 
                                                                lengthChange = FALSE, info = FALSE,
                                                                paging = FALSE))
  
  selected_team_bans <- reactive({
    team_wins %>%
      filter(FullName == input$select_team)
  })
  
  selected_team_rating_bans <- reactive({
    rating_bans %>%
      filter(FullName == input$select_team)
  })
  
  selected_team_defensive_rating_bans <- reactive({
    opponent_shooting_bans %>%
      filter(FullName == input$select_team)
  })
  
  selected_team_last_season <- reactive({
    last_season_wins %>%
      filter(FullName == input$select_team)
  })
  
  output$regular_wins <- renderValueBox({
    regular_valuebox_function(selected_team_bans())
  })
  
  output$last_season_wins <- renderValueBox({
    last_season_valuebox_function(selected_team_last_season())
  })
  
  output$game_types_output <- renderPlotly({
    game_types_plot(game_types)
  })
  
  output$contract_value_output <- renderPlotly({
    value_plot(contract_df)
  })
  
  output$team_contract_value_output <- renderPlotly({
    team_contract_value_plot(team_contract_value)
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
  
  output$bans_avg_pts <- renderValueBox({
    valueBox(
      value = main_bans$league_avg, HTML(paste0("League Average Points Scored <br> <br> ",
                                                         main_bans$pct_change, "% difference from Last Season")),
      icon = icon("caret-up"), color = "blue"
    )
  })
  
  output$bans_date <- renderValueBox({
    gp_valuebox_function(main_bans)
  })
  
  output$bans_homeroad <- renderValueBox({
    valueBox(
      value = main_bans$record, HTML(paste0("League Wide Home - Road Win Record <br> <br> ",  
                                                       main_bans$`Win Percentage Home`, "% - ", main_bans$`Win Percentage Road`,
                          '% Win Percentage Splits')),
      icon = icon("chart-bar"), color = "blue"
    )
  })
  
}



# Run the App
shinyApp(ui, server)

body_player_analysis <- dashboardBody(
  fluidRow(
    box(
      fluidRow(
        column(2,
               selectInput('select_player_plot_choice', "Select a Variable to Plot",
                           choices = c('PPG',
                                       'Player Value Metric',
                                       'TS%',
                                       'Plus Minus')
                           ),
        ),
        # column(2,
        #        selectInput('select_player_plot_count_choice', "Select a Selection Type",
        #                    choices = c('Top 20',
        #                                'Bottom 20',
        #                                'Full List')
        #                    ),
        # ),
        column(2,
               selectInput('select_player_plot_team_choice', "Select a Team",
                           choices = player_analysis_team_choices
               ),
        )
        ),
      title = "Rolling Average Analysis",
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 12,
      plotlyOutput("player_analysis_table", height = "600px")
    )
    # column(width = 12, DT::dataTableOutput("schedule_table")),
  )
)

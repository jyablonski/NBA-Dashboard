body_team_plots <- dashboardBody(
  fluidRow(
    box(h4(paste0("Key Team Metrics as of ", today)),
        fluidRow(
          column(
            box(
              selectInput('select_team', h3("Select a Team"),
                          choices = team_choices),
             width = 3),
            valueBoxOutput("regular_wins", width = 3),
            valueBoxOutput("team_ratings_rank", width = 3),
            valueBoxOutput("team_defensive_ratings_rank", width = 3),
            width = 12,
            style = "margin-left: -20px"
          )
        ),
        div("Last updated: ", updated_date),
        width = 12)),
  fluidRow(
    box(
      title = "Team Margin of Victory Plot",
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 6,
      plotlyOutput("team_mov_output", height = "600px")
    ),
    box(
      title = "Team PPG Plot",
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 6,
      plotlyOutput("team_ppg_plot_output", height = "600px")
    )),
  fluidRow(
    box(
      title = "Team Injuries",
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 6,
      DT::dataTableOutput("injury_table")
    ),
    box(
      title = "Team Transactions",
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 6,
      DT::dataTableOutput("transactions_table")
    ))
)

#   fluidRow(
# column(width = 6, h4("Injury Table"), DT::dataTableOutput("injury_table")),
# column(width = 6, h4('Transactions Table'), DT::dataTableOutput("transactions_table"))
# )


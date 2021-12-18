body_schedule <- dashboardBody(
  fluidRow(HTML(" Moneyline Odds for Today's Games provided by "),
            img(src = "draftkings.png", height = 70, width = 140), tags$a(href="https://www.espn.com/nba/schedule", "| Click here for National TV Schedule")),
  fluidRow(
    box(
      title = "Regular Season Game Type Analysis",
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 12,
      DT::dataTableOutput("schedule_table")
    )
    # column(width = 12, DT::dataTableOutput("schedule_table")),
  ),
  fluidRow(
    box(
      title = "Regular Season Game Type Analysis",
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 6,
      plotlyOutput("game_types_output", height = "600px")
    ),
    box(
      fluidRow(
        column(4, 
               selectInput('select_choice', "Select a Plot",
                           choices = c('Strength of Schedule (as of Today)',
                                       'Future Strength of Schedule',
                                       'Team Comebacks Analysis',
                                       'Vegas Preseason Over/Under Odds'))
        )),
      title = "NBA Strength of Schedule Analysis",
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 6,
      plotlyOutput("schedule_plot_output", height = "600px")
    )
  )
)
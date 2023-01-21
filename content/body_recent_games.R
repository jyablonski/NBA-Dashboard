body_recent <- dashboardBody(
  tags$head(
    tags$style(type = "text/css", ".legend { list-style: none; margin-left: -30px;}"),
    tags$style(type = "text/css", ".legend li { float: left; margin-right: 10px; position: relative; }"),
    tags$style(type = "text/css", ".legend span { border: 1px solid #ccc; float: left; width: 30px; height: 15px;
                   margin-right: 5px; margin-top: 1px; position: relative;"),
    tags$style(type = "text/css", ".legend .pos1 { background-color: #9362DA; }"), # purple
    tags$style(type = "text/css", ".legend .pos2 { background-color: #3fb7d9; }"), # blue
    tags$style(type = "text/css", ".legend .pos3 { background-color: #e04848; }"), # red
    tags$style(type = "text/css", ".legend .pos4 { background-color: #4BD33A; }"), # green
    tags$style(type = "text/css", ".legend .pos5 { background-color: #FFFFFF; }"), # white (clear)
    #tags$style(type = "text/css", ".legend .pos6 { background-color: #FF0000; }"),
    tags$style(type = "text/css", ".legend .neg1 { background-color: #3fb7d9; }"),
    tags$style(type = "text/css", ".legend .neg2 { background-color: #FF0000; }"),
    tags$style(type = "text/css", ".legend .neg3 { background-color: #e04848; }"),
    tags$style(type = "text/css", "@media (min-width: 768px) { .full-table { margin-top: -30px; } }")
  ),
  #fluidRow(
    #valueBoxOutput("recent_date", width = 6),
    #valueBoxOutput("recent_games_played", width = 6),
  #),
  fluidRow(
    box(column(width = 12, gt_output("top_15")), width = 4),
    box(column(width = 12, gt_output("recent_team_wins")), width = 4),
    box(column(width = 12, gt_output("injury_report")), width = 4)
  ),
  fluidRow(
    column(width = 4,
    div(
      tags$h5("Table Cell Coloring for Player Averages", style = "margin-left: 10px;"),
      tags$ul(class = "legend",
              tags$li(tags$span(class = "pos1"), " Season High"),
              tags$li(tags$span(class = "pos2"), "10+ pts Above"),
              tags$li(tags$span(class = "pos3"), "10+ pts Below"),
              tags$li(tags$span(class = "pos4"), "High Efficiency"),
              tags$li(tags$span(class = "pos5"), "Lower Efficiency"),
              #tags$li(tags$span(class = "pos6"), "TS Less than 40%"),
              tags$br()
      )
      )),
    column(width = 4,
    div(
      tags$h5("Table Cell Coloring for Team Averages", style = "margin-left: 10px;"),
      tags$ul(class = "legend",
              tags$li(tags$span(class = "pos1"), " Season High"),
              tags$li(tags$span(class = "neg1"), " 10+ pts Above"),
              tags$li(tags$span(class = "neg3"), "10+ pts Below")
    )
    ))#width = 12
  ),
  fluidRow(
    box(
    box(
      selectInput('select_game', h3("Select a Recent Game"),
                  choices = pbp_games_yesterday),
      width = 3),
    box(solidHeader = FALSE,
      collapsible = FALSE,
      width = 9,
      plotlyOutput("yesterday_event_output", height = "600px")
    ), title = "Play by Play Analysis",
    status = "primary",
    solidHeader = TRUE,
    collapsible = FALSE,
    width = 12
    )
  )
)



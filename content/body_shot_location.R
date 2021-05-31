body_shot_location <- dashboardBody(
  fluidPage(
    fluidRow(box(textInput("player_search", h3("Find your Player"), placeholder = 'Type a Players Name ...'), width = 3, height = 220,
                 solidheader = TRUE),
             box(selectInput('select_player', h3("Select your Player from these matches"), choices = c()), width = 3),
             box(selectInput('select_year', h3('Select a Season'), choices = year_df$year_choices), width = 3),
             box(selectInput('select_type', h3('Select a Season Type'), choices = c('Regular Season', 'Playoffs')),
                 actionButton("generate_plot", "Generate Plot",
                              style = "background-color:#F0F0F0;
                      color:#000000;
                      border-color:#000000;
                      border-style:solid;
                      border-width:2px;
                      border-radius:5%;
                      font-size:15px;", icon = icon('play')), width = 3, solidHeader = TRUE)
    ),
    fluidRow(
      box(
        title = "Hex Bin Shot Locations",
        status = "success",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 5,
        plotOutput("hexagon_plot", height = '500px')
      )
    )
  )
)

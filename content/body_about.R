body_about <- dashboardBody(
  fluidRow(
    fluidRow(
      column(
        box(
          title = div("About this Project", style = "padding-left: 20px", class = "h2"),
          column(
            "This dashboard shows up to date information about the 2020-2021 NBA Season.",
            tags$br(),
            h3("Data"),
            HTML("All NBA Game log, schedule, standings, and play-by-play data is webscraped from"),
             tags$a(href = "https://www.basketball-reference.com", "Sports Reference"), " at 8:00 A.M. EST everyday.",
            Sys.getenv('dbname'),
            tags$br(),
            tags$br(),
            Sys.getenv('aws_host'),
             "Gambling odds are taken from",  tags$a(href = "https://sportsbook.draftkings.com/leagues/basketball/88670846?category=game-lines&subcategory=game", "Draftkings"), " for that day's games.",
            tags$br(),
            tags$br(),
            Sys.getenv('aws_user'),
            HTML("The Dashboard's data is automatically updated whenever this application is loaded by a user."),
            h3("Developer"),
            "Jacob Yablonski | ",
            tags$a(href = "https://www.linkedin.com/in/jacobyablonski/", "LinkedIn"), "|",
            tags$a(href = "https://github.com/jyablonski", "Github"),
            width = 12,
            style = "padding-left: 20px; padding-right: 20px; padding-bottom: 40px; margin-top: -15px;"
          ),
          width = 6
        ),
        width = 12,
        style = "padding: 15px"
      )
    )
  )
)
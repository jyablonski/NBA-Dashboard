body_about <- dashboardBody(
  fluidRow(
    fluidRow(
      column(
        box(
          title = div("About This Project", style = "padding-left: 20px", class = "h2"),
          column(
            "This dashboard shows up to date information about the 2021-2022 NBA Season.",
            tags$br(),
            tags$br(),
            h3("Data"),
            HTML("All NBA Game log, schedule, standings, and play-by-play data is webscraped from"),
             tags$a(href = "https://www.basketball-reference.com", "Basketball Reference"),
            tags$br(),
             "Gambling odds are taken from",  tags$a(href = "https://sportsbook.draftkings.com/leagues/basketball/88670846?category=game-lines&subcategory=game", "Draftkings"), " for that day's games.",
            tags$br(),
            HTML("The Dashboard automatically updates whenever this application is loaded by a user."),
            tags$br(),
            tags$br(),
            h3("GitHub Links to Infrastructure for this Project:"),
            tags$br(),
            tags$a(href = "https://github.com/jyablonski/python_docker", "Python Web Scrape Script"),
            tags$br(),
            tags$a(href = "https://github.com/jyablonski/aws_terraform", "Terraform"),
            tags$br(),
            tags$a(href = "https://github.com/jyablonski/NBA-Dashboard", "Shiny"),
            tags$br(),
            tags$a(href = "https://github.com/jyablonski/nba_elt_dbt", "dbt"),
            tags$br(),
            h3("Developer"),
            "Jacob Yablonski | ",
            tags$a(href = "https://www.linkedin.com/in/jacobyablonski/", "LinkedIn"), "|",
            tags$a(href = "https://github.com/jyablonski", "Github"),
            tags$br(),
            tags$br(),
            h5("Version: 1.0.11"),
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
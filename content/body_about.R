body_about <- dashboardBody(
  fluidRow(
    fluidRow(
      column(
        box(
          title = div("About this Project", style = "padding-left: 20px", class = "h2"),
          column(
            "This dashboard shows up to date information about the 2021-2022 NBA Season.",
            tags$br(),
            tags$br(),
            h3("Data"),
            HTML("All NBA Game log, schedule, standings, and play-by-play data is webscraped daily from"),
             tags$a(href = "https://www.basketball-reference.com", "Basketball Reference"),
            tags$br(),
            tags$br(),
             "Gambling odds are taken from",  tags$a(href = "https://sportsbook.draftkings.com/leagues/basketball/88670846?category=game-lines&subcategory=game", "Draftkings"), " for today's games.",
            tags$br(),
            tags$br(),
            HTML("Reddit Comments are scraped from the top ~27 most active posts on "), 
            tags$a(href = "https://www.reddit.com/r/nba/", "r/nba"), HTML("from the previous day, while
                 Twitter Tweets are randomly collected and only include a subset of total volume due to scraping limitations."),
            tags$br(),
            tags$br(),
            HTML("A Logistic Regression Model is used to provide ML Predictions for tonight's games in the Schedule Tab."), 
            tags$br(),
            tags$br(),
            h3("GitHub Links to Infrastructure for this Project:"),
            tags$br(),
            tags$a(href = "https://github.com/jyablonski/python_docker", "Python Web Scrape"),
            tags$br(),
            tags$a(href = "https://github.com/jyablonski/aws_terraform", "Terraform"),
            tags$br(),
            tags$a(href = "https://github.com/jyablonski/NBA-Dashboard", "Shiny Server"),
            tags$br(),
            tags$a(href = "https://github.com/jyablonski/nba_elt_dbt", "dbt"),
            tags$br(),
            tags$a(href = "https://github.com/jyablonski/nba_elt_airflow", "Airflow Proof of Concept"),
            tags$br(),
            tags$a(href = "https://github.com/jyablonski/nba_elt_mlflow", "ML Pipeline"),
            tags$br(),
            tags$a(href = "https://github.com/jyablonski/graphql_praq", "GraphQL API"),
            tags$br(),
            tags$a(href = "https://github.com/jyablonski/nba_elt_rest_api", "REST API"),
            tags$br(),
            h3("Developer"),
            "Jacob Yablonski | ",
            tags$a(href = "https://www.linkedin.com/in/jacobyablonski/", "LinkedIn"), "|",
            tags$a(href = "https://github.com/jyablonski", "GitHub"),
            tags$br(),
            tags$br(),
            h5("Version: 1.6.0"),
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
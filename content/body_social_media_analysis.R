body_social_media_analysis <- dashboardBody(
  fluidRow(
    box(h4(paste0("Recent Social Media Comments & Posts as of ", today)),
        fluidRow(
          column(
            box(HTML("Social Media Scraping from <br> <br>"),
                img(src = "reddit.png", height = 70, width = 100), img(src = "twitter.png", height = 70, width = 140),
                width = 3),
            box(
                  selectInput('select_social_media', "Select a Table",
                                 choices = c('Reddit Comments',
                                             'Twitter Tweets')),
              width = 3),
            valueBoxOutput("bans_reddit", width = 3),
            valueBoxOutput("bans_twitter", width = 3),
            width = 12,
            style = "margin-left: -20px"
          )
        ),
        width = 12)),
  fluidRow(
    box(
      title = "Social Media Analysis",
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,
      width = 12,
      DT::dataTableOutput("social_media_table")
    )
  )
)
# fluidRow(
#   box(
#     title = "Reddit Comments Analysis",
#     status = "primary",
#     solidHeader = TRUE,
#     collapsible = FALSE,
#     width = 6,
#     DT::dataTableOutput("reddit_table")
#   ),
#   box(
#     title = "Twitter Tweets Analysis",
#     status = "primary",
#     solidHeader = TRUE,
#     collapsible = FALSE,
#     width = 6,
#     DT::dataTableOutput("twitter_table")
#     # column(width = 12, DT::dataTableOutput("schedule_table")),
#   )
# )
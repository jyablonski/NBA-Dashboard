body_social_media_analysis <- dashboardBody(
  fluidRow(
    HTML("   ", "Social Media Comments scraped from  "),
           img(src = "reddit.png", height = 70, width = 100), "   ", " and ", img(src = "twitter.png", height = 70, width = 140)),
  fluidRow(
    column(4, 
           selectInput('select_social_media', "Select a Table",
                       choices = c('Reddit Comments',
                                   'Twitter Tweets'))
    ),
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
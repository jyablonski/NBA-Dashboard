# deploy/deploy-shinyapps.R
# usethis::use_build_ignore("deploy")
install.packages("rsconnect", repos = "http://cran.us.r-project.org")
rsconnect::setAccountInfo(
  Sys.getenv("SHINYAPPS_ACCOUNT"),
  Sys.getenv("SHINYAPPS_TOKEN"),
  Sys.getenv("SHINYAPPS_SECRET")
)
rsconnect::deployApp(
  appName = "nbadashboard"
)
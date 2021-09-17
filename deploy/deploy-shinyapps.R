# deploy/deploy-shinyapps.R
# usethis::use_build_ignore("deploy")
devtools::install_github("rstudio/rsconnect")
rsconnect::setAccountInfo(
  Sys.getenv("SHINYAPPS_ACCOUNT"),
  Sys.getenv("SHINYAPPS_TOKEN"),
  Sys.getenv("SHINYAPPS_SECRET")
)
rsconnect::deployApp(
  appName = "nbadashboard"
)
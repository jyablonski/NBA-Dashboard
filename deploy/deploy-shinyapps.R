# deploy/deploy-shinyapps.R
# usethis::use_build_ignore("deploy")
install.packages"rstudio/rsconnect", repos = "https://cloud.r-project.org"))
rsconnect::setAccountInfo(
  Sys.getenv("SHINYAPPS_ACCOUNT"),
  Sys.getenv("SHINYAPPS_TOKEN"),
  Sys.getenv("SHINYAPPS_SECRET")
)
rsconnect::deployApp(
  appName = "nbadashboard"
)
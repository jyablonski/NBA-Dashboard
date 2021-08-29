# deploy/deploy-shinyapps.R
# usethis::use_build_ignore("deploy")
install.packages('renv')
rsconnect::setAccountInfo(
  Sys.getenv("SHINYAPPS_ACCOUNT"),
  Sys.getenv("SHINYAPPS_TOKEN"),
  Sys.getenv("SHINYAPPS_SECRET")
)
rsconnect::deployApp(
  appName = "nbadashboard",
  # exclude hidden files and renv directory (if present)
  appFiles = setdiff(list.files(), "renv")
)
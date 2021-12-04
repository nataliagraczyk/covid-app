library(rsconnect)

renv::restore()

rsconnect::setAccountInfo(
  name=Sys.getenv("SHINYAPP_NAME"), 
  secret=Sys.getenv("SHINYAPP_SECRET"), 
  token=Sys.getenv("SHINYAPP_TOKEN")
  )

rsconnect::deployApp(
  appName="covid-app", 
  appFiles = setdiff(list.files(), "renv")
  )

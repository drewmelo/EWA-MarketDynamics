rsconnect::setAccountInfo(
  name   = Sys.getenv("SHINYAPPS_NAME"),
  token  = Sys.getenv("SHINYAPPS_TOKEN"),
  secret = Sys.getenv("SHINYAPPS_SECRET")
)

# (opcional) confira se shiny/rgamer etc. foram detectados:
rsconnect::appDependencies("app/")

rsconnect::deployApp(
  appDir   = "app/",
  appName  = "ewa-market-dynamics",
  server   = "shinyapps.io",
  forceUpdate = T
)

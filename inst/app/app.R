library(randomPlatform)

shiny::shinyApp(
  ui = randomPlatform:::rp_app_ui(),
  server = randomPlatform:::rp_app_server
)

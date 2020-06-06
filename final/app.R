# Load libraries so they are available
library("shiny")


# Load 'ui' and 'server' variables
source("app_ui.R")
source("app_server.R")

# Create a new `shinyApp()` using the loaded `ui` and `server` variables
shinyApp(ui = ui, server = server)
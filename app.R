library(shiny)
# Loaded now to check that they install on shinyapps.io; used by the bivariate plots.
library(Hmisc)
library(UsingR)

ui <- navbarPage(
  "Exploration et classification",
  tabPanel("Données"),
  tabPanel("Prétraitement"),
  tabPanel("Exploration"),
  tabPanel("Modélisation"),
  tabPanel("Étude de cas")
)

server <- function(input, output, session) {}

shinyApp(ui, server)

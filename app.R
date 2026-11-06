library(shiny)
# Loaded now to check that they install on shinyapps.io; used by the bivariate plots.
library(Hmisc)
library(UsingR)

# Shiny refuses uploads above 5 MB by default; bank-additional-full.csv is about 5.5 MB.
options(shiny.maxRequestSize = 30 * 1024^2)

# Read once and shared between sessions; NULL when the file is missing.
baac_sample <- load_rds_or_null(file.path("data", "accidents_sample.rds"))
final_results <- load_rds_or_null(file.path("data", "final_models.rds"))

ui <- navbarPage(
  "Exploration et classification",
  tabPanel("Données", mod_data_ui("data")),
  tabPanel("Prétraitement", mod_preprocess_ui("preprocess")),
  tabPanel("Exploration", mod_explore_ui("explore")),
  tabPanel("Modélisation", mod_model_ui("model")),
  tabPanel("Étude de cas", mod_case_study_ui("case_study"))
)

server <- function(input, output, session) {
  dataset <- mod_data_server("data", baac_sample)
  preprocess <- mod_preprocess_server("preprocess", dataset)
  mod_explore_server("explore", preprocess$dataset_clean)
  mod_model_server("model", preprocess$dataset_clean, preprocess$recipe)
  mod_case_study_server("case_study", baac_sample, final_results)
}

shinyApp(ui, server)

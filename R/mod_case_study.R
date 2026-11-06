# Tab 5: case study (E13). baac_sample and final_results are read once in app.R
# and are NULL when their .rds file is missing.

mod_case_study_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("status"))
  )
}

mod_case_study_server <- function(id, baac_sample, final_results) {
  moduleServer(id, function(input, output, session) {
    output$status <- renderUI({
      if (is.null(final_results)) {
        p("Les résultats de l'étude ne sont pas disponibles : le fichier data/final_models.rds est absent.")
      } else {
        p("Étude de cas : à venir.")
      }
    })
  })
}

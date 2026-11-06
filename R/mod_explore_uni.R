# Univariate exploration (E8).

mod_explore_uni_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("Analyse univariée : à venir.")
  )
}

mod_explore_uni_server <- function(id, dataset_clean) {
  moduleServer(id, function(input, output, session) {
  })
}

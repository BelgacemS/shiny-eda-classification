# Tab 4: modelling (E10 to E11).

mod_model_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("dims")),
    p("Modélisation : à venir.")
  )
}

mod_model_server <- function(id, dataset_clean, recipe) {
  moduleServer(id, function(input, output, session) {
    output$dims <- renderText({
      req(validate_dataset(dataset_clean()))
      format_dims(dataset_clean()$data)
    })
  })
}

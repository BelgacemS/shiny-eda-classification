# Tab 3: exploration, split into univariate and bivariate sub-modules.

mod_explore_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("dims")),
    tabsetPanel(
      tabPanel("Univariée", mod_explore_uni_ui(ns("uni"))),
      tabPanel("Bivariée", mod_explore_bi_ui(ns("bi")))
    )
  )
}

mod_explore_server <- function(id, dataset_clean) {
  moduleServer(id, function(input, output, session) {
    output$dims <- renderText({
      req(validate_dataset(dataset_clean()))
      format_dims(dataset_clean()$data)
    })

    mod_explore_uni_server("uni", dataset_clean)
    mod_explore_bi_server("bi", dataset_clean)
  })
}

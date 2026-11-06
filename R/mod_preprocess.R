# Tab 2: preprocessing settings. In E2 the data pass through unchanged;
# the settings UI comes in E6b.

mod_preprocess_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("Réglages du prétraitement : à venir.")
  )
}

# Returns list(dataset_clean = reactive(list(data, var_types)), recipe = reactive(<list>)).
mod_preprocess_server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    list(
      dataset_clean = dataset,
      recipe = reactive(default_recipe())
    )
  })
}

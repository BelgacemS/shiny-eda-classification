# Tab 1: data loading. Import options and type editing come in E5.

mod_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Fichier CSV", accept = c(".csv", "text/csv")),
    textOutput(ns("dims"))
  )
}

# Returns dataset(): list(data, var_types), NULL until a file is loaded.
# Data and types live in a single reactiveVal so that they always change together.
mod_data_server <- function(id, baac_sample) {
  moduleServer(id, function(input, output, session) {
    dataset <- reactiveVal(NULL)

    observeEvent(input$file, {
      tryCatch({
        data <- read_table_file(input$file$datapath)
        types <- detect_types(data)
        dataset(make_dataset(data, types))
        possible_id <- attr(types, "possible_id")
        if (length(possible_id) > 0) {
          showNotification(
            paste("Identifiant possible :", paste(possible_id, collapse = ", ")),
            type = "warning"
          )
        }
      }, error = function(e) {
        showNotification(
          paste("Lecture impossible :", conditionMessage(e)),
          type = "error", duration = NULL
        )
      })
    })

    output$dims <- renderText({
      req(validate_dataset(dataset()))
      format_dims(dataset()$data)
    })

    reactive(dataset())
  })
}

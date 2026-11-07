# Tab 1: data loading, import options, variables table and type editing.

SEP_CHOICES <- c("Automatique" = "auto", "Virgule" = ",", "Point-virgule" = ";",
                 "Tabulation" = "tab", "Barre verticale" = "|")

NA_CODE_CHOICES <- c("NA", "N/A", ".", "?", "-1", "0")

TYPE_CHOICES <- c("Quantitative (quanti)" = "quanti", "Qualitative (quali)" = "quali",
                  "Identifiant (id)" = "id", "Ignorée (ignore)" = "ignore")

mod_data_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fileInput(ns("file"), "Fichier CSV ou TSV",
                accept = c(".csv", ".tsv", ".txt", "text/csv", "text/tab-separated-values")),
      selectInput(ns("sep"), "Séparateur", SEP_CHOICES),
      checkboxInput(ns("header"), "Première ligne : noms des colonnes", TRUE),
      selectInput(ns("encoding"), "Encodage",
                  c("UTF-8" = "UTF-8", "Windows-1252 (Latin-1)" = "CP1252")),
      radioButtons(ns("dec"), "Marque décimale", c("Point" = ".", "Virgule" = ","),
                   inline = TRUE),
      selectizeInput(ns("na_codes"), "Codes de valeur manquante", NA_CODE_CHOICES,
                     selected = "NA", multiple = TRUE, options = list(create = TRUE)),
      checkboxInput(ns("na_empty"), "Cellule vide : valeur manquante", TRUE),
      selectizeInput(ns("na_cols"), "Colonnes concernées par les codes (vide : toutes)",
                     choices = NULL, multiple = TRUE),
      helpText("Un code est comparé au texte exact de la cellule. « NA » et la cellule",
               "vide valent pour toutes les colonnes ; les autres codes, pour les",
               "colonnes choisies. Appliqué partout, « 0 » ou « -1 » efface aussi les",
               "vraies valeurs. Ces options ne portent que sur les fichiers, et chaque",
               "changement relit le fichier en gardant les types modifiés à la main."),
      hr(),
      uiOutput(ns("baac_ui"))
    ),
    mainPanel(
      textOutput(ns("dims")),
      tabsetPanel(
        tabPanel(
          "Variables",
          DT::DTOutput(ns("variables")),
          fluidRow(
            column(5, selectizeInput(ns("type_vars"), "Variables à retyper",
                                     choices = NULL, multiple = TRUE)),
            column(4, selectInput(ns("type_new"), "Nouveau type", TYPE_CHOICES)),
            column(3, br(), actionButton(ns("type_apply"), "Changer le type"))
          ),
          helpText("Un changement de type repart des données lues : passer une",
                   "variable en quali puis de nouveau en quanti ne perd aucune valeur.")
        ),
        tabPanel("Aperçu", DT::DTOutput(ns("preview")))
      )
    )
  )
}

# Returns dataset(): list(data, var_types), NULL until a file is loaded.
# Data and types live in a single reactiveVal so that they always change together.
mod_data_server <- function(id, baac_sample) {
  moduleServer(id, function(input, output, session) {
    dataset <- reactiveVal(NULL)
    # Data as read and detected types, to rebuild the dataset when a type changes.
    source_data <- reactiveVal(NULL)
    # "file" or "baac": import options only re-read an uploaded file.
    current_source <- reactiveVal("none")

    import_settings <- reactive({
      sep <- input$sep %||% "auto"
      list(
        sep = if (sep == "tab") "\t" else sep,
        header = input$header %||% TRUE,
        encoding = input$encoding %||% "UTF-8",
        dec = input$dec %||% ".",
        na_codes = c(input$na_codes, if (isTRUE(input$na_empty %||% TRUE)) ""),
        na_cols = input$na_cols
      )
    })

    # Types set by hand on the current data: kept when an import option re-reads the file.
    manual_types <- function() {
      src <- source_data()
      ds <- dataset()
      if (is.null(src) || !validate_dataset(ds)) {
        return(character(0))
      }
      ds$var_types[ds$var_types != src$detected[names(ds$var_types)]]
    }

    # keep: manual types to reapply; one that no longer fits the data is dropped.
    set_source <- function(data, keep = character(0)) {
      types <- detect_types(data)
      source_data(list(data = data, detected = types))
      ds <- make_dataset(data, types)
      keep <- keep[names(keep) %in% names(data)]
      for (type in unique(keep)) {
        ds <- tryCatch(change_types(data, ds$var_types, names(keep)[keep == type], type)$dataset,
                       error = function(e) ds)
      }
      dataset(ds)
      updateSelectizeInput(session, "type_vars", choices = names(data), selected = character(0))
      possible_id <- attr(types, "possible_id")
      if (length(possible_id) > 0) {
        showNotification(
          paste("Identifiant possible :", paste(possible_id, collapse = ", ")),
          type = "warning"
        )
      }
    }

    read_upload <- function(keep_types = FALSE) {
      req(input$file)
      keep <- if (keep_types) manual_types() else character(0)
      tryCatch({
        data <- do.call(read_table_file, c(list(input$file$datapath), import_settings()))
        set_source(data, keep)
        updateSelectizeInput(session, "na_cols", choices = names(data),
                             selected = intersect(input$na_cols, names(data)))
      }, error = function(e) {
        source_data(NULL)
        dataset(NULL)
        showNotification(
          paste("Lecture impossible :", conditionMessage(e)),
          type = "error", duration = NULL
        )
      })
    }

    observeEvent(input$file, {
      current_source("file")
      read_upload()
    })

    observeEvent(import_settings(), {
      if (current_source() == "file") read_upload(keep_types = TRUE)
    }, ignoreInit = TRUE)

    output$baac_ui <- renderUI({
      button <- actionButton(session$ns("baac"), "Charger l'échantillon BAAC")
      if (is.null(baac_sample)) {
        return(tagList(
          tagAppendAttributes(button, disabled = "disabled"),
          helpText("Échantillon BAAC absent : il est produit par data-raw/02_prepare_baac.R.")
        ))
      }
      tagList(button, helpText(paste0("Accidents corporels 2019 à 2024, un usager par ligne : ",
                                      format_dims(baac_sample), ".")))
    })

    observeEvent(input$baac, {
      req(!is.null(baac_sample))
      current_source("baac")
      set_source(baac_sample)
    })

    observeEvent(input$type_apply, {
      src <- source_data()
      ds <- dataset()
      req(src, validate_dataset(ds), length(input$type_vars) > 0)
      tryCatch({
        res <- change_types(src$data, ds$var_types, input$type_vars, input$type_new)
        dataset(res$dataset)
        if (length(res$n_lost) > 0) {
          showNotification(
            paste("Valeurs non numériques remplacées par NA :",
                  paste0(names(res$n_lost), " (", res$n_lost, ")", collapse = ", ")),
            type = "warning"
          )
        }
      }, error = function(e) {
        showNotification(paste("Type inchangé :", conditionMessage(e)), type = "error")
      })
    })

    output$dims <- renderText({
      req(validate_dataset(dataset()))
      format_dims(dataset()$data)
    })

    output$variables <- DT::renderDT({
      ds <- dataset()
      req(validate_dataset(ds), source_data())
      table <- summarize_variables(ds, source_data()$detected)
      DT::formatRound(
        DT::datatable(
          table, rownames = FALSE, selection = "none",
          colnames = c("Variable", "Classe R", "Type détecté", "Type",
                       "Modalités ou valeurs distinctes", "% de NA", "Alerte"),
          options = list(pageLength = 25, scrollX = TRUE)
        ),
        "pct_na", 1
      )
    })

    output$preview <- DT::renderDT({
      ds <- dataset()
      req(validate_dataset(ds))
      DT::datatable(ds$data, rownames = FALSE, selection = "none",
                    options = list(pageLength = 10, scrollX = TRUE))
    })

    reactive(dataset())
  })
}

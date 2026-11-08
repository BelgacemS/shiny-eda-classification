# Tab 4: modelling. E10b: target, predictors, sample, split, recipe fitted on
# the training set, logistic regression and random forest, metrics at
# threshold 0.5 on the internal validation set. The test set is kept for the
# evaluation button of E11a.

SPLIT_MODE_CHOICES <- c("Stratifié" = "stratified", "Groupé par identifiant" = "grouped",
                        "Par variable (test = valeurs choisies)" = "variable")
NO_GROUP <- "(aucun)"

mod_model_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      selectInput(ns("target"), "Variable cible (qualitative)", choices = NULL),
      selectInput(ns("positive"), "Modalité positive (codée 1)", choices = NULL),
      selectizeInput(ns("predictors"), "Variables explicatives", choices = NULL, multiple = TRUE),
      helpText("Les variables id et ignore ne sont pas proposées. Retirer toute variable",
               "dérivée de la cible : elle donnerait une prédiction parfaite et trompeuse."),
      numericInput(ns("sample_size"), "Taille de l'échantillon (lignes)", value = 20000,
                   min = 10, step = 1000),
      selectInput(ns("group"), "Identifiant de groupe", choices = NO_GROUP),
      helpText("Avec un identifiant, l'échantillon garde chaque groupe entier ; choisir le",
               "découpage groupé (ou par variable) pour que le découpage le garde aussi."),
      radioButtons(ns("mode"), "Découpage", SPLIT_MODE_CHOICES),
      conditionalPanel(
        sprintf("input['%s'] == 'variable'", ns("mode")),
        selectInput(ns("by"), "Variable du découpage", choices = NULL),
        selectizeInput(ns("test_values"), "Valeurs mises en test", choices = NULL, multiple = TRUE)
      ),
      sliderInput(ns("test_prop"), "Part du test", min = 0.1, max = 0.5, value = 0.2, step = 0.05),
      sliderInput(ns("valid_prop"), "Part de la validation (dans l'entraînement)",
                  min = 0.1, max = 0.5, value = 0.25, step = 0.05),
      checkboxGroupInput(ns("models"), "Modèles",
                         c("Régression logistique" = "glm", "Forêt aléatoire" = "ranger"),
                         selected = c("glm", "ranger")),
      numericInput(ns("seed"), "Graine", value = 1, min = 1, step = 1),
      actionButton(ns("train"), "Entraîner", class = "btn-primary"),
      width = 4
    ),
    mainPanel(
      textOutput(ns("dims")),
      textOutput(ns("trained_for")),
      h4("Répartition de la cible"),
      tableOutput(ns("distribution")),
      uiOutput(ns("messages")),
      h4("Validation interne, seuil 0,5"),
      helpText("Le seuil de chaque modèle et l'évaluation sur le test arrivent à l'étape suivante.",
               "La recette de l'onglet Prétraitement est ajustée sur l'entraînement seul."),
      tableOutput(ns("metrics")),
      uiOutput(ns("confusions")),
      width = 8
    )
  )
}

mod_model_server <- function(id, dataset_clean, recipe) {
  moduleServer(id, function(input, output, session) {
    output$dims <- renderText({
      req(validate_dataset(dataset_clean()))
      format_dims(dataset_clean()$data)
    })

    columns <- reactive({
      req(validate_dataset(dataset_clean()))
      dataset_clean()$var_types
    })

    observeEvent(columns(), {
      types <- columns()
      data <- dataset_clean()$data
      targets <- names(types)[types == "quali" &
                                vapply(data, function(v) length(unique(stats::na.omit(v))) >= 2, logical(1))]
      keep <- function(current, choices) head(c(intersect(current, choices), choices), 1)
      updateSelectInput(session, "target", choices = targets, selected = keep(input$target, targets))
      groups <- c(NO_GROUP, names(types)[types %in% c("id", "quali")])
      updateSelectInput(session, "group", choices = groups, selected = keep(input$group, groups))
      updateSelectInput(session, "by", choices = names(types), selected = keep(input$by, names(types)))
      updateNumericInput(session, "sample_size", value = min(nrow(data), 20000))
    })

    # The predictors selected by hand are kept while the target is unchanged:
    # a variable removed from the predictors (one derived from the target) must
    # not come back when the preprocessing changes.
    last_target <- reactiveVal(NULL)
    observeEvent(list(input$target, columns()), {
      req(input$target, input$target %in% names(columns()))
      levels <- levels(droplevels(as.factor(dataset_clean()$data[[input$target]])))
      updateSelectInput(session, "positive", choices = levels,
                        selected = head(c(intersect(input$positive, levels), levels[length(levels)]), 1))
      types <- columns()
      candidates <- setdiff(names(types)[types %in% c("quanti", "quali")], input$target)
      same_target <- identical(input$target, last_target()) && !is.null(input$predictors)
      selected <- if (same_target) intersect(input$predictors, candidates) else candidates
      updateSelectizeInput(session, "predictors", choices = candidates, selected = selected)
      last_target(input$target)
    })

    observeEvent(list(input$by, columns()), {
      req(input$by, input$by %in% names(columns()))
      values <- sort(unique(stats::na.omit(as.character(dataset_clean()$data[[input$by]]))))
      updateSelectizeInput(session, "test_values", choices = utils::head(values, 1000),
                           selected = intersect(input$test_values, values), server = TRUE)
    })

    settings <- reactive(list(
      target = input$target,
      positive = input$positive,
      predictors = input$predictors,
      sample_size = input$sample_size,
      group = if (is.null(input$group) || input$group == NO_GROUP) NULL else input$group,
      mode = input$mode %||% "stratified",
      by = input$by,
      test_values = input$test_values,
      test_prop = input$test_prop %||% 0.2,
      valid_prop = input$valid_prop %||% 0.25,
      models = input$models,
      seed = input$seed %||% 1
    ))

    # Results of the last training; cleared when the data or the recipe change,
    # so that results never sit next to data they were not computed on.
    trained <- reactiveVal(NULL)
    observeEvent(list(dataset_clean(), recipe()), trained(NULL), ignoreInit = TRUE)

    observeEvent(input$train, {
      trained(NULL)
      s <- settings()
      problem <- settings_problem(s, dataset_clean())
      if (!is.null(problem)) {
        showNotification(problem, type = "error")
        return()
      }
      result <- tryCatch(
        withProgress(message = "Entraînement des modèles", value = 0.3, {
          train_pipeline(dataset_clean(), recipe(), s)
        }),
        error = function(e) {
          showNotification(paste("Entraînement impossible :", conditionMessage(e)),
                           type = "error", duration = NULL)
          NULL
        }
      )
      trained(result)
    })

    output$trained_for <- renderText({
      out <- trained()
      req(out)
      sprintf("Résultats du dernier entraînement : cible %s, modalité positive « %s ».",
              out$target, out$positive)
    })

    output$distribution <- renderTable({
      out <- trained()
      req(out)
      d <- out$distribution
      data.frame(Partie = d$part, Lignes = d$n,
                 `Positifs (1)` = d$positives,
                 `Part de positifs` = paste0(format_number(100 * d$share, 1), " %"),
                 check.names = FALSE)
    }, align = "lrrr")

    output$messages <- renderUI({
      out <- trained()
      req(out)
      notes <- c(out$messages, unlist(lapply(out$results, `[[`, "messages"), use.names = FALSE))
      if (length(notes) == 0) return(NULL)
      tags$ul(lapply(unique(notes), tags$li))
    })

    output$metrics <- renderTable({
      out <- trained()
      req(out)
      rows <- lapply(names(out$results), function(type) {
        m <- out$results[[type]]$metrics
        data.frame(Modèle = MODEL_LABELS[[type]], Precision = format_metric(m[["precision"]]),
                   Recall = format_metric(m[["recall"]]), F1 = format_metric(m[["f1"]]),
                   check.names = FALSE)
      })
      do.call(rbind, rows)
    }, align = "lrrr")

    output$confusions <- renderUI({
      out <- trained()
      req(out)
      tagList(lapply(names(out$results), function(type) {
        tab <- confusion_table(out$results[[type]]$confusion)
        tagList(
          h5(paste("Matrice de confusion :", MODEL_LABELS[[type]])),
          tags$table(
            class = "table table-condensed", style = "width: auto;",
            tags$thead(tags$tr(tags$th("Observé \\ Prédit"), tags$th("positif"), tags$th("négatif"))),
            tags$tbody(
              tags$tr(tags$th("positif"), tags$td(tab[1, 1]), tags$td(tab[1, 2])),
              tags$tr(tags$th("négatif"), tags$td(tab[2, 1]), tags$td(tab[2, 2]))
            )
          )
        )
      }))
    })
  })
}

# French message when the settings cannot be used, NULL otherwise.
settings_problem <- function(s, dataset) {
  if (!validate_dataset(dataset)) return("Aucune donnée chargée.")
  if (is.null(s$target) || !nzchar(s$target)) return("Choisir une variable cible.")
  if (is.null(s$positive) || !nzchar(s$positive)) return("Choisir la modalité positive.")
  if (length(setdiff(s$predictors, s$target)) == 0) return("Choisir au moins une variable explicative.")
  if (length(s$models) == 0) return("Choisir au moins un modèle.")
  # At least 50 rows, or every row for a smaller file (students.csv has 48).
  minimum <- min(50, nrow(dataset$data))
  if (is.null(s$sample_size) || is.na(s$sample_size) || s$sample_size < minimum) {
    return(sprintf("La taille de l'échantillon doit être d'au moins %d lignes.", minimum))
  }
  if (!s$target %in% names(dataset$data) ||
      !s$positive %in% as.character(dataset$data[[s$target]])) {
    return("La modalité positive n'existe pas dans la cible.")
  }
  if (is.null(s$seed) || is.na(s$seed)) return("Choisir une graine.")
  if (s$mode == "grouped" && is.null(s$group)) return("Le découpage groupé demande un identifiant de groupe.")
  # A stratified split ignores groups: the rows of a group would land on both sides.
  if (s$mode == "stratified" && !is.null(s$group)) {
    return("Avec un identifiant de groupe, choisir le découpage groupé (ou par variable) pour garder chaque groupe entier.")
  }
  if (s$mode == "variable" && length(s$test_values) == 0) {
    return("Choisir les valeurs de la variable à mettre en test.")
  }
  NULL
}

format_metric <- function(x) {
  if (is.na(x)) "non défini" else format_number(x, 3)
}

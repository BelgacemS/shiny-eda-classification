# Tab 2: preprocessing. Two kinds of operations:
# - not learned, applied here to give dataset_clean: removed columns, and
#   optionally removal of incomplete rows;
# - learned (the recipe of R/fct_recipe.R): only settings leave this tab; the
#   modelling tab fits them on the training set. The preview and the export fit
#   the recipe on the whole clean dataset, which the tab says.

OUTLIER_METHODS <- c("Tukey (1,5 × IQR)" = "tukey", "Z-score" = "zscore")
OUTLIER_ACTIONS <- c("Aucune" = "none", "Signaler" = "flag", "Plafonner aux bornes" = "cap",
                     "Supprimer les lignes (entraînement seulement)" = "remove")
IMPUTE_QUANTI <- c("Aucune" = "none", "Médiane" = "median", "Moyenne" = "mean")
IMPUTE_QUALI <- c("Aucune" = "none", "Modalité la plus fréquente" = "mode",
                  "Modalité « Manquant »" = "missing_level")
SCALE_METHODS <- c("Aucune" = "none", "Centrage-réduction" = "standard", "Min-max" = "minmax")
DUMMY_METHODS <- c("Aucune" = "none", "One-hot (k colonnes)" = "one_hot",
                   "k − 1 colonnes (référence : modalité la plus fréquente)" = "k_minus_1")

# Above this number of points, the outlier plot keeps every outlier and a sample of the rest.
MAX_PLOT_POINTS <- 5000

# --- Pure functions ------------------------------------------------------------

# Removes columns, then optionally the rows with a missing value in a quanti or
# quali column (id and ignore columns are never analysed). Nothing is learned.
clean_dataset <- function(dataset, drop_cols = character(0), drop_incomplete = FALSE) {
  keep <- setdiff(names(dataset$data), drop_cols)
  data <- dataset$data[keep]
  var_types <- dataset$var_types[keep]
  if (drop_incomplete) {
    analysed <- names(var_types)[var_types %in% c("quanti", "quali")]
    if (length(analysed) > 0) {
      data <- data[stats::complete.cases(data[analysed]), , drop = FALSE]
      rownames(data) <- NULL
    }
  }
  list(data = data, var_types = var_types)
}

# Recipe settings from the inputs of the tab; NULL inputs keep the defaults.
build_recipe <- function(input) {
  recipe <- default_recipe()
  set <- function(value, current) if (is.null(value) || length(value) == 0 || is.na(value[1])) current else value
  recipe$outliers$method <- set(input$outlier_method, recipe$outliers$method)
  recipe$outliers$z <- set(input$outlier_z, recipe$outliers$z)
  recipe$outliers$action <- set(input$outlier_action, recipe$outliers$action)
  recipe$impute$quanti <- set(input$impute_quanti, recipe$impute$quanti)
  recipe$impute$quali <- set(input$impute_quali, recipe$impute$quali)
  if (!is.null(input$rare_pct) && !is.na(input$rare_pct)) {
    recipe$rare$threshold <- input$rare_pct / 100
  }
  recipe$scale$method <- set(input$scale_method, recipe$scale$method)
  recipe$dummy$method <- set(input$dummy_method, recipe$dummy$method)
  recipe$dummy$max_cols <- set(input$dummy_max_cols, recipe$dummy$max_cols)
  recipe
}

# One row per quanti column: bounds of the chosen rule and number of outliers;
# attr "rows_flagged": rows with at least one outlier, i.e. the rows that the
# "remove" action would drop if the whole dataset were the training set.
outlier_summary <- function(dataset, method = "tukey", z = 3) {
  data <- dataset$data
  cols <- names(dataset$var_types)[dataset$var_types == "quanti"]
  bounds <- lapply(data[cols], outlier_bounds, method = method, z = z)
  flags <- matrix(vapply(cols, function(v) is_outlier(data[[v]], bounds[[v]]),
                         logical(nrow(data))),
                  nrow = nrow(data), ncol = length(cols))  # (n, number of quanti columns)
  n_observed <- vapply(data[cols], function(x) sum(!is.na(x)), integer(1))
  n_outliers <- colSums(flags)
  table <- data.frame(
    variable = cols,
    lower = vapply(bounds, function(b) b[["lower"]], numeric(1)),
    upper = vapply(bounds, function(b) b[["upper"]], numeric(1)),
    n_outliers = as.integer(n_outliers),
    pct_outliers = ifelse(n_observed > 0, 100 * n_outliers / pmax(n_observed, 1), NA_real_),
    row.names = NULL
  )
  attr(table, "rows_flagged") <- sum(rowSums(flags) > 0)
  table
}

# Points of the outlier plot: every outlier, and at most max_points other values.
outlier_plot_data <- function(x, bounds, max_points = MAX_PLOT_POINTS, seed = 1) {
  x <- x[!is.na(x)]
  flag <- is_outlier(x, bounds)
  inside <- which(!flag)
  sampled <- length(inside) > max_points
  if (sampled) {
    set.seed(seed)
    inside <- sample(inside, max_points)
  }
  keep <- c(which(flag), inside)
  out <- data.frame(value = x[keep], outlier = flag[keep])
  attr(out, "sampled") <- sampled
  out
}

# --- Module ----------------------------------------------------------------------

mod_preprocess_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h4("Nettoyage"),
      selectizeInput(ns("drop_cols"), "Colonnes à retirer", choices = NULL, multiple = TRUE),
      checkboxInput(ns("drop_incomplete"), "Supprimer les lignes incomplètes", FALSE),
      helpText("Une ligne est incomplète si une variable quanti ou quali y manque.",
               "Ces deux opérations ne s'apprennent pas : elles valent pour tous les onglets."),
      textOutput(ns("clean_info")),
      hr(),
      h4("Recette"),
      helpText("Réglages appris sur l'entraînement dans l'onglet Modélisation."),
      selectInput(ns("outlier_method"), "Outliers : règle", OUTLIER_METHODS),
      conditionalPanel(
        sprintf("input['%s'] == 'zscore'", ns("outlier_method")),
        numericInput(ns("outlier_z"), "Seuil du z-score", value = 3, min = 1, step = 0.5)
      ),
      selectInput(ns("outlier_action"), "Outliers : traitement", OUTLIER_ACTIONS),
      selectInput(ns("impute_quanti"), "Imputation des variables quanti", IMPUTE_QUANTI),
      selectInput(ns("impute_quali"), "Imputation des variables quali", IMPUTE_QUALI),
      numericInput(ns("rare_pct"), "Modalités rares : seuil (% des valeurs), 0 pour aucun",
                   value = 0, min = 0, max = 50, step = 0.5),
      selectInput(ns("scale_method"), "Normalisation", SCALE_METHODS),
      selectInput(ns("dummy_method"), "Dummification", DUMMY_METHODS),
      numericInput(ns("dummy_max_cols"), "Plafond de colonnes par variable", value = 50,
                   min = 2, step = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Valeurs aberrantes",
          textOutput(ns("outlier_info")),
          DT::DTOutput(ns("outlier_table")),
          helpText("Bornes de Tukey calculées avec les quartiles de quantile(type = 7), ceux",
                   "de geom_boxplot. La slide de C02 calcule les quartiles autrement",
                   "(Q1 = 35 contre 36) et prolonge les moustaches jusqu'aux extrêmes."),
          selectInput(ns("outlier_var"), "Variable affichée", choices = NULL),
          plotOutput(ns("outlier_plot"), height = "320px"),
          textOutput(ns("outlier_plot_note"))
        ),
        tabPanel(
          "Aperçu et export",
          tags$p(tags$strong("Aperçu ajusté sur tout le jeu, la modélisation réajuste sur l'entraînement.")),
          textOutput(ns("preview_info")),
          uiOutput(ns("preview_messages")),
          downloadButton(ns("export"), "Exporter en CSV"),
          br(), br(),
          DT::DTOutput(ns("preview"))
        )
      )
    )
  )
}

# Returns list(dataset_clean = reactive(list(data, var_types)), recipe = reactive(<list>)).
mod_preprocess_server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    observeEvent(dataset(), {
      ds <- dataset()
      cols <- if (validate_dataset(ds)) names(ds$data) else character(0)
      updateSelectizeInput(session, "drop_cols", choices = cols,
                           selected = intersect(input$drop_cols, cols))
    }, ignoreNULL = FALSE)

    # Data and types computed together from the single dataset() reactive.
    dataset_clean <- reactive({
      req(validate_dataset(dataset()))
      clean_dataset(dataset(), input$drop_cols, isTRUE(input$drop_incomplete))
    })

    recipe <- reactive(build_recipe(input))

    output$clean_info <- renderText({
      before <- nrow(dataset()$data)
      after <- nrow(dataset_clean()$data)
      sprintf("%d lignes gardées sur %d (%d retirées).", after, before, before - after)
    })

    quanti_cols <- reactive({
      types <- dataset_clean()$var_types
      names(types)[types == "quanti"]
    })

    observeEvent(quanti_cols(), {
      updateSelectInput(session, "outlier_var", choices = quanti_cols(),
                        selected = intersect(input$outlier_var, quanti_cols())[1])
    })

    outliers <- reactive({
      r <- recipe()
      outlier_summary(dataset_clean(), r$outliers$method, r$outliers$z)
    })

    output$outlier_info <- renderText({
      table <- outliers()
      validate(need(nrow(table) > 0, "Aucune variable quanti."))
      n <- nrow(dataset_clean()$data)
      flagged <- attr(table, "rows_flagged")
      sprintf("%d lignes sur %d (%.1f %%) ont au moins un outlier : « Supprimer » les retirerait si tout le jeu servait d'entraînement.",
              flagged, n, 100 * flagged / max(n, 1))
    })

    output$outlier_table <- DT::renderDT({
      table <- outliers()
      req(nrow(table) > 0)
      DT::formatRound(
        DT::datatable(table, rownames = FALSE, selection = "none",
                      colnames = c("Variable", "Borne basse", "Borne haute", "Outliers",
                                   "% des valeurs"),
                      options = list(pageLength = 10, scrollX = TRUE)),
        c("lower", "upper", "pct_outliers"), 2
      )
    })

    plot_data <- reactive({
      v <- input$outlier_var
      req(v, v %in% quanti_cols())
      r <- recipe()
      x <- dataset_clean()$data[[v]]
      outlier_plot_data(x, outlier_bounds(x, r$outliers$method, r$outliers$z))
    })

    output$outlier_plot <- renderPlot({
      points <- plot_data()
      validate(need(nrow(points) > 0, "Aucune valeur observée pour cette variable."))
      r <- recipe()
      bounds <- outlier_bounds(dataset_clean()$data[[input$outlier_var]],
                               r$outliers$method, r$outliers$z)
      ggplot2::ggplot(points, ggplot2::aes(x = 0, y = value, colour = outlier)) +
        ggplot2::geom_jitter(width = 0.3, height = 0, alpha = 0.5, size = 1.2) +
        ggplot2::geom_hline(yintercept = bounds[is.finite(bounds)], linetype = "dashed") +
        ggplot2::scale_colour_manual(values = c(`FALSE` = "grey55", `TRUE` = "firebrick"),
                                     labels = c(`FALSE` = "non", `TRUE` = "oui"),
                                     name = "Outlier") +
        ggplot2::labs(x = NULL, y = input$outlier_var) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_blank())
    })

    output$outlier_plot_note <- renderText({
      if (isTRUE(attr(plot_data(), "sampled"))) {
        sprintf("Tous les outliers sont affichés, avec %d autres valeurs tirées au hasard.",
                MAX_PLOT_POINTS)
      } else {
        ""
      }
    })

    # Recipe fitted on the whole clean dataset, for the preview and the export only.
    preview_fit <- reactive({
      ds <- dataset_clean()
      tryCatch(fit_recipe(recipe(), ds$data, ds$var_types),
               error = function(e) e)
    })

    output$preview_info <- renderText({
      fitted <- preview_fit()
      if (inherits(fitted, "error")) {
        validate(paste("Recette impossible :", conditionMessage(fitted)))
      }
      n <- nrow(dataset_clean()$data)
      sprintf("%d lignes, %d colonnes après la recette%s.",
              nrow(fitted$train), ncol(fitted$train),
              if (nrow(fitted$train) < n)
                sprintf(" (%d lignes retirées par la suppression des outliers)",
                        n - nrow(fitted$train)) else "")
    })

    output$preview_messages <- renderUI({
      fitted <- preview_fit()
      req(!inherits(fitted, "error"), length(fitted$messages) > 0)
      tags$ul(lapply(fitted$messages, tags$li))
    })

    output$preview <- DT::renderDT({
      fitted <- preview_fit()
      req(!inherits(fitted, "error"))
      data <- fitted$train
      numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
      table <- DT::datatable(data, rownames = FALSE, selection = "none",
                             options = list(pageLength = 10, scrollX = TRUE))
      if (length(numeric_cols) > 0) table <- DT::formatRound(table, numeric_cols, 4)
      table
    })

    output$export <- downloadHandler(
      filename = function() "donnees_pretraitees.csv",
      content = function(file) {
        fitted <- preview_fit()
        req(!inherits(fitted, "error"))
        utils::write.csv(fitted$train, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    list(dataset_clean = dataset_clean, recipe = recipe)
  })
}

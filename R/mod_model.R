# Tab 4: modelling. Target, predictors, sample, split, recipe fitted on the
# training set, logistic regression, random forest and tree (E10b). On the
# internal validation set: ROC, AUC, one threshold slider per model set at the
# maximal F1, comparison table with naive references (E11a). The test set is
# evaluated on a button only, with the thresholds fixed at that moment; the
# importances follow it (E11b). Rebalancing of the training set (E7, E11b).

BALANCE_CHOICES <- c("Aucun" = "none", "Pondération des classes" = "weights",
                     "Sous-échantillonnage" = "under", "Sur-échantillonnage" = "over")

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
      checkboxGroupInput(ns("models"), "Modèles", stats::setNames(names(MODEL_LABELS), MODEL_LABELS),
                         selected = names(MODEL_LABELS)),
      radioButtons(ns("balance"), "Rééquilibrage de l'entraînement", BALANCE_CHOICES),
      conditionalPanel(
        sprintf("input['%s'] == 'under' || input['%s'] == 'over'", ns("balance"), ns("balance")),
        sliderInput(ns("balance_ratio"), "Ratio visé (minorité / majorité)", min = 0.2, max = 1,
                    value = 1, step = 0.1)
      ),
      helpText("Appliqué à l'entraînement seul, après la recette : la validation et le test gardent",
               "la vraie répartition. Ces corrections déplacent surtout les probabilités et donc le seuil,",
               "souvent sans gain d'AUC (van den Goorbergh et al., 2022)."),
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
      h4("Validation interne : seuils et comparaison"),
      helpText("La recette de l'onglet Prétraitement est ajustée sur l'entraînement seul.",
               "Chaque seuil part du F1 maximal sur la validation interne et se règle au curseur ;",
               "un individu est prédit positif quand sa probabilité atteint le seuil.",
               "L'AUC ne dépend pas du seuil : c'est le critère principal de comparaison."),
      uiOutput(ns("thresholds")),
      tableOutput(ns("metrics")),
      plotOutput(ns("roc_valid"), height = "380px"),
      uiOutput(ns("confusions")),
      h4("Test"),
      helpText("Le test n'est évalué qu'à la demande, avec les seuils courants, alors figés :",
               "déplacer un curseur ensuite ne change pas ces résultats. Un nouvel entraînement les efface."),
      fluidRow(
        column(5, numericInput(ns("importance_size"), "Lignes du test pour les importances",
                               value = 1000, min = 100, step = 500)),
        column(4, numericInput(ns("importance_reps"), "Permutations par variable",
                               value = 1, min = 1, max = 5, step = 1))
      ),
      actionButton(ns("evaluate"), "Évaluer sur le test"),
      textOutput(ns("test_info")),
      tableOutput(ns("test_metrics")),
      plotOutput(ns("roc_test"), height = "380px"),
      uiOutput(ns("test_confusions")),
      h4("Importance des variables"),
      helpText("Calculée après « Évaluer sur le test », sur un sous-échantillon du test : baisse d'AUC",
               "quand on permute une variable d'origine (avant la recette). Une variable corrélée à une",
               "autre peut paraître peu importante, l'autre gardant l'information, et la permutation crée",
               "des combinaisons irréalistes (Hooker et al., 2021)."),
      textOutput(ns("importance_info")),
      uiOutput(ns("importance_plot_ui")),
      uiOutput(ns("logistic_importance")),
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
      balance = input$balance %||% "none",
      balance_ratio = input$balance_ratio %||% 1,
      seed = input$seed %||% 1
    ))

    # Results of the last training; cleared when the data or the recipe change,
    # so that results never sit next to data they were not computed on.
    trained <- reactiveVal(NULL)
    observeEvent(list(dataset_clean(), recipe()), trained(NULL), ignoreInit = TRUE)
    # Test results exist only after the button, and go with the training.
    tested <- reactiveVal(NULL)
    importances <- reactiveVal(NULL)
    observeEvent(trained(), {
      tested(NULL)
      importances(NULL)
    }, ignoreNULL = FALSE)
    # Each training gets new slider ids, so that a slider value of a previous
    # training is never read as the threshold of the new models.
    run <- reactiveVal(0)

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
      if (!is.null(result)) {
        run(run() + 1)
        if (length(result$leak) > 0) {
          showNotification(grep("^Liaison quasi parfaite", result$messages, value = TRUE),
                           type = "warning", duration = NULL)
        }
      }
      trained(result)
    })

    slider_id <- function(type) paste0("threshold_", type, "_", run())

    output$thresholds <- renderUI({
      out <- trained()
      req(out)
      tagList(lapply(names(out$results), function(type) {
        sliderInput(session$ns(slider_id(type)), paste("Seuil :", MODEL_LABELS[[type]]),
                    min = 0, max = 1, step = 0.001, value = out$results[[type]]$threshold)
      }))
    })

    # Current thresholds: the slider value once the browser has sent it,
    # otherwise the threshold of maximal F1.
    thresholds <- reactive({
      out <- trained()
      req(out)
      vapply(names(out$results), function(type) {
        value <- input[[slider_id(type)]]
        if (is.null(value) || is.na(value)) out$results[[type]]$threshold else value
      }, numeric(1))
    })

    valid_metrics <- reactive({
      out <- trained()
      req(out)
      thr <- thresholds()
      lapply(stats::setNames(names(out$results), names(out$results)), function(type) {
        r <- out$results[[type]]
        c(threshold_metrics(out$split$valid$y, r$prob_valid, thr[[type]]), list(roc = r$roc))
      })
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
      table <- data.frame(Partie = d$part, Lignes = d$n, `Positifs (1)` = d$positives,
                          `Part de positifs` = paste0(format_number(100 * d$share, 1), " %"),
                          check.names = FALSE)
      b <- out$results[[1]]$balance
      if (!identical(b$method, "none")) {
        label <- names(BALANCE_CHOICES)[BALANCE_CHOICES == b$method]
        balanced <- data.frame(Partie = paste0("Entraînement rééquilibré (", tolower(label), ")"),
                               Lignes = b$n, `Positifs (1)` = b$positives,
                               `Part de positifs` = paste0(format_number(100 * b$share, 1), " %",
                                                           if (b$method == "weights") " des poids"),
                               check.names = FALSE)
        table <- rbind(table[1, ], balanced, table[-1, ])
      }
      table
    }, align = "lrrr")

    output$messages <- renderUI({
      out <- trained()
      req(out)
      notes <- c(out$messages, unlist(lapply(out$results, `[[`, "messages"), use.names = FALSE))
      if (length(notes) == 0) return(NULL)
      tags$ul(lapply(unique(notes), tags$li))
    })

    output$metrics <- renderTable(comparison_table(valid_metrics(), trained()$naive_valid),
                                  align = "lrrrrr")
    output$roc_valid <- renderPlot(roc_plot(valid_metrics(), "Courbes ROC sur la validation interne"))
    output$confusions <- renderUI(confusion_tables(valid_metrics()))

    observeEvent(input$evaluate, {
      # Importances of a previous click never sit next to a new test evaluation.
      importances(NULL)
      out <- trained()
      if (is.null(out)) {
        showNotification("Entraîner les modèles avant d'évaluer le test.", type = "error")
        return()
      }
      result <- tryCatch(
        evaluate_test(out, thresholds()),
        error = function(e) {
          showNotification(paste("Évaluation impossible :", conditionMessage(e)),
                           type = "error", duration = NULL)
          NULL
        }
      )
      tested(result)
      if (is.null(result)) return()
      size <- input$importance_size
      reps <- input$importance_reps
      if (is.null(size) || is.na(size) || size < 100 || is.null(reps) || is.na(reps) ||
          reps < 1 || reps > 5) {
        showNotification("Importances : au moins 100 lignes et de 1 à 5 permutations par variable.",
                         type = "error")
        return()
      }
      started <- proc.time()[["elapsed"]]
      imp <- tryCatch(
        withProgress(message = "Calcul des importances", value = 0.3, {
          compute_importances(out, size = size, n_rep = reps, seed = out$seed)
        }),
        error = function(e) {
          showNotification(paste("Importances impossibles :", conditionMessage(e)),
                           type = "error", duration = NULL)
          NULL
        }
      )
      if (!is.null(imp)) imp$elapsed <- proc.time()[["elapsed"]] - started
      importances(imp)
    })

    output$importance_info <- renderText({
      imp <- importances()
      req(imp)
      sprintf("Calculées sur %d lignes du test, %d permutation%s par variable, en %s s.",
              imp$n_rows, imp$n_rep, if (imp$n_rep > 1) "s" else "", format_number(imp$elapsed, 1))
    })

    output$importance_plot_ui <- renderUI({
      imp <- importances()
      req(imp)
      plotOutput(session$ns("importance_plot"), height = paste0(260 * length(imp$permutation), "px"))
    })
    output$importance_plot <- renderPlot({
      imp <- importances()
      req(imp)
      importance_plot(imp$permutation)
    })

    output$logistic_importance <- renderUI({
      imp <- importances()
      req(imp, imp$logistic)
      tagList(
        h5("Régression logistique : coefficients standardisés et odds ratios"),
        helpText("|β| × s : valeur absolue du coefficient multipliée par l'écart-type (1/n) de la",
                 "colonne dans l'entraînement après la recette. Odds ratios et IC à 95 % (Wald) sur le",
                 "modèle sans pondération ni rééchantillonnage ; ces IC supposent des observations",
                 "indépendantes (faux si plusieurs individus d'un même groupe). « Séparation probable » :",
                 "erreur standard de Wald supérieure à 5, coefficient non interprétable (modalité rare ou",
                 "séparation quasi complète) ; regrouper les modalités rares dans l'onglet Prétraitement."),
        tableOutput(session$ns("logistic_table"))
      )
    })
    output$logistic_table <- renderTable({
      imp <- importances()
      req(imp, imp$logistic)
      logistic_table(imp$logistic)
    }, align = "lrrrrrl")

    output$test_info <- renderText({
      res <- tested()
      req(res)
      paste0("Seuils figés à l'évaluation : ", paste(
        vapply(names(res$results), function(type) {
          paste(MODEL_LABELS[[type]], format_number(res$results[[type]]$threshold, 3))
        }, character(1)), collapse = " ; "), ".")
    })
    output$test_metrics <- renderTable({
      res <- tested()
      req(res)
      comparison_table(res$results, res$naive)
    }, align = "lrrrrr")
    output$roc_test <- renderPlot({
      res <- tested()
      req(res)
      roc_plot(res$results, "Courbes ROC sur le test")
    })
    output$test_confusions <- renderUI({
      res <- tested()
      req(res)
      confusion_tables(res$results)
    })
  })
}

# Comparison table: one row per model (AUC, threshold, precision, recall, F1),
# then the naive references. evaluations: per model, a list with threshold,
# metrics and roc.
comparison_table <- function(evaluations, naive) {
  rows <- lapply(names(evaluations), function(type) {
    e <- evaluations[[type]]
    data.frame(Modèle = MODEL_LABELS[[type]], AUC = format_metric(e$roc$auc),
               Seuil = format_number(e$threshold, 3),
               Precision = format_metric(e$metrics[["precision"]]),
               Recall = format_metric(e$metrics[["recall"]]), F1 = format_metric(e$metrics[["f1"]]),
               check.names = FALSE)
  })
  m <- naive$all_positive$metrics
  rows <- c(rows, list(
    data.frame(Modèle = "Référence : score constant", AUC = format_metric(naive$constant_auc),
               Seuil = "", Precision = "", Recall = "", F1 = "", check.names = FALSE),
    data.frame(Modèle = "Référence : tout positif", AUC = "", Seuil = "",
               Precision = format_metric(m[["precision"]]), Recall = format_metric(m[["recall"]]),
               F1 = format_metric(m[["f1"]]), check.names = FALSE)
  ))
  do.call(rbind, rows)
}

# Superposed ROC curves, with the diagonal of a constant score (AUC 0.5).
roc_plot <- function(evaluations, title) {
  curves <- do.call(rbind, lapply(names(evaluations), function(type) {
    roc <- evaluations[[type]]$roc
    if (length(roc$fpr) == 0) return(NULL)
    data.frame(fpr = roc$fpr, tpr = roc$tpr,
               model = sprintf("%s (AUC %s)", MODEL_LABELS[[type]], format_metric(roc$auc)))
  }))
  req(!is.null(curves))
  curves$model <- factor(curves$model, levels = unique(curves$model))  # order of the table
  ggplot2::ggplot(curves, ggplot2::aes(x = fpr, y = tpr, colour = model)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55") +
    ggplot2::geom_path(linewidth = 0.8) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(title = title, x = "Taux de faux positifs (1 - spécificité)",
                  y = "Taux de vrais positifs (sensibilité, Recall)", colour = NULL) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "bottom", legend.direction = "vertical")
}

# Permutation importances, one panel per model, the n_top largest drops of AUC.
importance_plot <- function(permutation, n_top = 12) {
  data <- do.call(rbind, lapply(names(permutation), function(type) {
    p <- utils::head(permutation[[type]], n_top)
    # One key per model and variable, ordered within each panel.
    data.frame(model = MODEL_LABELS[[type]], variable = p$variable, importance = p$importance,
               sd = p$sd, key = factor(paste(type, p$variable, sep = "\u001f"),
                                       levels = rev(paste(type, p$variable, sep = "\u001f"))))
  }))
  data$model <- factor(data$model, levels = unique(data$model))
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = importance, y = key)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey55") +
    ggplot2::geom_col(fill = "steelblue4", width = 0.7) +
    ggplot2::facet_wrap(~model, ncol = 1, scales = "free_y") +
    ggplot2::scale_y_discrete(labels = function(x) sub("^[^\u001f]*\u001f", "", x)) +
    ggplot2::labs(x = "Baisse d'AUC quand la variable est permutée", y = NULL) +
    ggplot2::theme_minimal(base_size = 13)
  if (any(!is.na(data$sd))) {
    plot <- plot + ggplot2::geom_errorbar(
      ggplot2::aes(xmin = importance - sd, xmax = importance + sd), orientation = "y", width = 0.3,
      colour = "grey30", na.rm = TRUE)
  }
  plot
}

# Logistic coefficients for display: the n_top largest standardized ones.
logistic_table <- function(logistic, n_top = 20) {
  l <- utils::head(logistic, n_top)
  number <- function(x) ifelse(is.na(x), "non défini",
                               formatC(x, format = "g", digits = 3, decimal.mark = ","))
  data.frame(Terme = l$term, `Coefficient β` = number(l$coefficient),
             `|β| × s` = format_number(l$standardized, 3), `Odds ratio` = number(l$odds_ratio),
             `IC 95 % bas` = number(l$lower), `IC 95 % haut` = number(l$upper),
             Remarque = ifelse(!is.na(l$se) & l$se > SEPARATION_SE, "séparation probable", ""),
             check.names = FALSE)
}

# One 2 x 2 confusion table per model.
confusion_tables <- function(evaluations) {
  tagList(lapply(names(evaluations), function(type) {
    e <- evaluations[[type]]
    tab <- confusion_table(e$confusion)
    tagList(
      h5(sprintf("Matrice de confusion : %s (seuil %s)", MODEL_LABELS[[type]],
                 format_number(e$threshold, 3))),
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

# Univariate exploration (E8): the graphs and indicators of the course for one
# quanti or quali variable of dataset_clean, on its non-missing values.

CLASS_RULE_CHOICES <- c("Sturges" = "sturges", "Scott" = "scott", "Freedman-Diaconis" = "fd")
TABLE_MODE_CHOICES <- c("Automatique" = "auto", "Par valeur" = "values", "Par classes" = "classes")

mod_explore_uni_ui <- function(id) {
  ns <- NS(id)
  is_type <- function(type) sprintf("output['%s'] == '%s'", ns("var_type"), type)
  sidebarLayout(
    sidebarPanel(
      selectInput(ns("var"), "Variable", choices = NULL),
      textOutput(ns("excluded")),
      conditionalPanel(
        is_type("quanti"),
        hr(),
        radioButtons(ns("table_mode"), "Tableau statistique", TABLE_MODE_CHOICES),
        helpText("Automatique : par valeur jusqu'à 20 valeurs distinctes (variable",
                 "discrète), par classes au-delà."),
        selectInput(ns("class_rule"), "Nombre de classes", CLASS_RULE_CHOICES),
        radioButtons(ns("hist_scale"), "Histogramme",
                     c("Effectifs" = "count", "Densité de fréquence" = "density"), inline = TRUE),
        checkboxInput(ns("show_density"), "Superposer la courbe de densité", FALSE)
      ),
      width = 3
    ),
    mainPanel(
      conditionalPanel(
        is_type("quanti"),
        tabsetPanel(
          tabPanel("Indicateurs et tableau",
                   h4("Indicateurs de centre et de dispersion"),
                   tableOutput(ns("indicators")),
                   helpText("Variance et écart-type en 1/n. Quartiles de quantile(type = 7),",
                            "ceux de la boîte à moustaches."),
                   h4("Tableau statistique"),
                   DT::DTOutput(ns("quanti_table"))),
          tabPanel("Bâtons et cumulatif",
                   plotOutput(ns("bars"), height = "300px"),
                   plotOutput(ns("ecdf"), height = "300px")),
          tabPanel("Boîte à moustaches",
                   plotOutput(ns("boxplot"), height = "380px"),
                   helpText("Moustaches jusqu'à la dernière valeur à moins de 1,5 × IQR des",
                            "quartiles, points au-delà. La slide de C02 calcule les quartiles",
                            "autrement (Q1 = 35 contre 36 sur les 48 âges) et prolonge les",
                            "moustaches jusqu'aux extrêmes.")),
          tabPanel("Histogramme et densité",
                   plotOutput(ns("histogram"), height = "320px"),
                   plotOutput(ns("cumulative_curve"), height = "280px"))
        )
      ),
      conditionalPanel(
        is_type("quali"),
        tabsetPanel(
          tabPanel("Table des effectifs", DT::DTOutput(ns("quali_table"))),
          tabPanel("Diagramme en colonnes", plotOutput(ns("columns"), height = "380px")),
          tabPanel("Diagramme en secteurs", plotOutput(ns("pie"), height = "420px"))
        )
      ),
      width = 9
    )
  )
}

mod_explore_uni_server <- function(id, dataset_clean) {
  moduleServer(id, function(input, output, session) {
    analysed <- reactive({
      req(validate_dataset(dataset_clean()))
      types <- dataset_clean()$var_types
      names(types)[types %in% c("quanti", "quali")]
    })

    observeEvent(analysed(), {
      choices <- analysed()
      selected <- head(c(intersect(input$var, choices), choices), 1)
      updateSelectInput(session, "var", choices = choices, selected = selected)
    })

    # Values of the chosen variable, missing values included.
    column <- reactive({
      req(input$var, input$var %in% analysed())
      ds <- dataset_clean()
      list(name = input$var, type = ds$var_types[[input$var]], x = ds$data[[input$var]])
    })
    observed <- reactive(column()$x[!is.na(column()$x)])

    output$var_type <- renderText(column()$type)
    outputOptions(output, "var_type", suspendWhenHidden = FALSE)

    output$excluded <- renderText({
      n <- length(column()$x)
      sprintf("%d valeurs analysées, %d manquantes exclues (sur %d lignes).",
              length(observed()), n - length(observed()), n)
    })

    quanti_x <- reactive({
      req(column()$type == "quanti")
      validate(need(length(observed()) > 0, "Aucune valeur observée pour cette variable."))
      observed()
    })

    use_classes <- reactive({
      mode <- input$table_mode %||% "auto"
      mode == "classes" || (mode == "auto" && !is_discrete(quanti_x()))
    })

    class_table <- reactive({
      freq_table_classes(quanti_x(), class_breaks(quanti_x(), input$class_rule %||% "sturges"))
    })

    output$indicators <- renderTable({
      ind <- uni_indicators(quanti_x())
      labels <- c(n = "Effectif", mean = "Moyenne", median = "Médiane", q1 = "Premier quartile",
                  q3 = "Troisième quartile", min = "Minimum", max = "Maximum",
                  range = "Étendue", iqr = "Intervalle interquartile",
                  variance = "Variance (1/n)", sd = "Écart-type (1/n)")
      data.frame(
        Indicateur = unname(labels[names(ind)]),
        Valeur = vapply(names(ind), function(k) {
          if (k == "n") format_count(ind[[k]]) else format_number(ind[[k]], 3)
        }, character(1)),
        check.names = FALSE
      )
    }, align = "lr")

    output$quanti_table <- DT::renderDT({
      if (use_classes()) {
        t <- class_table()
        shown <- data.frame(
          Classe = sprintf("%s%s ; %s]", ifelse(seq_len(nrow(t)) == 1, "[", "]"),
                           format_bound(t$lower), format_bound(t$upper)),
          Centre = t$center, n_i = t$n, N_i = t$N, f_i = 100 * t$f, F_i = 100 * t$F,
          check.names = FALSE
        )
      } else {
        t <- freq_table_values(quanti_x())
        shown <- data.frame(x_i = t$value, n_i = t$n, N_i = t$N, f_i = 100 * t$f,
                            F_i = 100 * t$F, check.names = FALSE)
      }
      table <- DT::datatable(shown, rownames = FALSE, selection = "none",
                             colnames = sub("^f_i$", "f_i (%)", sub("^F_i$", "F_i (%)", names(shown))),
                             options = list(pageLength = 25, scrollX = TRUE))
      value_col <- if (use_classes()) "Centre" else "x_i"
      table <- DT::formatRound(table, value_col, decimals_needed(shown[[value_col]]),
                               mark = " ", dec.mark = ",")
      DT::formatRound(table, c("f_i", "F_i"), 2, mark = " ", dec.mark = ",")
    })

    output$bars <- renderPlot({
      t <- freq_table_values(quanti_x())
      ggplot2::ggplot(t, ggplot2::aes(x = value, y = n)) +
        ggplot2::geom_segment(ggplot2::aes(xend = value, yend = 0), linewidth = 0.8,
                              colour = "steelblue4") +
        ggplot2::labs(title = "Diagramme en bâtons des effectifs", x = column()$name,
                      y = "Effectif") +
        ggplot2::theme_minimal()
    })

    output$ecdf <- renderPlot({
      t <- freq_table_values(quanti_x())
      steps <- data.frame(value = c(t$value[1], t$value), F = c(0, t$F))
      ggplot2::ggplot(steps, ggplot2::aes(x = value, y = F)) +
        ggplot2::geom_step(direction = "hv", colour = "steelblue4") +
        ggplot2::labs(title = "Diagramme cumulatif des fréquences", x = column()$name,
                      y = "Fréquence cumulée") +
        ggplot2::theme_minimal()
    })

    output$boxplot <- renderPlot({
      ggplot2::ggplot(data.frame(value = quanti_x()), ggplot2::aes(x = "", y = value)) +
        ggplot2::geom_boxplot(fill = "grey90", outlier.colour = "firebrick") +
        ggplot2::labs(title = "Boîte à moustaches", x = NULL, y = column()$name) +
        ggplot2::theme_minimal()
    })

    output$histogram <- renderPlot({
      t <- class_table()
      rule <- input$class_rule %||% "sturges"
      density_scale <- identical(input$hist_scale, "density")
      t$height <- if (density_scale) t$density else t$n
      plot <- ggplot2::ggplot(t) +
        ggplot2::geom_rect(ggplot2::aes(xmin = lower, xmax = upper, ymin = 0, ymax = height),
                           fill = "steelblue", colour = "white") +
        ggplot2::labs(title = sprintf("Histogramme, %d classes (%s)", nrow(t),
                                      names(CLASS_RULE_CHOICES)[CLASS_RULE_CHOICES == rule]),
                      x = column()$name,
                      y = if (density_scale) "Densité de fréquence (f_i / a_i)" else "Effectif") +
        ggplot2::theme_minimal()
      if (isTRUE(input$show_density) && length(quanti_x()) > 1) {
        # Kernel density on the same scale: n * a_i * density in counts (equal widths).
        factor <- if (density_scale) 1 else length(quanti_x()) * t$width[1]
        curve <- stats::density(quanti_x())
        plot <- plot + ggplot2::geom_line(
          data = data.frame(x = curve$x, y = curve$y * factor),
          ggplot2::aes(x = x, y = y), colour = "firebrick", linewidth = 0.8
        )
      }
      plot
    })

    output$cumulative_curve <- renderPlot({
      t <- class_table()
      points <- data.frame(bound = c(t$lower[1], t$upper), F = c(0, t$F))
      ggplot2::ggplot(points, ggplot2::aes(x = bound, y = F)) +
        ggplot2::geom_line(colour = "steelblue4") +
        ggplot2::geom_point(colour = "steelblue4") +
        ggplot2::labs(title = "Courbe cumulative (fréquence cumulée à la borne supérieure)",
                      x = column()$name, y = "Fréquence cumulée") +
        ggplot2::theme_minimal()
    })

    quali_table <- reactive({
      req(column()$type == "quali")
      freq_table_levels(column()$x)
    })

    output$quali_table <- DT::renderDT({
      t <- quali_table()
      shown <- data.frame(Modalité = t$level, n_i = t$n, f_i = 100 * t$f, check.names = FALSE)
      DT::formatRound(
        DT::datatable(shown, rownames = FALSE, selection = "none",
                      colnames = c("Modalité", "n_i", "f_i (%)"),
                      options = list(pageLength = 25, scrollX = TRUE)),
        "f_i", 2, mark = " ", dec.mark = ","
      )
    })

    output$columns <- renderPlot({
      t <- quali_table()
      t$level <- factor(t$level, levels = t$level)
      ggplot2::ggplot(t, ggplot2::aes(x = level, y = n)) +
        ggplot2::geom_col(fill = "steelblue") +
        ggplot2::labs(title = "Diagramme en colonnes des effectifs", x = column()$name,
                      y = "Effectif") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    })

    output$pie <- renderPlot({
      t <- quali_table()
      t <- t[t$n > 0, , drop = FALSE]
      validate(need(nrow(t) > 0, "Aucune valeur observée pour cette variable."))
      t$level <- factor(t$level, levels = t$level)
      ggplot2::ggplot(t, ggplot2::aes(x = "", y = n, fill = level)) +
        ggplot2::geom_col(width = 1, colour = "white") +
        ggplot2::coord_polar(theta = "y") +
        ggplot2::labs(title = "Diagramme en secteurs", x = NULL, y = NULL,
                      fill = column()$name) +
        ggplot2::theme_void()
    })
  })
}

# Numbers for the French interface: decimal comma, space between thousands.
format_number <- function(x, digits) {
  formatC(x, format = "f", digits = digits, big.mark = " ", decimal.mark = ",")
}

# Class bounds without trailing zeros (0,8 rather than 0,8000).
format_bound <- function(x) {
  formatC(x, format = "fg", digits = 6, decimal.mark = ",")
}

# Fewest decimals (at most 6) that show every value exactly, for a numeric column
# formatted by DT with a decimal comma.
decimals_needed <- function(x) {
  x <- x[is.finite(x)]
  for (d in 0:6) {
    if (all(abs(round(x, d) - x) < 1e-9)) return(d)
  }
  6
}

format_count <- function(x) {
  formatC(x, format = "d", big.mark = " ")
}

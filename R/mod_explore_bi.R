# Bivariate exploration (E9): graphs and association indices of the course for
# a pair of quanti or quali variables of dataset_clean, on the complete cases
# of the pair, and the table of associations of every variable with a target.

PROFILE_CHOICES <- c("Effectifs empilés" = "stack", "Proportions" = "fill",
                     "Côte à côte" = "dodge")

# Above this number of points, the scatter plots show points taken at regular
# intervals; the indices always use every complete case.
MAX_SCATTER_POINTS <- 5000

mod_explore_bi_ui <- function(id) {
  ns <- NS(id)
  is_pair <- function(pair) sprintf("output['%s'] == '%s'", ns("pair"), pair)
  tabsetPanel(
    tabPanel(
      "Couple de variables",
      sidebarLayout(
        sidebarPanel(
          selectInput(ns("x"), "Variable X", choices = NULL),
          selectInput(ns("y"), "Variable Y", choices = NULL),
          textOutput(ns("pair_info")),
          conditionalPanel(
            is_pair("cc"),
            hr(),
            radioButtons(ns("profile"), "Profils des lignes (X)", PROFILE_CHOICES)
          ),
          width = 3
        ),
        mainPanel(
          conditionalPanel(
            is_pair("qq"),
            tableOutput(ns("indices_qq")),
            helpText("Covariance et variances en 1/n. Droite des moindres carrés y = â x + b̂,",
                     "avec â = s_XY / s_X² et b̂ = ȳ − â x̄."),
            tabsetPanel(
              tabPanel("Nuage et droite de régression",
                       plotOutput(ns("scatter"), height = "380px"),
                       textOutput(ns("scatter_note"))),
              tabPanel("Histogrammes dos à dos", plotOutput(ns("backback"), height = "420px")),
              tabPanel("Nuage et histogrammes marginaux",
                       plotOutput(ns("scatter_hist"), height = "480px"),
                       textOutput(ns("scatter_hist_note")))
            )
          ),
          conditionalPanel(
            is_pair("nc"),
            tableOutput(ns("indices_nc")),
            helpText("Rapport de corrélation c = s_E / s_Y, variances en 1/n : s_E² variance",
                     "des moyennes de groupes (inter), s_R² moyenne des variances de groupes",
                     "(intra), s_E² + s_R² = s_Y²."),
            plotOutput(ns("boxes"), height = "380px"),
            tableOutput(ns("groups"))
          ),
          conditionalPanel(
            is_pair("cc"),
            h4("Table de contingence"),
            tableOutput(ns("contingency")),
            tableOutput(ns("indices_cc")),
            textOutput(ns("chi2_warning")),
            helpText("Φ² = χ² / n, T de Tschuprow = √(Φ² / √((r − 1)(c − 1))),",
                     "C de Cramér = √(Φ² / (min(r, c) − 1))."),
            plotOutput(ns("profiles"), height = "380px"),
            h4("Profils des lignes (%)"),
            tableOutput(ns("profile_table"))
          ),
          width = 9
        )
      )
    ),
    tabPanel(
      "Liaisons avec une cible",
      br(),
      selectInput(ns("target"), "Variable cible", choices = NULL),
      helpText("Indice selon les types : r de Pearson (quanti / quanti, entre −1 et 1),",
               "rapport de corrélation c (quanti / quali, entre 0 et 1), C de Cramér",
               "(quali / quali, entre 0 et 1). Ces indices ne sont pas sur la même échelle :",
               "ne comparer les valeurs qu'à indice égal. Chaque ligne utilise les cas",
               "complets du couple."),
      DT::DTOutput(ns("associations"))
    )
  )
}

mod_explore_bi_server <- function(id, dataset_clean) {
  moduleServer(id, function(input, output, session) {
    analysed <- reactive({
      req(validate_dataset(dataset_clean()))
      types <- dataset_clean()$var_types
      names(types)[types %in% c("quanti", "quali")]
    })

    observeEvent(analysed(), {
      choices <- analysed()
      keep_or <- function(current, default) head(c(intersect(current, choices), default), 1)
      updateSelectInput(session, "x", choices = choices, selected = keep_or(input$x, choices[1]))
      updateSelectInput(session, "y", choices = choices,
                        selected = keep_or(input$y, choices[min(2, length(choices))]))
      updateSelectInput(session, "target", choices = choices,
                        selected = keep_or(input$target, choices[1]))
    })

    # The pair, as complete cases. For a quanti / quali pair, the quanti
    # variable is y and the quali variable gives the groups, whatever the order.
    pair <- reactive({
      req(input$x, input$y, input$x %in% analysed(), input$y %in% analysed())
      validate(need(input$x != input$y, "Choisir deux variables différentes."))
      ds <- dataset_clean()
      types <- ds$var_types[c(input$x, input$y)]
      vars <- c(input$x, input$y)
      code <- if (all(types == "quanti")) "qq" else if (all(types == "quali")) "cc" else "nc"
      if (code == "nc") vars <- c(vars[types == "quali"], vars[types == "quanti"])
      x <- ds$data[[vars[1]]]
      y <- ds$data[[vars[2]]]
      keep <- !is.na(x) & !is.na(y)
      list(code = code, x_name = vars[1], y_name = vars[2], x = x[keep], y = y[keep],
           n_rows = length(keep))
    })

    output$pair <- renderText(pair()$code)
    outputOptions(output, "pair", suspendWhenHidden = FALSE)

    output$pair_info <- renderText({
      p <- pair()
      validate(need(length(p$x) > 0, "Aucune ligne où les deux variables sont renseignées."))
      sprintf("%d cas complets, %d lignes exclues (valeur manquante dans X ou Y).",
              length(p$x), p$n_rows - length(p$x))
    })

    complete_pair <- function(code) {
      p <- pair()
      req(p$code == code)
      validate(need(length(p$x) > 0, "Aucune ligne où les deux variables sont renseignées."))
      p
    }

    # --- quanti / quanti ---------------------------------------------------------

    qq <- reactive({
      p <- complete_pair("qq")
      c(p, list(stats = quanti_quanti(p$x, p$y)))
    })

    thinned <- reactive({
      p <- qq()
      rows <- thin_indices(length(p$x), MAX_SCATTER_POINTS)
      list(x = p$x[rows], y = p$y[rows], sampled = length(rows) < length(p$x))
    })

    output$indices_qq <- renderTable({
      s <- qq()$stats
      data.frame(
        Indice = c("Covariance s_XY (1/n)", "Corrélation linéaire r", "Pente â", "Ordonnée à l'origine b̂"),
        Valeur = format_number(c(s$covariance, s$r, s$slope, s$intercept), 3),
        check.names = FALSE
      )
    }, align = "lr")

    output$scatter <- renderPlot({
      p <- qq()
      t <- thinned()
      plot <- ggplot2::ggplot(data.frame(x = t$x, y = t$y), ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(alpha = 0.5, colour = "steelblue4") +
        ggplot2::labs(x = p$x_name, y = p$y_name) +
        ggplot2::theme_minimal()
      if (is.finite(p$stats$slope)) {
        plot <- plot + ggplot2::geom_abline(slope = p$stats$slope, intercept = p$stats$intercept,
                                            colour = "firebrick", linewidth = 0.8)
      }
      plot
    })

    sample_note <- reactive({
      if (thinned()$sampled) {
        sprintf("Affichage de %d points pris à intervalles réguliers sur %d ; indices et droite calculés sur tous les cas complets.",
                length(thinned()$x), length(qq()$x))
      } else {
        ""
      }
    })
    output$scatter_note <- renderText(sample_note())
    output$scatter_hist_note <- renderText(sample_note())

    output$backback <- renderPlot({
      p <- qq()
      Hmisc::histbackback(p$x, p$y, xlab = c(p$x_name, p$y_name),
                          main = paste(p$x_name, "et", p$y_name), las = 2)
    })

    output$scatter_hist <- renderPlot({
      p <- qq()
      t <- thinned()
      # scatter.with.hist() labels its axes with the names of its arguments.
      env <- list2env(stats::setNames(list(t$x, t$y), c(p$x_name, p$y_name)))
      eval(as.call(list(UsingR::scatter.with.hist, as.name(p$x_name), as.name(p$y_name))),
           envir = env)
    })

    # --- quanti / quali ----------------------------------------------------------

    nc <- reactive({
      p <- complete_pair("nc")
      c(p, list(stats = correlation_ratio(p$y, p$x)))
    })

    output$indices_nc <- renderTable({
      s <- nc()$stats
      data.frame(
        Indice = c("Variance inter s_E²", "Variance intra s_R²", "Variance totale s_Y²",
                   "Rapport de corrélation c"),
        Valeur = format_number(c(s$s_e2, s$s_r2, s$s_y2, s$c), 3),
        check.names = FALSE
      )
    }, align = "lr")

    output$boxes <- renderPlot({
      p <- nc()
      ggplot2::ggplot(data.frame(g = droplevels(as.factor(p$x)), y = p$y),
                      ggplot2::aes(x = g, y = y)) +
        ggplot2::geom_boxplot(fill = "grey90", outlier.colour = "firebrick") +
        ggplot2::labs(title = "Boîtes parallèles", x = p$x_name, y = p$y_name) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    })

    output$groups <- renderTable({
      g <- nc()$stats$groups
      data.frame(Modalité = g$level, Effectif = g$n, Moyenne = format_number(g$mean, 3),
                 `Écart-type (1/n)` = format_number(g$sd, 3), check.names = FALSE)
    }, align = "lrrr")

    # --- quali / quali -----------------------------------------------------------

    cc <- reactive({
      p <- complete_pair("cc")
      tab <- contingency_table(p$x, p$y)
      c(p, list(tab = tab, stats = chi2_indices(tab)))
    })

    output$contingency <- renderTable({
      tab <- stats::addmargins(cc()$tab)
      out <- as.data.frame.matrix(tab)
      rownames(out)[nrow(out)] <- "Total"
      names(out)[ncol(out)] <- "Total"
      out
    }, rownames = TRUE, digits = 0)

    output$indices_cc <- renderTable({
      s <- cc()$stats
      data.frame(
        Indice = c("Khi-deux χ²", "Φ²", "T de Tschuprow", "C de Cramér"),
        Valeur = format_number(c(s$chi2, s$phi2, s$tschuprow, s$cramer), 3),
        check.names = FALSE
      )
    }, align = "lr")

    output$chi2_warning <- renderText({
      s <- cc()$stats
      if (s$n_expected_below_5 > 0) {
        sprintf("%d cellules sur %d ont un effectif théorique inférieur à 5 : le χ² est à interpréter avec prudence.",
                s$n_expected_below_5, s$n_cells)
      } else {
        ""
      }
    })

    output$profiles <- renderPlot({
      p <- cc()
      counts <- as.data.frame(p$tab, stringsAsFactors = FALSE)
      names(counts) <- c("x", "y", "n")
      counts$x <- factor(counts$x, levels = rownames(p$tab))
      counts$y <- factor(counts$y, levels = colnames(p$tab))
      position <- input$profile %||% "stack"
      ggplot2::ggplot(counts, ggplot2::aes(x = x, y = n, fill = y)) +
        ggplot2::geom_col(position = position) +
        ggplot2::labs(title = names(PROFILE_CHOICES)[PROFILE_CHOICES == position],
                      x = p$x_name, y = if (position == "fill") "Proportion" else "Effectif",
                      fill = p$y_name) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    })

    output$profile_table <- renderTable({
      prof <- 100 * row_profiles(cc()$tab)
      out <- as.data.frame.matrix(prof)
      out[] <- lapply(out, format_number, digits = 1)
      out
    }, rownames = TRUE)

    # --- associations with a target --------------------------------------------

    associations <- reactive({
      req(input$target, input$target %in% analysed())
      association_table(dataset_clean(), input$target)
    })

    output$associations <- DT::renderDT({
      t <- associations()
      validate(need(nrow(t) > 0, "Aucune autre variable quanti ou quali."))
      shown <- data.frame(Variable = t$variable, Type = t$type, Indice = t$index,
                          Valeur = t$value, `Cas complets` = t$n, check.names = FALSE)
      DT::formatRound(
        DT::datatable(shown, rownames = FALSE, selection = "none",
                      options = list(pageLength = 25, scrollX = TRUE)),
        "Valeur", 3, mark = " ", dec.mark = ","
      )
    })
  })
}

# At most max_points indices of 1..n, taken at regular intervals (deterministic).
thin_indices <- function(n, max_points) {
  if (n <= max_points) {
    return(seq_len(n))
  }
  unique(round(seq(1, n, length.out = max_points)))
}

# Tab 5: case study of the BAAC data (E13), following PLAN.md, appendix F.
# baac_sample and final_results are read once in app.R and are NULL when their
# .rds file is missing. The descriptive part is computed live on the sample
# (case_study_descriptive.R), the models are read from final_results
# (case_study_models.R). Nothing here drives the inputs of the other tabs.

NO_SAMPLE <- "L'échantillon BAAC (data/accidents_sample.rds) est absent : la partie descriptive n'est pas disponible."
NO_RESULTS <- "Les résultats des modèles (data/final_models.rds) sont absents : la partie modèles n'est pas disponible."

prose <- function(...) div(style = "max-width: 900px;", ...)
pct <- function(x) format_number(x, 1)

# Model names with their article, for the sentences of the tab.
MODEL_NAMES <- c(glm = "la régression logistique", ranger = "la forêt aléatoire", rpart = "l'arbre de décision")
capitalize <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))

mod_case_study_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("status")),
    tabsetPanel(
      tabPanel("Question et démarche", prose(uiOutput(ns("approach")))),
      tabPanel(
        "Q1 Gravité",
        prose(
          h3("Répartition de la gravité"),
          plotOutput(ns("severity_plot"), height = "320px"),
          uiOutput(ns("severity_text")),
          replay_steps("Onglet Données : « Charger l'échantillon BAAC ».",
                       "Onglet Exploration, Univariée : Variable grav, sous-onglets « Table des effectifs » et « Diagramme en colonnes »."),
          h3("Évolution de 2019 à 2024"),
          plotOutput(ns("year_plot"), height = "340px"),
          tableOutput(ns("year_table")),
          textOutput(ns("year_indices")),
          uiOutput(ns("year_text")),
          tableOutput(ns("full_years")),
          do.call(replay_steps, c(replay_load(), list(replay_pair("an"))))
        )
      ),
      tabPanel(
        "Q2 Usagers vulnérables",
        prose(
          h3("Catégorie d'usager"),
          plotOutput(ns("catu_plot"), height = "320px"),
          tableOutput(ns("catu_table")),
          textOutput(ns("catu_indices")),
          uiOutput(ns("catu_text")),
          do.call(replay_steps, c(replay_load(retype = FALSE), list(replay_pair("catu")))),
          h3("Véhicule de l'usager"),
          plotOutput(ns("catv_plot"), height = "400px"),
          tableOutput(ns("catv_table")),
          textOutput(ns("catv_indices")),
          uiOutput(ns("catv_text")),
          do.call(replay_steps, c(replay_load(retype = FALSE),
                                  list(replay_pair("catv_groupe"),
                                       "Piétons par véhicule : même chose avec Variable Y catu, table de contingence.")))
        )
      ),
      tabPanel(
        "Q3 Conditions",
        prose(
          h3("Liaison de chaque condition avec la gravité"),
          tableOutput(ns("q3_table")),
          helpText("C de Cramér pour une variable quali (entre 0 et 1), rapport de corrélation c pour",
                   "l'âge (entre 0 et 1, autre indice) : ne comparer qu'à indice égal."),
          do.call(replay_steps, c(replay_load(), list(
            "Onglet Exploration, Bivariée, « Liaisons avec une cible » : Variable cible grave."))),
          uiOutput(ns("q3_text")),
          h3("Détail d'une condition"),
          selectInput(ns("q3_var"), "Condition", Q3_VARS),
          plotOutput(ns("q3_plot"), height = "380px"),
          tableOutput(ns("q3_detail")),
          textOutput(ns("q3_indices")),
          uiOutput(ns("q3_replay"))
        )
      ),
      tabPanel(
        "Modèles",
        prose(
          h3("Comparaison sur 2024, spécification principale"),
          tableOutput(ns("test_table")),
          uiOutput(ns("test_text")),
          plotOutput(ns("roc_plot"), height = "420px"),
          tableOutput(ns("difference_table")),
          uiOutput(ns("confusions")),
          h3("Pondération contre aucun rééquilibrage"),
          tableOutput(ns("balance_table")),
          uiOutput(ns("balance_text")),
          h3("Ablation : variables du choc"),
          tableOutput(ns("ablation_table")),
          uiOutput(ns("ablation_text")),
          h3("Variables les plus importantes"),
          uiOutput(ns("importance_text")),
          plotOutput(ns("importance_plot"), height = "780px"),
          h4("Régression logistique : coefficients standardisés et odds ratios"),
          tableOutput(ns("logistic_table")),
          uiOutput(ns("logistic_text")),
          h3("Cohérence avec la partie descriptive"),
          uiOutput(ns("coherence_text")),
          h3("Refaire les modèles dans l'app (approché)"),
          uiOutput(ns("model_replay"))
        )
      ),
      tabPanel("Limites et conclusions", prose(uiOutput(ns("limits"))))
    )
  )
}

mod_case_study_server <- function(id, baac_sample, final_results) {
  moduleServer(id, function(input, output, session) {
    output$status <- renderUI({
      notes <- c(if (is.null(baac_sample)) NO_SAMPLE, if (is.null(final_results)) NO_RESULTS)
      if (length(notes) == 0) return(NULL)
      div(class = "alert alert-warning", lapply(notes, p))
    })

    sample_ds <- reactive({
      validate(need(!is.null(baac_sample), NO_SAMPLE))
      case_study_dataset(baac_sample)
    })
    final <- reactive({
      validate(need(!is.null(final_results), NO_RESULTS))
      check_final_results(final_results)
      final_results
    })
    profiles <- reactive({
      vars <- c("an", "catu", "catv_groupe", setdiff(Q3_VARS, "age"))
      stats::setNames(lapply(vars, function(v) grave_profile(sample_ds(), v)), vars)
    })
    # Contingency table of two variables of the sample, complete cases.
    crossed <- function(x, y) contingency_table(sample_ds()$data[[x]], sample_ds()$data[[y]])

    # --- Question and approach ----------------------------------------------------

    output$approach <- renderUI({
      sample_line <- if (!is.null(baac_sample)) {
        sprintf(paste("Échantillon de l'app : 7 %% des accidents de chaque année, tirés entiers avec une",
                      "graine fixe, soit %s usagers et %s accidents. Les chiffres des onglets Q1 à Q3 en",
                      "sont calculés en direct, par les fonctions des onglets Exploration et Modélisation :",
                      "les réglages donnés sous chaque figure les redonnent à l'identique."),
                format_count(nrow(baac_sample)), format_count(length(unique(baac_sample$Num_Acc))))
      }
      models <- if (!is.null(final_results)) {
        m <- final_results$meta
        vars <- final_results$specs$main$vars
        grid <- m$settings_grid
        values <- function(settings, name) paste(unique(vapply(settings, `[[`, numeric(1), name)), collapse = ", ")
        tagList(
          h3("Démarche des modèles"),
          tags$ul(
            tags$li(sprintf(paste("Découpage par année, jamais au hasard : entraînement %d-%d (%s usagers),",
                                  "choix des réglages et du seuil sur %d (%s usagers), une seule évaluation",
                                  "sur %d (%s usagers, %s accidents). Le test ressemble à l'usage réel :",
                                  "prédire une année jamais vue."),
                            min(m$train_years), max(m$train_years), format_count(m$n_train), m$valid_year,
                            format_count(m$n_valid), m$test_year, format_count(m$n_test),
                            format_count(m$n_test_accidents))),
            tags$li(paste("Unité de tirage : l'accident. Ses usagers partagent ses conditions et ne sont pas",
                          "indépendants : ils ne sont jamais séparés entre deux parties, et les intervalles",
                          sprintf("de confiance tirent des accidents (bootstrap par grappes, %s tirages).",
                                  format_count(m$boot_draws)))),
            tags$li(sprintf(paste("Liste blanche de %d variables (data-raw/model_vars.R), fixée avant tout modèle",
                                  "d'après le guide de codification et la stabilité du codage de 2019 à 2023 : une",
                                  "variable par information (catv_groupe et non catv), ni l'année (un effet année",
                                  "appris sur 2019-2022 serait extrapolé à 2024), ni identifiant, ni coordonnées :"),
                            length(vars)),
                    tags$code(paste(vars, collapse = ", "))),
            tags$li(paste("Ablation : col, choc, obs et obsm décrivent le choc lui-même (type de collision, point",
                          "de choc, obstacle), pas les conditions de l'accident. Ils sont absents du modèle",
                          "principal et ajoutés dans une variante, pour mesurer ce qu'ils apportent.")),
            tags$li(sprintf(paste("Trois modèles : régression logistique, forêt aléatoire (%d arbres, mtry dans {%s},",
                                  "taille minimale des nœuds dans {%s}) et arbre de décision (cp dans {%s}). Chaque",
                                  "réglage est essayé sans rééquilibrage et avec pondération des classes ; le",
                                  "meilleur est choisi sur l'AUC de %d, et le seuil au F1 maximal sur %d."),
                            m$ranger_trees, values(grid$ranger, "mtry"), values(grid$ranger, "min.node.size"),
                            values(grid$rpart, "cp"), m$valid_year, m$valid_year)),
            tags$li(sprintf(paste("Prétraitement appris sur l'entraînement seul : imputation par la médiane et la",
                                  "modalité la plus fréquente, modalités de moins de %s %% regroupées."),
                            format_number(100 * final_results$specs$main$recipe$rare_threshold, 1))),
            tags$li(paste("Les modèles sont entraînés une fois, hors de l'app, par data-raw/03_final_models.R sur",
                          "toute la table : l'app n'a ni la table complète ni la mémoire nécessaire. L'onglet",
                          "Modèles lit leurs résultats."))
          )
        )
      }
      tagList(
        h3("Question"),
        p(paste("Quels facteurs sont associés à la gravité de l'accident pour une personne impliquée ? Un",
                "usager est grave s'il est tué ou blessé hospitalisé : la variable grave vaut « oui » quand",
                "grav vaut « Tué » ou « Blessé hospitalisé »." )),
        h3("Données"),
        tags$ul(
          tags$li(paste("Bases annuelles des accidents corporels de la circulation routière (BAAC), ONISR,",
                        "data.gouv.fr, Licence Ouverte. Quatre tables par an (caractéristiques, lieux,",
                        "véhicules, usagers), jointes au niveau de l'usager : une ligne par personne impliquée,",
                        "avec son véhicule, son accident et le lieu.")),
          tags$li(paste("Période 2019 à 2024 : la qualification « blessé hospitalisé » n'est pas comparable avant",
                        "et après 2018, on ne remonte donc pas avant 2019.")),
          tags$li(sprintf(paste("France métropolitaine, %s usagers. Les usagers en fuite sont exclus : leur gravité",
                                "n'est pas observée mais vaut « indemne » dans les fichiers."),
                          format_count(sum(FULL_TABLE_YEARS$users)))),
          tags$li(paste("Recodages : -1 et cellules vides deviennent des valeurs manquantes ; âge, jour de la",
                        "semaine, mois et tranche horaire calculés ; catégorie de véhicule regroupée",
                        "(catv_groupe) ; les trois cases secu1 à secu3, qui listent les équipements sans ordre,",
                        "remplacées par six indicateurs (ceinture, casque, dispositif enfants, gilet, airbag,",
                        "gants) et un indicateur d'équipement indéterminé.")),
          if (!is.null(sample_line)) tags$li(sample_line)
        ),
        models
      )
    })

    # --- Q1 -------------------------------------------------------------------------

    severity <- reactive(severity_table(sample_ds()))
    output$severity_plot <- renderPlot(severity_plot(severity()))
    output$severity_text <- renderUI({
      s <- severity()
      f <- stats::setNames(100 * s$f, s$level)
      p(sprintf(paste("Dans l'échantillon, %s %% des usagers sont indemnes, %s %% blessés légers, %s %% blessés",
                      "hospitalisés et %s %% tués : %s %% sont graves. La classe positive est minoritaire, ce",
                      "qui compte pour lire les métriques des modèles (onglet Modèles)."),
                pct(f[["Indemne"]]), pct(f[["Blessé léger"]]), pct(f[["Blessé hospitalisé"]]), pct(f[["Tué"]]),
                pct(f[["Blessé hospitalisé"]] + f[["Tué"]])))
    })

    output$year_plot <- renderPlot(profile_plot(profiles()$an, "an"))
    output$year_table <- renderTable(profile_table(profiles()$an), align = "lrrr")
    output$year_indices <- renderText(profile_indices(profiles()$an))
    output$year_text <- renderUI({
      s <- profiles()$an$share
      y <- FULL_TABLE_YEARS
      drop_2020 <- 100 * (1 - y$users[y$an == 2020] / y$users[y$an == 2019])
      tagList(
        p(sprintf(paste("Dans l'échantillon, la part de graves va de %s %% (%s) à %s %% (%s), et le lien avec",
                        "l'année est presque nul (C de Cramér = %s). Sur la table complète (tableau ci-dessous),",
                        "elle varie moins, de %s %% à %s %% : une partie des écarts de l'échantillon vient du",
                        "tirage de 7 %% des accidents."),
                  pct(min(s)), names(s)[which.min(s)], pct(max(s)), names(s)[which.max(s)],
                  format_number(profiles()$an$stats$cramer, 3), pct(min(y$grave_pct)), pct(max(y$grave_pct)))),
        p(sprintf(paste("En 2020, le nombre d'usagers impliqués baisse de %s %% par rapport à 2019, effet probable",
                        "des confinements, mais la part de graves ne baisse pas (%s %% contre %s %%) : moins",
                        "d'accidents, pas des accidents moins graves."),
                  pct(drop_2020), format_number(y$grave_pct[y$an == 2020], 2),
                  format_number(y$grave_pct[y$an == 2019], 2)))
      )
    })
    output$full_years <- renderTable({
      req(sample_ds())
      data.frame(Année = as.character(FULL_TABLE_YEARS$an), `Usagers (table complète)` = format_count(FULL_TABLE_YEARS$users),
                 `Part de graves` = paste(format_number(FULL_TABLE_YEARS$grave_pct, 2), "%"), check.names = FALSE)
    }, align = "lrr", caption = "Table complète : sortie de data-raw/02_prepare_baac.R, hors de l'app.")

    # --- Q2 -------------------------------------------------------------------------

    output$catu_plot <- renderPlot(profile_plot(profiles()$catu, "catu"))
    output$catu_table <- renderTable(profile_table(profiles()$catu), align = "lrrr")
    output$catu_indices <- renderText(profile_indices(profiles()$catu))
    output$catu_text <- renderUI({
      pr <- profiles()$catu
      p(sprintf("Un piéton est grave dans %s %% des cas, contre %s %% pour un conducteur et %s %% pour un passager.",
                pct(share_of(pr, "Piéton")), pct(share_of(pr, "Conducteur")), pct(share_of(pr, "Passager"))))
    })

    output$catv_plot <- renderPlot(profile_plot(profiles()$catv_groupe, "catv_groupe"))
    output$catv_table <- renderTable(profile_table(profiles()$catv_groupe), align = "lrrr")
    output$catv_indices <- renderText(profile_indices(profiles()$catv_groupe))
    output$catv_text <- renderUI({
      pr <- profiles()$catv_groupe
      by_user <- crossed("catv_groupe", "catu")
      tagList(
        p(sprintf(paste("Les usagers de deux-roues motorisés de plus de 125 cm³ sont les plus souvent graves (%s %%),",
                        "devant ceux de 50 cm³ ou moins (%s %%), de 50 à 125 cm³ (%s %%) et les cyclistes (%s %%) ;",
                        "trottinettes et autres engins de déplacement personnels (EDP) : %s %%. Les usagers",
                        "rattachés à un véhicule léger (VL) sont graves dans %s %% des cas, ceux d'un poids lourd",
                        "dans %s %%. Le véhicule est l'une des conditions les plus liées à la gravité",
                        "(C de Cramér = %s)."),
                  pct(share_of(pr, "2RM/3RM > 125 cm3")), pct(share_of(pr, "2RM/3RM <= 50 cm3")),
                  pct(share_of(pr, "2RM/3RM 50 à 125 cm3")), pct(share_of(pr, "Vélo")), pct(share_of(pr, "EDP")),
                  pct(share_of(pr, "VL")), pct(share_of(pr, "PL, tracteur routier")),
                  format_number(pr$stats$cramer, 3))),
        p(sprintf(paste("Attention à la lecture : d'après le guide de codification, un piéton est rattaché au",
                        "véhicule qui l'a heurté. La ligne VL mêle donc les occupants des voitures et %s piétons",
                        "heurtés par une voiture, sur %s usagers. Pour comparer les usagers vulnérables aux",
                        "occupants de VL, il faut lire les deux tableaux ensemble ; le modèle, lui, tient compte",
                        "des deux variables à la fois (onglet Modèles)."),
                  format_count(by_user["VL", "Piéton"]), format_count(sum(by_user["VL", ]))))
      )
    })

    # --- Q3 -------------------------------------------------------------------------

    output$q3_table <- renderTable(q3_associations(sample_ds()), align = "lllrr")
    output$q3_text <- renderUI({
      pr <- profiles()
      belt_user <- crossed("secu_ceinture", "catu")
      belt_unknown <- crossed("secu_ceinture", "secu_indetermine")
      light_area <- 100 * row_profiles(crossed("lum", "agg"))
      age <- age_profile(sample_ds())
      age_mean <- stats::setNames(age$groups$mean, age$groups$level)
      or_belt <- if (!is.null(final_results)) odds_ratio_of(final_results$specs$main, "secu_ceinturenon")
      vma <- pr$vma
      tagList(
        p(tags$strong("Ceinture. "), sprintf(paste(
          "Sans ceinture, %s %% de graves, contre %s %% avec. Mais « non » ne veut pas dire « ceinture non",
          "bouclée » : l'indicateur vaut « non » pour tout usager sans ceinture déclarée, donc aussi pour les",
          "piétons (%s des %s usagers à « non »), les cyclistes et les usagers de deux-roues, et pour les",
          "équipements indéterminés (%s). Le lien brut mêle l'équipement et la catégorie d'usager.%s"),
          pct(share_of(pr$secu_ceinture, "non")), pct(share_of(pr$secu_ceinture, "oui")),
          format_count(belt_user["non", "Piéton"]), format_count(sum(belt_user["non", ])),
          format_count(belt_unknown["non", "oui"]),
          if (!is.null(or_belt)) sprintf(paste(" La régression logistique, qui tient compte de ces variables à la",
                                               "fois, garde pour « ceinture non » un odds ratio de %s (onglet Modèles)."),
                                         format_number(or_belt[["estimate"]], 1)) else "")),
        p(tags$strong("Route et agglomération. "), sprintf(paste(
          "Hors agglomération, %s %% de graves, contre %s %% en agglomération. Les routes départementales sont",
          "les plus touchées (%s %%), les autoroutes parmi les moins (%s %%) malgré leur vitesse, ce qui",
          "s'accorde avec des chaussées séparées et sans piétons ; voies communales : %s %%."),
          pct(share_of(pr$agg, "Hors agglomération")), pct(share_of(pr$agg, "En agglomération")),
          pct(share_of(pr$catr, "Route départementale")), pct(share_of(pr$catr, "Autoroute")),
          pct(share_of(pr$catr, "Voie communale")))),
        p(tags$strong("Vitesse maximale autorisée. "), sprintf(paste(
          "La part de graves n'augmente pas régulièrement avec la vitesse autorisée : %s %% à 30 km/h, %s %% à 50,",
          "%s %% à 80, %s %% à 90, %s %% à 110 et %s %% à 130. La vitesse autorisée décrit surtout le type de",
          "route : 80 km/h est la limite par défaut des routes hors agglomération à double sens sans séparateur",
          "central depuis juillet 2018. Son C de Cramér (%s) est en partie gonflé par des valeurs rares : %d cellules sur %d ont un",
          "effectif théorique inférieur à 5."),
          pct(share_of(vma, "30")), pct(share_of(vma, "50")), pct(share_of(vma, "80")), pct(share_of(vma, "90")),
          pct(share_of(vma, "110")), pct(share_of(vma, "130")), format_number(vma$stats$cramer, 3),
          vma$stats$n_expected_below_5, vma$stats$n_cells)),
        p(tags$strong("Luminosité. "), sprintf(paste(
          "De nuit sans éclairage public, %s %% de graves, contre %s %% en plein jour et %s %% de nuit avec",
          "éclairage allumé. La nuit sans éclairage se trouve surtout hors agglomération (%s %% des cas) :",
          "luminosité et lieu vont ensemble."),
          pct(share_of(pr$lum, "Nuit sans éclairage public")), pct(share_of(pr$lum, "Plein jour")),
          pct(share_of(pr$lum, "Nuit avec éclairage public allumé")),
          pct(light_area["Nuit sans éclairage public", "Hors agglomération"]))),
        p(tags$strong("Météo. "), sprintf(paste(
          "Elle est peu liée à la gravité (C de Cramér = %s) : %s %% de graves par temps normal, %s %% sous",
          "pluie légère ; les situations rares (brouillard, vent fort) ont des parts plus hautes mais peu",
          "d'usagers."),
          format_number(pr$atm$stats$cramer, 3), pct(share_of(pr$atm, "Normale")), pct(share_of(pr$atm, "Pluie légère")))),
        p(tags$strong("Âge. "), sprintf(paste(
          "Les usagers graves sont un peu plus âgés en moyenne (%s ans contre %s), mais le lien est faible",
          "(rapport de corrélation c = %s)."),
          format_number(age_mean[["oui"]], 1), format_number(age_mean[["non"]], 1), format_number(age$c, 3))),
        replay_steps(paste("Tableaux croisés cités : onglet Exploration, Bivariée, Variable X secu_ceinture et",
                           "Variable Y catu (puis secu_indetermine) ; Variable X lum et Variable Y agg,",
                           "« Profils des lignes (X) » Proportions."))
      )
    })

    output$q3_plot <- renderPlot({
      req(input$q3_var)
      if (input$q3_var == "age") age_plot(sample_ds()) else profile_plot(profiles()[[input$q3_var]], input$q3_var)
    })
    output$q3_detail <- renderTable({
      req(input$q3_var)
      if (input$q3_var == "age") {
        g <- age_profile(sample_ds())$groups
        data.frame(grave = g$level, Usagers = format_count(g$n), `Âge moyen` = format_number(g$mean, 1),
                   `Écart-type (1/n)` = format_number(g$sd, 1), check.names = FALSE)
      } else {
        profile_table(profiles()[[input$q3_var]])
      }
    }, align = "lrrr")
    output$q3_indices <- renderText({
      req(input$q3_var)
      if (input$q3_var == "age") {
        a <- age_profile(sample_ds())
        sprintf("%s cas complets. Rapport de corrélation c = %s.", format_count(a$n), format_number(a$c, 3))
      } else {
        profile_indices(profiles()[[input$q3_var]])
      }
    })
    output$q3_replay <- renderUI({
      req(input$q3_var)
      v <- input$q3_var
      last <- if (v == "age") {
        "Onglet Exploration, Bivariée, « Couple de variables » : Variable X age, Variable Y grave (boîtes parallèles et tableau des groupes)."
      } else {
        replay_pair(v)
      }
      do.call(replay_steps, c(replay_load(retype = v == "vma"), list(last)))
    })

    # --- Models -----------------------------------------------------------------------

    main_eval <- reactive(model_evaluations(final()$specs$main))
    output$test_table <- renderTable(test_table(final(), "main"), align = "llrrrrr")
    output$test_text <- renderUI({
      f <- final()
      ev <- main_eval()
      auc <- vapply(ev, function(e) e$roc$auc, numeric(1))
      best <- names(which.max(auc))
      tagList(
        p(sprintf(paste("%s a la meilleure AUC sur %d (%s), devant %s. Au seuil choisi sur %d, %s retrouve",
                        "%s %% des usagers graves (Recall) et %s %% des usagers qu'elle signale le sont",
                        "(Precision). Tous les modèles dépassent nettement la référence « tout positif »",
                        "(F1 = %s) : la prédiction est réelle mais modérée, les conditions de l'accident ne",
                        "suffisent pas à dire qui sera grave."),
                  capitalize(MODEL_NAMES[[best]]), f$meta$test_year, format_number(auc[[best]], 3),
                  paste(vapply(setdiff(names(sort(auc, decreasing = TRUE)), best), function(t) {
                    sprintf("%s (%s)", MODEL_NAMES[[t]], format_number(auc[[t]], 3))
                  }, character(1)), collapse = " et "),
                  f$meta$valid_year, MODEL_NAMES[[best]], pct(100 * ev[[best]]$metrics[["recall"]]),
                  pct(100 * ev[[best]]$metrics[["precision"]]), format_metric(f$naive_test$all_positive[["f1"]]))),
        p(sprintf(paste("Les seuils ont été choisis sur %d et appliqués tels quels à %d : rien n'est rechoisi",
                        "sur le test. Les intervalles viennent de %s tirages des accidents de %d avec remise."),
                  f$meta$valid_year, f$meta$test_year, format_count(f$meta$boot_draws), f$meta$test_year))
      )
    })
    output$roc_plot <- renderPlot(roc_plot(main_eval(), sprintf("Courbes ROC sur %d", final()$meta$test_year)))
    output$difference_table <- renderTable(model_difference_table(final(), "main"), align = "lr")
    output$confusions <- renderUI(confusion_tables(main_eval()))

    output$balance_table <- renderTable(balance_table(final(), "main"), align = "lrrrrrrr")
    output$balance_text <- renderUI({
      f <- final()
      diff <- function(type) interval_of(f, paste(variant_name("main", type, "weights"), "-",
                                                  variant_name("main", type, "none")))
      t <- f$specs$main$test
      thr <- function(type, b) t$threshold[t$model == type & t$balance == b]
      p(sprintf(paste("Pour la logistique et la forêt, la pondération ne change presque pas l'AUC (écarts de",
                      "%s et %s) : l'AUC ne dépend que de l'ordre des scores, et la pondération déplace surtout",
                      "les probabilités, donc le seuil (%s contre %s pour la logistique). Choisir le seuil sur %d",
                      "fait le même travail (van den Goorbergh et al., 2022). L'arbre gagne davantage (%s),",
                      "parce que les poids changent ses coupures. Les probabilités des modèles pondérés sont",
                      "décalées vers le haut : elles servent à classer, pas à lire un risque."),
                format_interval(diff("glm"), 4), format_interval(diff("ranger"), 4),
                format_number(thr("glm", "weights"), 3), format_number(thr("glm", "none"), 3),
                f$meta$valid_year, format_interval(diff("rpart"), 4)))
    })

    output$ablation_table <- renderTable(ablation_table(final()), align = "lrrr")
    output$ablation_text <- renderUI({
      f <- final()
      main <- chosen_balance(f$specs$main)
      abl <- chosen_balance(f$specs$ablation)
      gains <- vapply(names(MODEL_LABELS), function(type) {
        interval_of(f, paste(variant_name("ablation", type, abl[[type]]), "-",
                             variant_name("main", type, main[[type]])))[["estimate"]]
      }, numeric(1))
      p(sprintf(paste("Ajouter col, choc, obs et obsm augmente l'AUC de %s à %s selon le modèle. Ces variables",
                      "décrivent l'accident lui-même : elles améliorent la prédiction, mais ne répondent pas à",
                      "la question, qui porte sur les conditions de l'accident. Le modèle principal reste la",
                      "référence de l'étude."),
                format_number(min(gains), 3), format_number(max(gains), 3)))
    })

    output$importance_text <- renderUI({
      f <- final()
      imp <- f$specs$main$importance
      top <- vapply(imp, function(d) d$variable[1], character(1))
      forest <- imp$ranger
      p(sprintf(paste("Importance par permutation : baisse d'AUC sur %d quand on mélange une variable, calculée",
                      "sur %s usagers tirés par accidents entiers, %d permutations. La variable la plus importante",
                      "est %s pour %s (baisse de %s pour la forêt). Suivent, pour la forêt : %s."),
                f$meta$test_year, format_count(f$meta$importance_users), f$meta$importance_reps,
                paste(unique(top), collapse = ", "),
                if (length(unique(top)) == 1) "les trois modèles" else "les modèles",
                format_number(forest$importance[1], 3), paste(forest$variable[2:6], collapse = ", ")))
    })
    output$importance_plot <- renderPlot(importance_plot(final()$specs$main$importance))
    output$logistic_table <- renderTable(logistic_table(final()$specs$main$logistic, n_top = 15), align = "lrrrrrl")
    output$logistic_text <- renderUI({
      spec <- final()$specs$main
      or <- function(term) format_interval(odds_ratio_of(spec, term), 2)
      p(sprintf(paste("|β| × s : coefficient de la logistique pondérée multiplié par l'écart-type (1/n) de sa",
                      "colonne. Odds ratios et IC à 95 %% (Wald) sur la logistique sans rééquilibrage, à autres",
                      "variables égales et par rapport à la modalité la plus fréquente de l'entraînement : ceinture",
                      "non (contre oui) %s, piéton (contre conducteur) %s, hors agglomération %s, autoroute",
                      "(contre route départementale) %s. Ces",
                      "IC supposent les usagers indépendants, ce qui est faux dans un même accident : ils sont",
                      "trop étroits."),
                or("secu_ceinturenon"), or("catuPiéton"), or("aggHors agglomération"), or("catrAutoroute")))
    })

    output$coherence_text <- renderUI({
      f <- final()
      forest <- f$specs$main$importance$ranger
      rank_of <- function(v) match(v, forest$variable)
      p(sprintf(paste("Les conditions les plus liées à la gravité dans la partie descriptive (équipement,",
                      "véhicule, agglomération, vitesse autorisée, type de route) sont aussi parmi les plus",
                      "importantes pour la forêt (rangs %d, %d, %d, %d et %d sur %d), et la météo, peu liée,",
                      "n'y arrive qu'au rang %d. Le modèle ajoute ce que les tableaux croisés ne donnent pas :",
                      "à catégorie d'usager, véhicule et route égaux, l'absence de ceinture déclarée reste",
                      "associée à des chances d'être grave multipliées par %s."),
                rank_of("secu_ceinture"), rank_of("catv_groupe"), rank_of("agg"), rank_of("vma"),
                rank_of("catr"), nrow(forest), rank_of("atm"),
                format_number(odds_ratio_of(f$specs$main, "secu_ceinturenon")[["estimate"]], 1)))
    })

    output$model_replay <- renderUI({
      vars <- final()$specs$main$vars
      removed <- if (!is.null(baac_sample)) replay_removed(sample_ds(), vars)
      tagList(
        replay_steps(
          "Onglet Données : « Charger l'échantillon BAAC ».",
          "Onglet Prétraitement : « Modalités rares : seuil » 0,5 ; garder l'imputation par défaut (médiane, modalité la plus fréquente).",
          "Onglet Modélisation : « Variable cible » grave, « Modalité positive » oui.",
          tagList("« Variables explicatives » : retirer ",
                  if (!is.null(removed)) tags$code(paste(removed, collapse = ", ")) else "toute variable hors de la liste blanche",
                  paste(". Il reste les", length(vars), "variables de la liste blanche. grav en particulier : c'est",
                        "la variable dont la cible est tirée ; qualitative, elle est cochée par défaut, et la garder",
                        "donne une prédiction presque parfaite et trompeuse, que l'alerte de fuite de l'onglet",
                        "Modélisation signale. an sert au découpage et n'est pas une variable de l'étude.")),
          "« Identifiant de groupe » Num_Acc ; « Découpage » Par variable, « Variable du découpage » an, « Valeurs mises en test » 2024.",
          "« Rééquilibrage de l'entraînement » Pondération des classes ; « Entraîner », puis « Évaluer sur le test »."
        ),
        p(paste("Les chiffres seront proches, pas identiques : l'app tire 20 000 lignes de l'échantillon au lieu",
                "de toute la table, apprend sur 2019-2023 avec une validation interne au lieu de 2019-2022 avec",
                "2023, garde les réglages par défaut de la forêt et de l'arbre, n'applique pas les recodages de",
                "data-raw/model_vars.R (vosp, catr, trajet, circ), regroupe les modalités rares de toutes les",
                "variables quali et calcule les importances sur 1 000 lignes du test par défaut."))
      )
    })

    # --- Limits and conclusions ---------------------------------------------------

    output$limits <- renderUI({
      f <- final()
      spec <- f$specs$main
      grid <- spec$grid
      chosen <- spec$test[spec$test$chosen, , drop = FALSE]
      cp_values <- vapply(f$meta$settings_grid$rpart, `[[`, numeric(1), "cp")
      tree_cp <- as.numeric(sub("cp = ", "", chosen$setting[chosen$model == "rpart"]))
      forest_auc <- range(grid$auc[grid$model == "ranger"])
      ev <- main_eval()
      tagList(
        h3("Limites"),
        tags$ul(
          tags$li(paste("Indicateur officiel : la qualification « blessé hospitalisé » n'est plus labellisée par",
                        "l'autorité de la statistique publique depuis 2019, et n'est pas comparable avant et après",
                        "2018. La cible en dépend.")),
          tags$li(paste("Données administratives : seuls les accidents corporels constatés par les forces de",
                        "l'ordre sont enregistrés. Une partie des accidents n'y figure pas (sous-déclaration), ce",
                        "qui peut déformer les comparaisons entre catégories d'usagers.")),
          tags$li(paste("Échantillon : la partie descriptive porte sur 7 % des accidents ; une part de graves y",
                        "varie d'environ un point d'une année à l'autre par le seul tirage. Les modèles, eux, ont",
                        "utilisé toute la table.")),
          tags$li(paste("Dépendance entre usagers d'un même accident : prise en compte par le bootstrap par",
                        "grappes pour les AUC, pas par les IC de Wald des odds ratios, trop étroits. Les",
                        "intervalles mesurent la variabilité de l'échantillon de 2024 à modèles fixés, pas celle",
                        "du choix des réglages ni d'un changement d'année.")),
          tags$li(paste("Importance par permutation : avec des variables corrélées (ceinture, catégorie",
                        "d'usager et véhicule), l'importance se partage entre elles, et la permutation crée des",
                        "combinaisons irréalistes, comme un piéton avec ceinture (Hooker et al., 2021).")),
          tags$li("Association n'est pas causalité : les tableaux croisés et les odds ratios décrivent des liens, pas des effets."),
          tags$li(paste("Codage : le changement de codage de secu3 en 2024 a été vu à la préparation des données,",
                        "sur les variables explicatives et sans les étiquettes ; les indicateurs d'équipement",
                        "reposent sur le guide de codification. La part de valeurs manquantes de vma passe de",
                        "1,5 % à 3,5 % en 2023, imputées par la médiane de l'entraînement.")),
          tags$li(sprintf(paste("Réglages : le meilleur cp de l'arbre (%s) %s ; la forêt est peu sensible à ses",
                                "réglages (AUC de la grille entre %s et %s sur %d)."),
                          format(tree_cp, scientific = TRUE),
                          if (isTRUE(all.equal(tree_cp, min(cp_values)))) {
                            "est la plus petite valeur de la grille : un arbre plus profond ferait peut-être mieux"
                          } else "est à l'intérieur de la grille",
                          format_number(forest_auc[1], 3), format_number(forest_auc[2], 3), f$meta$valid_year)),
          tags$li("Calibration : les probabilités des modèles pondérés sont décalées ; elles ne se lisent pas comme un risque."),
          tags$li(paste("Les données 2025, publiées par l'ONISR le 30 octobre 2026 après le début de l'étude, ne",
                        "sont pas utilisées : elles permettraient un second test temporel."))
        ),
        h3("Conclusions"),
        tags$ul(
          tags$li(paste("Les usagers sans carrosserie (piétons, deux-roues motorisés, cyclistes) sont nettement",
                        "plus souvent graves que les occupants de véhicules légers.")),
          tags$li(paste("Les conditions les plus liées à la gravité sont l'équipement de sécurité, le véhicule et",
                        "le réseau : hors agglomération, routes départementales, nuit sans éclairage. La météo",
                        "compte peu.")),
          tags$li(sprintf(paste("Sur %d, la forêt aléatoire (AUC %s) fait un peu mieux que la régression",
                                "logistique (%s), elle-même devant l'arbre (%s) ; F1 de %s à %s, contre %s pour",
                                "« tout positif »."),
                          f$meta$test_year, format_number(ev$ranger$roc$auc, 3), format_number(ev$glm$roc$auc, 3),
                          format_number(ev$rpart$roc$auc, 3),
                          format_number(min(vapply(ev, function(e) e$metrics[["f1"]], numeric(1))), 3),
                          format_number(max(vapply(ev, function(e) e$metrics[["f1"]], numeric(1))), 3),
                          format_metric(f$naive_test$all_positive[["f1"]]))),
          tags$li(paste("Le rééquilibrage n'améliore pas l'AUC de la logistique ni de la forêt une fois le seuil",
                        "choisi sur la validation. Les variables du choc améliorent la prédiction, mais décrivent",
                        "l'accident plutôt que ses conditions."))
        )
      )
    })
  })
}

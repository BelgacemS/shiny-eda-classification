# Tab 5, descriptive part (E13): computed live on the deployed BAAC sample with
# the functions of tabs 1 to 3 (make_dataset, change_types, clean_dataset,
# contingency_table, chi2_indices, row_profiles, correlation_ratio,
# association_table), so that each figure is replayed exactly in the app.

CASE_STUDY_TARGET <- "grave"
CASE_STUDY_POSITIVE <- "oui"

# Integer codes detected as quanti, retyped as quali in tab 1 before the
# replay: one profile per year and per speed limit.
CASE_STUDY_RETYPED <- c("an", "vma")

# Question Q3 (PLAN.md, appendix F): conditions of the accident.
Q3_VARS <- c(
  "Équipement : ceinture" = "secu_ceinture",
  "Vitesse maximale autorisée" = "vma",
  "Type de route" = "catr",
  "Agglomération" = "agg",
  "Luminosité" = "lum",
  "Conditions atmosphériques" = "atm",
  "Âge" = "age"
)

# Meaning of the BAAC columns quoted in the tab, from the coding guide
# (data-raw/raw/description-des-bases-de-donnees-annuelles.pdf) and the
# variables built by data-raw/02_prepare_baac.R. Short label for plots and
# sentences, description for the table of the variables.
VARIABLE_LABELS <- c(
  grave = "usager grave", grav = "gravité en quatre niveaux", an = "année",
  catu = "catégorie d'usager", sexe = "sexe", age = "âge", trajet = "motif du déplacement",
  secu_ceinture = "ceinture", secu_casque = "casque", secu_enfant = "dispositif enfants",
  secu_gilet = "gilet réfléchissant", secu_airbag = "airbag", secu_gants = "gants",
  secu_indetermine = "équipement indéterminé", catv_groupe = "catégorie de véhicule",
  manv = "manœuvre avant l'accident", mois = "mois", jour_semaine = "jour de la semaine",
  tranche_horaire = "tranche horaire", lum = "luminosité", agg = "agglomération", int = "intersection",
  atm = "conditions atmosphériques", catr = "type de route", circ = "régime de circulation",
  vosp = "voie réservée", prof = "profil de la route", plan = "tracé en plan", surf = "état de la surface",
  infra = "aménagement", situ = "situation de l'accident", vma = "vitesse maximale autorisée",
  col = "type de collision", choc = "point de choc", obs = "obstacle fixe heurté",
  obsm = "obstacle mobile heurté",
  Num_Acc = "identifiant de l'accident", id_vehicule = "identifiant du véhicule",
  id_usager = "identifiant de l'usager", place = "place dans le véhicule", locp = "localisation du piéton",
  actp = "action du piéton", etatp = "piéton seul ou accompagné", catv = "catégorie de véhicule détaillée",
  senc = "sens de circulation", motor = "motorisation", occutc = "occupants d'un transport en commun",
  dep = "département", nbv = "nombre de voies", lartpc = "largeur du terre-plein central",
  larrout = "largeur de la chaussée", lat = "latitude", long = "longitude"
)
VARIABLE_DESCRIPTIONS <- c(
  grave = "« oui » si l'usager est tué ou blessé hospitalisé, « non » s'il est indemne ou blessé léger : la cible",
  grav = "gravité de blessure : indemne, blessé léger, blessé hospitalisé, tué (source de la cible, jamais variable explicative)",
  an = "année de l'accident (sert au découpage, jamais variable explicative)",
  catu = "conducteur, passager ou piéton",
  sexe = "sexe de l'usager",
  age = "âge de l'usager, année de l'accident moins année de naissance",
  trajet = "motif du déplacement au moment de l'accident (domicile-travail, loisirs...)",
  secu_ceinture = "« oui » si une ceinture est déclarée, « non » sinon (piétons, deux-roues et équipements indéterminés compris)",
  secu_casque = "casque utilisé",
  secu_enfant = "dispositif pour enfants utilisé", secu_gilet = "gilet réfléchissant utilisé",
  secu_airbag = "airbag de deux- ou trois-roues utilisé", secu_gants = "gants de deux- ou trois-roues utilisés",
  secu_indetermine = "équipement de sécurité non déterminable ou non renseigné",
  catv_groupe = "catégorie du véhicule de l'usager, en 13 classes (véhicule léger, vélo, deux-roues motorisés par cylindrée, poids lourd...) ; un piéton est rattaché au véhicule qui l'a heurté",
  manv = "manœuvre principale du véhicule avant l'accident",
  mois = "mois de l'accident", jour_semaine = "jour de la semaine de l'accident",
  tranche_horaire = "tranche horaire de l'accident",
  lum = "conditions d'éclairage : plein jour, crépuscule, nuit avec ou sans éclairage public",
  agg = "accident en ou hors agglomération",
  int = "type d'intersection (hors intersection, en X, en T, giratoire...)",
  atm = "conditions atmosphériques : normale, pluie, neige, brouillard, vent fort...",
  catr = "catégorie de route : autoroute, nationale, départementale, voie communale...",
  circ = "régime de circulation : sens unique, double sens, chaussées séparées, voies d'affectation variable ; « Non renseigné » ajouté pour les modèles",
  vosp = "existence d'une voie réservée (piste ou bande cyclable, voie réservée)",
  prof = "profil en long : plat, pente, sommet ou bas de côte",
  plan = "tracé en plan : partie rectiligne ou courbe",
  surf = "état de la surface : normale, mouillée, verglacée...",
  infra = "aménagement ou infrastructure : pont, souterrain, carrefour aménagé, chantier...",
  situ = "situation de l'accident : sur chaussée, accotement, trottoir, piste cyclable...",
  vma = "vitesse maximale autorisée sur le lieu et au moment de l'accident, en km/h",
  col = "type de collision : frontale, par l'arrière, par le côté, en chaîne...",
  choc = "point de choc initial sur le véhicule",
  obs = "obstacle fixe heurté (arbre, glissière, poteau...)",
  obsm = "obstacle mobile heurté (piéton, véhicule, animal...)",
  Num_Acc = "identifiant de l'accident : garde les usagers d'un même accident ensemble",
  id_vehicule = "identifiant du véhicule", id_usager = "identifiant de l'usager",
  place = "place occupée dans le véhicule ; écartée, presque un détail de catu",
  locp = "localisation du piéton ; écartée, sans objet pour tout autre usager, donc redondante avec catu",
  actp = "action du piéton ; écartée pour la même raison que locp",
  etatp = "piéton seul ou accompagné ; écartée pour la même raison que locp",
  catv = "catégorie du véhicule détaillée ; écartée, regroupée dans catv_groupe",
  senc = "sens de circulation par rapport aux repères de la route ; écartée, décrit la route et non l'accident",
  motor = "motorisation du véhicule ; écartée, part de « Inconnue » instable d'une année à l'autre",
  occutc = "nombre d'occupants d'un transport en commun ; écartée, manquante pour tous les autres véhicules",
  dep = "département de l'accident ; écartée, localise l'accident (96 modalités)",
  nbv = "nombre total de voies de circulation ; écartée, les valeurs manquantes changent de sens en 2023",
  lartpc = "largeur du terre-plein central, en m ; écartée, manquante pour presque tous les usagers",
  larrout = "largeur de la chaussée, en m ; écartée, manquante pour la plupart des usagers et codage changé en 2023",
  lat = "latitude de l'accident ; écartée, identifie le lieu exact",
  long = "longitude de l'accident ; écartée, identifie le lieu exact"
)

# Quantitative columns of the models (the others are qualitative), columns of
# the sample left out of the models (data-raw/model_vars.R: model_excluded,
# plus the coordinates), and identifiers.
QUANTITATIVE_VARS <- c("age", "vma")
EXCLUDED_VARS <- c("place", "locp", "actp", "etatp", "catv", "senc", "motor", "occutc", "dep", "nbv",
                   "lartpc", "larrout", "lat", "long")
IDENTIFIER_VARS <- c("Num_Acc", "id_vehicule", "id_usager")
EXCLUDED_QUANTITATIVE <- c("occutc", "nbv", "lartpc", "larrout", "lat", "long")

# "label (code)", for axis titles and replay steps.
variable_label <- function(code) {
  stopifnot(all(code %in% names(VARIABLE_LABELS)))
  sprintf("%s (%s)", unname(VARIABLE_LABELS[code]), code)
}

# Table of every column of the sample: code, meaning, type, role. Without the
# results (main_vars empty), the variables of the models are listed without
# saying which specification uses them.
variable_glossary <- function(main_vars, ablation_vars) {
  fixed <- c(CASE_STUDY_TARGET, "grav", "an")
  others <- setdiff(names(VARIABLE_DESCRIPTIONS), c(fixed, EXCLUDED_VARS, IDENTIFIER_VARS))
  model <- unique(c(main_vars, ablation_vars, others))
  codes <- c(fixed, model, EXCLUDED_VARS, IDENTIFIER_VARS)
  role <- rep(if (length(main_vars) == 0) "variable des modèles" else "hors des modèles", length(codes))
  role[codes %in% setdiff(ablation_vars, main_vars)] <- "ablation seulement"
  role[codes %in% main_vars] <- "modèle principal"
  role[codes %in% EXCLUDED_VARS] <- "écartée des modèles"
  role[codes %in% IDENTIFIER_VARS] <- "identifiant"
  role[codes %in% c("grav", "an")] <- "jamais dans les modèles"
  role[codes == CASE_STUDY_TARGET] <- "cible"
  type <- ifelse(codes %in% c(QUANTITATIVE_VARS, EXCLUDED_QUANTITATIVE), "quantitative", "qualitative")
  type[codes %in% c(IDENTIFIER_VARS, "dep")] <- "identifiant"
  type[codes == "an"] <- "quantitative (qualitative pour Q1)"
  type[codes == "vma"] <- "quantitative (qualitative pour Q3)"
  data.frame(Code = codes, Signification = unname(VARIABLE_DESCRIPTIONS[codes]), Type = type,
             Rôle = role, check.names = FALSE)
}

# Share of the accidents of each year drawn into the app sample
# (data-raw/02_prepare_baac.R, sample_fraction).
SAMPLE_FRACTION <- 0.07

# Share of missing vma by year (%) in the full table, 2019 to 2023, the shift
# described in data-raw/model_vars.R.
VMA_NA_PCT <- c(`2019` = 1.50, `2020` = 1.52, `2021` = 1.50, `2022` = 1.53, `2023` = 3.50)

# Users and share of grave by year in the full table (692 784 users), printed by
# data-raw/02_prepare_baac.R ("Share of grave by year"). The sample holds 7 % of
# the accidents of each year.
FULL_TABLE_YEARS <- data.frame(
  an = 2019:2024,
  users = c(126352L, 99169L, 119281L, 117027L, 115917L, 115038L),
  grave_pct = c(18.09, 18.29, 17.43, 18.19, 18.30, 18.28)
)

# The dataset of tabs 1 and 2 after the replay steps: BAAC sample loaded, an
# and vma retyped as quali, nothing removed.
case_study_dataset <- function(sample) {
  types <- detect_types(sample)
  ds <- make_dataset(sample, types)
  ds <- change_types(sample, ds$var_types, CASE_STUDY_RETYPED, "quali")$dataset
  clean_dataset(ds)
}

# Profile of the target along a quali variable, as tab 3 computes it for the
# pair (X = var, Y = grave): contingency table of the complete cases, indices,
# share of grave in each level (%).
grave_profile <- function(ds, var) {
  tab <- contingency_table(ds$data[[var]], ds$data[[CASE_STUDY_TARGET]])
  list(var = var, tab = tab, stats = chi2_indices(tab),
       share = 100 * row_profiles(tab)[, CASE_STUDY_POSITIVE])
}

# Share of grave (%) of one level of a profile.
share_of <- function(profile, level) {
  stopifnot(level %in% names(profile$share))
  unname(profile$share[[level]])
}

profile_table <- function(profile) {
  tab <- profile$tab
  data.frame(Modalité = rownames(tab), Usagers = format_count(rowSums(tab)),
             Graves = format_count(tab[, CASE_STUDY_POSITIVE]),
             `Part de graves` = paste(format_number(profile$share, 1), "%"),
             check.names = FALSE)
}

profile_indices <- function(profile) {
  s <- profile$stats
  sprintf("%s cas complets. χ² = %s, C de Cramér = %s%s.", format_count(s$n),
          format_number(s$chi2, 1), format_number(s$cramer, 3),
          if (s$n_expected_below_5 > 0) {
            sprintf(" (%d cellules sur %d ont un effectif théorique inférieur à 5)",
                    s$n_expected_below_5, s$n_cells)
          } else "")
}

# Row profiles in proportions, the "Proportions" graph of tab 3, with the share
# of grave written on each bar.
profile_plot <- function(profile) {
  counts <- as.data.frame(profile$tab, stringsAsFactors = FALSE)
  names(counts) <- c("x", "y", "n")
  counts$x <- factor(counts$x, levels = rownames(profile$tab))
  counts$y <- factor(counts$y, levels = colnames(profile$tab))
  shares <- data.frame(x = factor(rownames(profile$tab), levels = rownames(profile$tab)),
                       share = profile$share)
  ggplot2::ggplot(counts, ggplot2::aes(x = x, y = n, fill = y)) +
    ggplot2::geom_col(position = "fill") +
    ggplot2::geom_text(data = shares, ggplot2::aes(x = x, y = share / 100,
                                                   label = format_number(share, 1)),
                       inherit.aes = FALSE, vjust = -0.4, size = 3.5) +
    ggplot2::scale_fill_manual(values = c(non = "grey80", oui = "firebrick")) +
    ggplot2::labs(x = variable_label(profile$var), y = "Proportion", fill = variable_label(CASE_STUDY_TARGET),
                  caption = "Nombre au-dessus de la barre rouge : part de graves (%).") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
}

# Q1: distribution of the four levels of grav, the column chart of tab 3.
severity_table <- function(ds) {
  freq_table_levels(ds$data$grav)
}

severity_plot <- function(table) {
  table$level <- factor(table$level, levels = table$level)
  ggplot2::ggplot(table, ggplot2::aes(x = level, y = n)) +
    ggplot2::geom_col(fill = "steelblue4", width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = paste(format_number(100 * f, 1), "%")),
                       vjust = -0.4, size = 3.8) +
    ggplot2::labs(x = variable_label("grav"), y = "Effectif") +
    ggplot2::theme_minimal(base_size = 13)
}

# Q3: index of association of each condition with grave, from the table of
# tab 3 ("Liaisons avec une cible"): C de Cramér for a qualitative variable,
# correlation ratio c for age.
q3_associations <- function(ds) {
  table <- association_table(ds, CASE_STUDY_TARGET)
  table <- table[match(Q3_VARS, table$variable), , drop = FALSE]
  data.frame(Variable = Q3_VARS, Libellé = names(Q3_VARS), Indice = table$index,
             Valeur = format_number(table$value, 3), `Cas complets` = format_count(table$n),
             check.names = FALSE, row.names = NULL)
}

# Age by grave: correlation ratio and parallel boxplots, as tab 3 shows them
# for the pair (age, grave).
age_profile <- function(ds) {
  correlation_ratio(ds$data$age, ds$data[[CASE_STUDY_TARGET]])
}

age_plot <- function(ds) {
  keep <- !is.na(ds$data$age) & !is.na(ds$data[[CASE_STUDY_TARGET]])
  data <- data.frame(g = droplevels(ds$data[[CASE_STUDY_TARGET]][keep]), y = ds$data$age[keep])
  ggplot2::ggplot(data, ggplot2::aes(x = g, y = y)) +
    ggplot2::geom_boxplot(fill = "grey90", outlier.colour = "firebrick") +
    ggplot2::labs(x = variable_label(CASE_STUDY_TARGET), y = variable_label("age")) +
    ggplot2::theme_minimal(base_size = 13)
}

# Steps to replay a figure in tabs 1 to 4, shown under the figure.
replay_steps <- function(...) {
  tags$div(
    class = "well well-sm", style = "margin-top: 10px;",
    tags$strong("Pour refaire dans l'app"),
    tags$ol(style = "margin-bottom: 0;", lapply(list(...), tags$li))
  )
}

# Common first steps: load the sample, retype an and vma.
replay_load <- function(retype = TRUE) {
  steps <- list("Onglet Données : « Charger l'échantillon BAAC ».")
  if (retype) {
    steps <- c(steps, list(paste(
      "Onglet Données, tableau Variables : « Variables à retyper » an (année) et vma (vitesse maximale",
      "autorisée), « Nouveau type » Qualitative (quali), « Changer le type ». Codées par des nombres, elles",
      "sont lues comme quantitatives ; en qualitatives, elles donnent un profil par année et par vitesse.")))
  }
  steps
}

replay_pair <- function(x, y = CASE_STUDY_TARGET) {
  sprintf(paste("Onglet Exploration, Bivariée, « Couple de variables » : Variable X %s (%s),",
                "Variable Y %s (%s), « Profils des lignes (X) » Proportions."),
          x, VARIABLE_LABELS[[x]], y, VARIABLE_LABELS[[y]])
}

# Term of the logistic regression for display: "label (code) : level", or
# "label (code)" for a quantitative variable.
pretty_term <- function(term, vars) {
  vars <- vars[order(-nchar(vars))]
  vapply(term, function(t) {
    v <- vars[startsWith(t, vars)][1]
    if (is.na(v)) return(t)
    level <- substring(t, nchar(v) + 1)
    if (nzchar(level)) sprintf("%s : %s", variable_label(v), level) else variable_label(v)
  }, character(1), USE.NAMES = FALSE)
}

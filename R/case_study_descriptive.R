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
profile_plot <- function(profile, label) {
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
    ggplot2::labs(x = label, y = "Proportion", fill = CASE_STUDY_TARGET,
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
    ggplot2::labs(x = "grav", y = "Effectif") +
    ggplot2::theme_minimal(base_size = 13)
}

# Q3: index of association of each condition with grave, from the table of
# tab 3 ("Liaisons avec une cible"): C de Cramér for a quali variable,
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
    ggplot2::labs(x = CASE_STUDY_TARGET, y = "age") +
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
      "Onglet Données, tableau Variables : « Variables à retyper » an et vma, « Nouveau type »",
      "Qualitative (quali), « Changer le type ».")))
  }
  steps
}

replay_pair <- function(x, y = CASE_STUDY_TARGET) {
  sprintf(paste("Onglet Exploration, Bivariée, « Couple de variables » : Variable X %s,",
                "Variable Y %s, « Profils des lignes (X) » Proportions."), x, y)
}

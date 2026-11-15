# Variables of the final BAAC models (PLAN.md, step E12a).
# Fixed before any model is fitted. The decisions below rest on the coding
# guide and on the stability table of 2019 to 2023 printed by
# 03_final_models.R (NA rate and share of each level by year), never on a
# model score; 2024 is not looked at. Shares quoted are users of 2019 to 2023.

# Main specification: conditions of the accident and of the user, one
# variable per information.
model_vars_main <- c(
  # User. catu only for the user category: see place, locp, actp, etatp below.
  "catu", "sexe", "age", "trajet",
  "secu_ceinture", "secu_casque", "secu_enfant", "secu_gilet", "secu_airbag", "secu_gants",
  "secu_indetermine",
  # Vehicle. catv_groupe and not catv, its detailed version. manv is the main
  # manoeuvre before the accident (guide), not a description of the crash.
  "catv_groupe", "manv",
  # Time.
  "mois", "jour_semaine", "tranche_horaire",
  # Place and conditions.
  "lum", "agg", "int", "atm", "catr", "circ", "vosp", "prof", "plan", "surf", "infra", "situ", "vma"
)

# Variables of the crash itself, added in the ablation only.
model_vars_crash <- c("col", "choc", "obs", "obsm")
model_vars_ablation <- c(model_vars_main, model_vars_crash)

model_whitelist <- union(model_vars_main, model_vars_ablation)

# Never predictors: the target and its source, identifiers, the year (a year
# effect learned on 2019-2022 would be extrapolated to 2024) and the coordinates.
model_forbidden <- c("grave", "grav", "Num_Acc", "id_vehicule", "id_usager", "an", "lat", "long")

# Other columns of accidents_full.rds left out, and why:
# - catv: catv_groupe groups it, with fewer rare levels;
# - place: almost a detail of catu. "Piéton" is exactly the pedestrians, and
#   place 1 holds 427 509 of the 428 468 drivers; its dummy for pedestrians
#   would duplicate that of catu;
# - locp, actp, etatp: only defined for pedestrians, "Sans objet" (or "Non
#   renseigné ou sans objet") for the others. The other levels of etatp are
#   exactly the pedestrians, hence a collinearity with catu and the intercept
#   whatever the reference level;
# - occutc: number of occupants of a bus or coach, NA for every other vehicle
#   (98.7 % of the users);
# - lartpc: width of the central reservation, NA for 99.8 % of the users;
# - larrout: road width, NA for 93 to 99 % of the users from 2019 to 2022 and
#   69 % in 2023, a change of coding;
# - nbv: number of lanes. Its NA rate doubles in 2023 (2.8 % in 2022, 6.2 %)
#   and the missing values move to communal roads (39.8 % of them in 2022, 56.1 %
#   in 2023): an NA does not mean the same thing from one year to the next. The
#   type of road stays described by catr, circ and agg;
# - motor: the share of "Inconnue" moves from 10.2 % (2019) to 3.3 % (2022)
#   and back to 7.2 % (2023), a change of coding; human and electric power are
#   largely described by catv_groupe (bicycles, personal transporters);
# - senc: direction of traffic along the reference points of the road, a
#   property of the road reference system, not of the accident;
# - dep: locates the accident (96 levels); the question is about the
#   conditions of the accident, and agg, catr and vma describe the network.
#
# Kept with a known shift:
# - vma: NA rate 1.5 % from 2019 to 2022 and 3.5 % in 2023, with the same
#   move of the missing values to communal roads as nbv. Kept because the
#   study question names it (PLAN.md, appendix F, Q3); imputed by the training
#   median (50), the usual limit on those roads. A limit of the study;
# - circ: NA rate 5.1 to 5.5 %, then 6.8 % in 2023; NA recoded as a level (below);
# - situ and the safety equipment: 2019 differs from the following years
#   (situ "Sur chaussée" 88.8 % against 85.6 to 86.5 %; secu_indetermine 14.2 %
#   against 10.6 to 12.4 %), then the shares are stable; 2019 is in the
#   training set only;
# - vma 30 against 50 km/h, mois in 2020, catv_groupe "EDP": real changes
#   (speed limits lowered in towns, lockdowns, more electric scooters).

# Recodings of a coding change, applied to every year (fixed rules, nothing
# learned). Takes and returns a data.table.
# - vosp: NA merged into "Sans objet". In 2023, NA jumps to 2.2 % (0.3 to
#   1.1 % before) while "Sans objet" drops, and the sum of the two follows the
#   trend of the previous years (92.5, 90.0, 88.7, 87.8, 88.1 %);
# - catr: "Route de métropole urbaine" merged into "Route départementale".
#   The level goes 2.3, 1.6, 5.4, 4.1, 2.9 % while departmental roads move the
#   other way; the sum of the two is 41.3 to 42.4 % from 2020 on;
# - trajet and circ: NA becomes the level "Non renseigné" (the meaning of -1,
#   and of 0 for trajet, in the guide). They are the two whitelisted factors
#   with a frequent NA (26 % and 6 %); the recipe imputes the rare NA of the
#   others with the training mode. A "Manquant" level for every factor,
#   grouped with the rare levels, aliased a coefficient on 10 % of the
#   training set: the few NA of prof and plan fall on the same rows there.
na_level <- "Non renseigné"
na_level_vars <- c("trajet", "circ")

recode_model_vars <- function(dt) {
  dt <- data.table::copy(dt)
  # set() and not dt[, vosp := vosp]: inside [ ], the name would mean the column.
  vosp <- dt$vosp
  vosp[is.na(vosp)] <- "Sans objet"
  data.table::set(dt, j = "vosp", value = vosp)
  metro <- "Route de métropole urbaine"
  catr <- as.character(dt$catr)
  catr[catr %in% metro] <- "Route départementale"
  data.table::set(dt, j = "catr", value = factor(catr, levels = setdiff(levels(dt$catr), metro)))
  for (v in na_level_vars) {
    x <- dt[[v]]
    levels(x) <- c(levels(x), na_level)
    x[is.na(x)] <- na_level
    data.table::set(dt, j = v, value = x)
  }
  dt
}

# Rows built by hand: each recoded value, a missing catr and unchanged values.
local({
  lv <- function(x, levels) factor(x, levels = levels)
  vosp_levels <- c("Sans objet", "Piste cyclable", "Bande cyclable", "Voie réservée")
  catr_levels <- c("Route départementale", "Voie communale", "Route de métropole urbaine")
  dt <- data.table::data.table(
    vosp = lv(c(NA, "Piste cyclable", "Sans objet", NA), vosp_levels),
    catr = lv(c("Route de métropole urbaine", "Voie communale", NA, "Route départementale"), catr_levels),
    trajet = lv(c(NA, "Autre", NA, "Courses, achats"), c("Courses, achats", "Autre")),
    circ = lv(c("A sens unique", NA, NA, NA), c("A sens unique", "Bidirectionnelle")))
  out <- recode_model_vars(dt)
  stopifnot(
    identical(as.character(out$vosp), c("Sans objet", "Piste cyclable", "Sans objet", "Sans objet")),
    identical(levels(out$vosp), vosp_levels),
    identical(as.character(out$catr), c("Route départementale", "Voie communale", NA, "Route départementale")),
    identical(levels(out$catr), c("Route départementale", "Voie communale")),
    identical(as.character(out$trajet), c("Non renseigné", "Autre", "Non renseigné", "Courses, achats")),
    identical(levels(out$trajet), c("Courses, achats", "Autre", "Non renseigné")),
    identical(as.character(out$circ), c("A sens unique", rep("Non renseigné", 3))),
    anyNA(dt$vosp), anyNA(dt$trajet)  # the input is not modified
  )
})

# Columns left out above, listed so that every column of accidents_full.rds is
# classified once: whitelisted, excluded or forbidden.
model_excluded <- c("catv", "place", "locp", "actp", "etatp", "occutc", "lartpc", "larrout",
                    "nbv", "motor", "senc", "dep")

# Stops unless vars is a subset of the whitelist without an excluded or
# forbidden column, and never has catv next to catv_groupe.
check_model_vars <- function(vars) {
  stopifnot(
    length(vars) > 0, !anyDuplicated(vars),
    all(vars %in% model_whitelist),
    !any(vars %in% c(model_forbidden, model_excluded)),
    !("catv" %in% vars && "catv_groupe" %in% vars)
  )
  invisible(TRUE)
}

# Stops unless the three lists split the columns of the table exactly.
check_columns_classified <- function(columns) {
  lists <- list(model_whitelist, model_excluded, model_forbidden)
  stopifnot(
    !anyDuplicated(unlist(lists)),
    setequal(unlist(lists), columns)
  )
  invisible(TRUE)
}

check_model_vars(model_vars_main)
check_model_vars(model_vars_ablation)

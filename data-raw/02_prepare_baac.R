# Build the BAAC user-level table 2019 to 2024 from the raw files.
# Usage, from the project root, after data-raw/01_download_baac.R:
#   Rscript data-raw/02_prepare_baac.R
# Outputs:
#   data-raw/out/accidents_full.rds  every user kept (not deployed, not versioned);
#   data/accidents_sample.rds        accidents sampled per year, with all their users.
# The format findings behind each recoding are in data-raw/SCHEMA.md.

library(data.table)
source(file.path("data-raw", "baac_labels.R"), encoding = "UTF-8")

raw_dir <- file.path("data-raw", "raw")
out_dir <- file.path("data-raw", "out")
data_dir <- "data"
years <- 2019:2024

sample_fraction <- 0.07
sample_seed <- 20261106

metro_deps <- c(sprintf("%02d", c(1:19, 21:95)), "2A", "2B")
max_age <- 110
vma_range <- c(10, 130)

# Columns kept from each table. Left out: com, adr, voie, v1, v2, pr, pr1 and
# num_veh, which locate or identify an accident; jour and hrmn are only used
# to derive the weekday and the time slot.
caract_cols <- c("Num_Acc", "jour", "mois", "an", "hrmn", "lum", "dep", "agg", "int", "atm", "col", "lat", "long")
lieux_cols <- c("Num_Acc", "catr", "circ", "nbv", "vosp", "prof", "plan", "lartpc", "larrout",
                "surf", "infra", "situ", "vma")
vehicule_cols <- c("Num_Acc", "id_vehicule", "senc", "catv", "obs", "obsm", "choc", "manv", "motor", "occutc")
usager_cols <- c("Num_Acc", "id_usager", "id_vehicule", "place", "catu", "grav", "sexe", "an_nais",
                 "trajet", "secu1", "secu2", "secu3", "locp", "actp", "etatp")

labelled_vars <- c("lum", "agg", "int", "atm", "col", "catr", "circ", "vosp", "prof", "plan", "surf",
                   "infra", "situ", "senc", "catv", "obs", "obsm", "choc", "manv", "motor",
                   "place", "catu", "grav", "sexe", "trajet", "locp", "actp", "etatp",
                   "secu1", "secu2", "secu3")

output_cols <- c(
  "Num_Acc", "id_vehicule", "id_usager", "an", "grave", "grav",
  "catu", "place", "sexe", "age", "trajet", "secu1", "secu2", "secu3", "locp", "actp", "etatp",
  "catv", "catv_groupe", "senc", "motor", "manv", "obs", "obsm", "choc", "occutc",
  "mois", "jour_semaine", "tranche_horaire", "lum", "agg", "int", "atm", "col", "dep",
  "catr", "circ", "nbv", "vosp", "prof", "plan", "lartpc", "larrout", "surf", "infra", "situ", "vma",
  "lat", "long"
)

read_baac <- function(year, table, cols) {
  path <- file.path(raw_dir, year, paste0(table, ".csv"))
  dt <- fread(path, sep = ";", colClasses = "character", encoding = "UTF-8",
              na.strings = NULL, showProgress = FALSE)
  if ("Accident_Id" %in% names(dt)) setnames(dt, "Accident_Id", "Num_Acc")  # 2022
  if (table == "usagers" && !"id_usager" %in% names(dt)) dt[, id_usager := NA_character_]  # 2019, 2020
  dt <- dt[, ..cols]
  # "-1" is written " -1" inside quotes, and fread keeps spaces inside quotes.
  dt[, (cols) := lapply(.SD, trimws)]
  # Identifiers use non-breaking spaces as thousands separators ("138 306 524").
  for (id in intersect(c("id_vehicule", "id_usager"), cols)) {
    set(dt, j = id, value = gsub(" ", "", dt[[id]], fixed = TRUE))
  }
  dt
}

is_missing_code <- function(x, extra = character(0)) x %in% c("", "-1", extra)

# Codes absent from the label table become NA; returns them with their counts.
unknown_codes <- function(x, labels, missing_codes) {
  unknown <- x[!is_missing_code(x, missing_codes) & !x %in% names(labels)]
  if (length(unknown) == 0) return(NULL)
  counts <- table(unknown)
  data.table(code = names(counts), n = as.integer(counts))
}

label_codes <- function(x, labels) {
  factor(unname(labels[x]), levels = unname(labels))
}

# Decimal comma from 2020, decimal point in 2019; "#ERREUR" and similar become NA.
to_number <- function(x) {
  x[is_missing_code(x)] <- NA
  suppressWarnings(as.numeric(sub(",", ".", x, fixed = TRUE)))
}

prepare_year <- function(year) {
  ca <- read_baac(year, "caracteristiques", caract_cols)
  li <- read_baac(year, "lieux", lieux_cols)
  ve <- read_baac(year, "vehicules", vehicule_cols)
  us <- read_baac(year, "usagers", usager_cols)
  counts <- list(year = year, users_raw = nrow(us))

  if (anyNA(us$id_usager)) {
    # Built in file order, with a prefix that cannot collide with the numeric ids of 2021 on.
    us[, id_usager := sprintf("U%d_%06d", year, .I)]
  }

  # 2019 writes departments 1 to 9 without the leading zero.
  ca[nchar(dep) == 1, dep := paste0("0", dep)]
  metro_acc <- ca[dep %in% metro_deps, Num_Acc]

  # From 2023, lieux has one row per road for some accidents: the first row in
  # file order is kept, so that the join does not duplicate users.
  counts$lieux_rows_dropped <- sum(duplicated(li$Num_Acc))
  li <- li[!duplicated(Num_Acc)]

  # Fugitive users (from 2021): no flag in the files, sexe = -1 and an_nais empty.
  # Their grav is set to 1 or -1 without being observed.
  fugitive <- year >= 2021 & us$sexe == "-1" & us$an_nais == ""
  counts$fugitive <- sum(fugitive)
  us <- us[!fugitive]
  counts$grav_missing <- sum(us$grav == "-1")
  us <- us[grav != "-1"]
  counts$outside_metro <- sum(!us$Num_Acc %in% metro_acc)
  us <- us[Num_Acc %in% metro_acc]
  counts$users_kept <- nrow(us)

  # Join at the user level; each right-hand table has one row per key, checked here.
  stopifnot(!anyDuplicated(ca$Num_Acc), !anyDuplicated(li$Num_Acc),
            !anyDuplicated(ve, by = c("Num_Acc", "id_vehicule")))
  ve[, found_ve := TRUE]
  ca[, found_ca := TRUE]
  li[, found_li := TRUE]
  dt <- merge(us, ve, by = c("Num_Acc", "id_vehicule"), all.x = TRUE, sort = FALSE)
  dt <- merge(dt, ca, by = "Num_Acc", all.x = TRUE, sort = FALSE)
  dt <- merge(dt, li, by = "Num_Acc", all.x = TRUE, sort = FALSE)
  stopifnot(nrow(dt) == counts$users_kept, !anyNA(dt$found_ve), !anyNA(dt$found_ca), !anyNA(dt$found_li))
  counts$users_joined <- nrow(dt)

  unknown <- rbindlist(lapply(labelled_vars, function(v) {
    labels <- baac_labels[[if (startsWith(v, "secu")) "secu" else v]]
    u <- unknown_codes(dt[[v]], labels, baac_missing_codes[[v]])
    if (!is.null(u)) u[, `:=`(year = year, variable = v)]
    u
  }))

  dt[, grave := factor(ifelse(grav %in% c("2", "3"), "oui", "non"), levels = c("non", "oui"))]
  dt[, catv_groupe := factor(unname(catv_groups[catv]), levels = catv_group_levels)]

  dt[, an := as.integer(an)]
  dt[, age := an - as.integer(fifelse(is_missing_code(an_nais), NA_character_, an_nais))]
  # Ages above max_age come from default birth years (1900, 1901 in 2019).
  dt[age > max_age, age := NA]

  date <- as.Date(sprintf("%s-%s-%s", dt$an, dt$mois, dt$jour))
  stopifnot(!anyNA(date))
  # wday: 0 is Sunday; weekdays() would depend on the locale.
  dt[, jour_semaine := factor(weekday_levels[(as.POSIXlt(date)$wday + 6) %% 7 + 1], levels = weekday_levels)]
  dt[, mois := factor(month_levels[as.integer(mois)], levels = month_levels)]
  hour <- as.integer(substr(dt$hrmn, 1, 2))
  stopifnot(!anyNA(hour), all(hour >= 0 & hour <= 23))
  dt[, tranche_horaire := cut(hour, breaks = time_slot_breaks, labels = time_slot_levels, right = FALSE)]

  for (v in c("nbv", "lartpc", "larrout", "occutc", "lat", "long")) set(dt, j = v, value = to_number(dt[[v]]))
  dt[, vma := to_number(vma)]
  dt[vma < vma_range[1] | vma > vma_range[2], vma := NA]

  for (v in labelled_vars) {
    labels <- baac_labels[[if (startsWith(v, "secu")) "secu" else v]]
    set(dt, j = v, value = label_codes(dt[[v]], labels))
  }
  dt[, dep := factor(dep, levels = metro_deps)]

  dt <- dt[, ..output_cols]
  list(data = dt, counts = as.data.table(counts), unknown = unknown)
}

sample_accidents <- function(dt, fraction, seed) {
  set.seed(seed)
  acc <- unique(dt[, .(an, Num_Acc)])
  chosen <- acc[, .(Num_Acc = sort(Num_Acc)[sample.int(.N, round(.N * fraction))]), by = an]
  dt[Num_Acc %in% chosen$Num_Acc]
}

parts <- lapply(years, prepare_year)
full <- rbindlist(lapply(parts, `[[`, "data"))
full <- droplevels(full)
counts <- rbindlist(lapply(parts, `[[`, "counts"))
unknown <- rbindlist(lapply(parts, `[[`, "unknown"))

cat("\n## Users by year\n\n")
print(counts, row.names = FALSE)

cat("\n## Codes absent from the guide, set to NA\n\n")
if (nrow(unknown)) print(unknown[, .(n = sum(n)), by = .(variable, code)], row.names = FALSE) else cat("none\n")

# Checks required by the plan.
stopifnot(
  all(counts$users_joined == counts$users_kept),
  !anyDuplicated(full$id_usager),
  !anyNA(full$Num_Acc), !anyNA(full$id_vehicule), !anyNA(full$id_usager), !anyNA(full$grave),
  identical(names(full), output_cols)
)
cat("\nSame number of users before and after the join, no duplicated user.\n")

cat("\n## Share of grave by year (%)\n\n")
print(full[, .(users = .N, grave_pct = round(100 * mean(grave == "oui"), 2)), by = an], row.names = FALSE)

cat("\n## Missing values (%)\n\n")
na_pct <- sort(round(100 * vapply(full, function(x) mean(is.na(x)), numeric(1)), 2), decreasing = TRUE)
print(na_pct[na_pct > 0])

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(data_dir, showWarnings = FALSE)
full_path <- file.path(out_dir, "accidents_full.rds")
sample_path <- file.path(data_dir, "accidents_sample.rds")

sample <- droplevels(sample_accidents(full, sample_fraction, sample_seed))
saveRDS(as.data.frame(full), full_path)
saveRDS(as.data.frame(sample), sample_path)

cat("\n## Outputs\n\n")
sample_mb <- c(file = file.size(sample_path), memory = as.numeric(object.size(sample))) / 1024^2
cat(sprintf("%s: %d users, %.1f MB\n", full_path, nrow(full), file.size(full_path) / 1024^2))
cat(sprintf("%s: %d users, %d accidents, %.1f MB on disk, %.1f MB in memory\n",
            sample_path, nrow(sample), uniqueN(sample$Num_Acc), sample_mb[["file"]], sample_mb[["memory"]]))
print(sample[, .(users = .N, accidents = uniqueN(Num_Acc), grave_pct = round(100 * mean(grave == "oui"), 2)), by = an],
      row.names = FALSE)
stopifnot(sample_mb[["file"]] < 20, sample_mb[["memory"]] < 50)

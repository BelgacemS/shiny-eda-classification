# Audit of the raw BAAC files 2019 to 2024; its output is summarised in data-raw/SCHEMA.md.
# Usage, from the project root, after data-raw/01_download_baac.R:
#   Rscript data-raw/audit_baac.R
# Every value is read as character, so that codes such as "-1", "", "." and
# decimal commas are seen as they are in the files.

library(data.table)

raw_dir <- file.path("data-raw", "raw")
years <- 2019:2024
tables <- c("caracteristiques", "lieux", "vehicules", "usagers")

raw_path <- function(year, table) file.path(raw_dir, year, paste0(table, ".csv"))

read_raw <- function(year, table) {
  fread(raw_path(year, table), sep = ";", colClasses = "character", encoding = "UTF-8",
        na.strings = NULL, showProgress = FALSE)
}

section <- function(title) cat("\n## ", title, "\n\n", sep = "")

print_table <- function(dt) {
  print(as.data.frame(dt), row.names = FALSE, right = FALSE)
}

raw <- list()
for (y in years) for (t in tables) raw[[paste(y, t)]] <- read_raw(y, t)
raw_table <- function(year, table) raw[[paste(year, table)]]

key_of <- function(dt) intersect(c("Num_Acc", "Accident_Id"), names(dt))

section("File format")
fmt <- rbindlist(lapply(years, function(y) rbindlist(lapply(tables, function(t) {
  path <- raw_path(y, t)
  lines <- readLines(path, warn = FALSE, encoding = "bytes")
  header <- lines[1]
  first_bytes <- readBin(path, "raw", n = 3)
  data.table(
    year = y, table = t,
    sep = if (grepl(";", header, fixed = TRUE)) ";" else if (grepl(",", header, fixed = TRUE)) "," else "?",
    quoted_header = startsWith(header, "\""),
    bom = identical(first_bytes, as.raw(c(0xef, 0xbb, 0xbf))),
    crlf = grepl("\r$", header),
    utf8_valid = all(validUTF8(lines)),
    non_ascii_lines = sum(grepl("[^\\x01-\\x7F]", lines, perl = TRUE, useBytes = TRUE)),
    rows = nrow(raw_table(y, t)),
    cols = ncol(raw_table(y, t))
  )
}))))
print_table(fmt)

section("Columns")
for (t in tables) {
  cols <- lapply(years, function(y) names(raw_table(y, t)))
  ref <- cols[[length(cols)]]
  cat(t, "(2024):", paste(ref, collapse = ", "), "\n")
  for (i in seq_along(years)) {
    diff_in <- setdiff(cols[[i]], ref)
    diff_out <- setdiff(ref, cols[[i]])
    if (length(diff_in) || length(diff_out)) {
      cat("  ", years[i], ": extra =", paste(diff_in, collapse = ", "),
          "| missing =", paste(diff_out, collapse = ", "), "\n")
    }
  }
}

section("Accident key")
keys <- rbindlist(lapply(years, function(y) {
  ca <- raw_table(y, "caracteristiques")
  li <- raw_table(y, "lieux")
  ve <- raw_table(y, "vehicules")
  us <- raw_table(y, "usagers")
  k <- key_of(ca)
  acc <- ca[[k]]
  data.table(
    year = y, key_caract = k,
    key_nchar = paste(unique(nchar(acc)), collapse = "/"),
    key_prefix_is_year = all(startsWith(acc, as.character(y))),
    caract_unique = !anyDuplicated(acc),
    lieux_rows = nrow(li),
    lieux_unique = !anyDuplicated(li$Num_Acc),
    lieux_dup_keys = sum(duplicated(li$Num_Acc)),
    same_acc_lieux = setequal(acc, li$Num_Acc),
    vehicules_acc_in_caract = all(ve$Num_Acc %in% acc),
    usagers_acc_in_caract = all(us$Num_Acc %in% acc)
  )
}))
print_table(keys)

section("Vehicle and user identifiers")
ids <- rbindlist(lapply(years, function(y) {
  ve <- raw_table(y, "vehicules")
  us <- raw_table(y, "usagers")
  has_id_usager <- "id_usager" %in% names(us)
  data.table(
    year = y,
    id_usager = has_id_usager,
    id_usager_unique = if (has_id_usager) !anyDuplicated(us$id_usager) else NA,
    id_vehicule_unique_in_vehicules = !anyDuplicated(ve$id_vehicule),
    usagers_vehicule_in_vehicules = all(us$id_vehicule %in% ve$id_vehicule),
    vehicules_without_usager = sum(!ve$id_vehicule %in% us$id_vehicule),
    # Without id_usager, rows identical on every column cannot be told apart.
    usagers_full_duplicates = sum(duplicated(us))
  )
}))
print_table(ids)

section("lat and long")
geo <- rbindlist(lapply(years, function(y) {
  ca <- raw_table(y, "caracteristiques")
  lat <- as.numeric(sub(",", ".", ca$lat, fixed = TRUE))
  long <- as.numeric(sub(",", ".", ca$long, fixed = TRUE))
  data.table(
    year = y,
    lat_comma = mean(grepl(",", ca$lat, fixed = TRUE)),
    lat_dot = mean(grepl(".", ca$lat, fixed = TRUE)),
    lat_unparsed = sum(is.na(lat)),
    lat_range = paste(round(range(lat, na.rm = TRUE), 2), collapse = " to "),
    long_range = paste(round(range(long, na.rm = TRUE), 2), collapse = " to "),
    example = paste(ca$lat[1], ca$long[1])
  )
}))
print_table(geo)

section("hrmn")
hr <- rbindlist(lapply(years, function(y) {
  h <- raw_table(y, "caracteristiques")$hrmn
  data.table(
    year = y,
    hh_colon_mm = sum(grepl("^[0-2][0-9]:[0-5][0-9]$", h)),
    digits_only = sum(grepl("^[0-9]{1,4}$", h)),
    other = sum(!grepl("^[0-2][0-9]:[0-5][0-9]$|^[0-9]{1,4}$", h)),
    examples = paste(head(unique(h), 3), collapse = " ")
  )
}))
print_table(hr)

section("Other decimal commas")
for (y in years) for (t in tables) {
  dt <- raw_table(y, t)
  commas <- names(dt)[vapply(dt, function(x) any(grepl("^-?[0-9]+,[0-9]+$", x)), logical(1))]
  if (length(commas)) cat(y, t, ":", paste(commas, collapse = ", "), "\n")
}

section("Missing value codes (columns that contain each code)")
codes <- c(empty = "", minus1 = "-1", spaced_minus1 = " -1", dot = ".", na_text = "N/A")
for (t in tables) {
  cat("###", t, "\n")
  for (y in years) {
    dt <- raw_table(y, t)
    found <- vapply(codes, function(code) {
      cols <- names(dt)[vapply(dt, function(x) any(x == code), logical(1))]
      paste(cols, collapse = ",")
    }, character(1))
    cat(y, paste(names(codes), found, sep = "=", collapse = " | "), "\n")
  }
}

section("Whitespace around values")
for (y in years) for (t in tables) {
  dt <- raw_table(y, t)
  cols <- names(dt)[vapply(dt, function(x) any(x != trimws(x)), logical(1))]
  if (length(cols)) cat(y, t, ":", paste(cols, collapse = ", "), "\n")
}

section("grav, sexe and an_nais of users")
us_codes <- rbindlist(lapply(years, function(y) {
  us <- raw_table(y, "usagers")
  grav <- trimws(us$grav)
  sexe <- trimws(us$sexe)
  missing_age <- trimws(us$an_nais) %in% c("", "-1", "0")
  data.table(
    year = y,
    users = nrow(us),
    grav_values = paste(sort(unique(grav)), collapse = " "),
    grav_minus1 = sum(grav == "-1"),
    sexe_values = paste(sort(unique(sexe)), collapse = " "),
    sexe_minus1 = sum(sexe == "-1"),
    an_nais_missing = sum(missing_age),
    sexe_and_age_missing = sum(sexe == "-1" & missing_age),
    grav_sexe_age_missing = sum(sexe == "-1" & missing_age & grav == "-1")
  )
}))
print_table(us_codes)

section("Age at the accident (an - an_nais)")
ages <- rbindlist(lapply(years, function(y) {
  us <- raw_table(y, "usagers")
  age <- y - suppressWarnings(as.integer(us$an_nais))
  data.table(
    year = y,
    min = min(age, na.rm = TRUE), q50 = median(age, na.rm = TRUE), max = max(age, na.rm = TRUE),
    negative = sum(age < 0, na.rm = TRUE),
    over_100 = sum(age > 100, na.rm = TRUE),
    over_110 = sum(age > 110, na.rm = TRUE)
  )
}))
print_table(ages)

section("vma")
vma <- rbindlist(lapply(years, function(y) {
  v <- suppressWarnings(as.integer(trimws(raw_table(y, "lieux")$vma)))
  data.table(
    year = y,
    unparsed = sum(is.na(v)),
    minus1 = sum(v == -1, na.rm = TRUE),
    zero = sum(v == 0, na.rm = TRUE),
    from_1_to_9 = sum(v >= 1 & v <= 9, na.rm = TRUE),
    not_multiple_of_10 = sum(v > 0 & v %% 10 != 0, na.rm = TRUE),
    over_130 = sum(v > 130, na.rm = TRUE),
    max = max(v, na.rm = TRUE),
    top_values = paste(names(sort(table(v), decreasing = TRUE))[1:6], collapse = " ")
  )
}))
print_table(vma)

section("Departments outside metropolitan France")
metro <- c(sprintf("%02d", c(1:19, 21:95)), "2A", "2B")
deps <- rbindlist(lapply(years, function(y) {
  d <- raw_table(y, "caracteristiques")$dep
  outside <- d[!d %in% metro]
  data.table(
    year = y,
    accidents = length(d),
    outside = length(outside),
    outside_codes = paste(sort(unique(outside)), collapse = " "),
    dep_formats = paste(head(sort(unique(d)), 5), collapse = " ")
  )
}))
print_table(deps)

section("Drivers with sexe and age missing, 2021 and later")
fug <- rbindlist(lapply(2021:2024, function(y) {
  us <- raw_table(y, "usagers")
  drivers <- us[trimws(catu) == "1"]
  missing_age <- trimws(drivers$an_nais) %in% c("", "-1", "0")
  data.table(
    year = y,
    drivers = nrow(drivers),
    drivers_sexe_minus1 = sum(trimws(drivers$sexe) == "-1"),
    drivers_age_missing = sum(missing_age),
    drivers_both_missing = sum(trimws(drivers$sexe) == "-1" & missing_age),
    grav_of_both_missing = paste(names(table(trimws(drivers$grav[trimws(drivers$sexe) == "-1" & missing_age]))),
                                 table(trimws(drivers$grav[trimws(drivers$sexe) == "-1" & missing_age])),
                                 sep = ":", collapse = " ")
  )
}))
print_table(fug)

section("Duplicated Num_Acc in lieux")
for (y in 2023:2024) {
  li <- raw_table(y, "lieux")
  dup <- li[Num_Acc %in% Num_Acc[duplicated(Num_Acc)]]
  per_acc <- dup[, .N, by = Num_Acc]
  cat(y, ": accidents with several lieux rows =", nrow(per_acc),
      "| rows per such accident =", paste(names(table(per_acc$N)), table(per_acc$N), sep = ":", collapse = " "),
      "| fully identical rows =", sum(duplicated(li)), "\n")
  varying <- names(li)[vapply(names(li), function(col) {
    any(dup[, uniqueN(.SD[[col]]), by = Num_Acc]$V1 > 1)
  }, logical(1))]
  cat("  columns that differ within an accident:", paste(varying, collapse = ", "), "\n")
  ex <- dup[Num_Acc == dup$Num_Acc[1], .(Num_Acc, catr, voie, circ, vma, situ)]
  print_table(ex)
}

section("Department codes")
for (y in years) {
  d <- raw_table(y, "caracteristiques")$dep
  cat(y, ": codes =", length(unique(d)), "| Corsica =", paste(sort(unique(d[grepl("^2[AB0]|^20", d)])), collapse = " "),
      "| one character =", paste(sort(unique(d[nchar(d) == 1])), collapse = " "), "\n")
}

section("Coordinates of metropolitan accidents")
geo_metro <- rbindlist(lapply(years, function(y) {
  ca <- raw_table(y, "caracteristiques")
  d <- ca$dep
  if (y == 2019) d <- ifelse(nchar(d) == 1, paste0("0", d), d)
  ca <- ca[d %in% metro]
  lat <- as.numeric(sub(",", ".", ca$lat, fixed = TRUE))
  long <- as.numeric(sub(",", ".", ca$long, fixed = TRUE))
  # Bounding box of metropolitan France and Corsica.
  inside <- lat >= 41 & lat <= 51.2 & long >= -5.5 & long <= 10
  data.table(year = y, accidents = nrow(ca), zero_coords = sum(lat == 0 & long == 0),
             outside_box = sum(!inside))
}))
print_table(geo_metro)

section("Identifier format and uniqueness across years")
nbsp <- " "
id_fmt <- rbindlist(lapply(years, function(y) {
  ve <- raw_table(y, "vehicules")
  us <- raw_table(y, "usagers")
  data.table(
    year = y,
    id_vehicule_with_nbsp = mean(grepl(nbsp, ve$id_vehicule, fixed = TRUE)),
    id_vehicule_digits_after_nbsp_removed = all(grepl("^[0-9]+$", gsub(nbsp, "", ve$id_vehicule, fixed = TRUE))),
    id_usager_with_nbsp = if ("id_usager" %in% names(us)) mean(grepl(nbsp, us$id_usager, fixed = TRUE)) else NA,
    num_veh_examples = paste(head(sort(unique(us$num_veh)), 4), collapse = " ")
  )
}))
print_table(id_fmt)
all_ve <- unlist(lapply(years, function(y) raw_table(y, "vehicules")$id_vehicule))
all_us <- unlist(lapply(2021:2024, function(y) raw_table(y, "usagers")$id_usager))
cat("id_vehicule duplicated across 2019-2024:", sum(duplicated(all_ve)), "\n")
cat("id_usager duplicated across 2021-2024:", sum(duplicated(all_us)), "\n")

section("Birth years of users over 100 in 2019, and decimals of lat")
us19 <- raw_table(2019, "usagers")
birth <- as.integer(trimws(us19$an_nais))
print(table(birth[2019 - birth > 100]))
print_table(rbindlist(lapply(years, function(y) {
  lat <- trimws(raw_table(y, "caracteristiques")$lat)
  decimals <- nchar(sub("^[^,]*,", "", lat))
  data.table(year = y, min_decimals = min(decimals), max_decimals = max(decimals))
})))

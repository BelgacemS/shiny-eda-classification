# Reading, typing and validation of the dataset shared between tabs.
# A dataset is list(data = <data.frame>, var_types = <named chr>), see PLAN.md.

VAR_TYPES <- c("quanti", "quali", "id", "ignore")

# Above this number of distinct values, a character column is typed as an identifier.
MAX_QUALI_LEVELS <- 50

# Above this number of distinct values, a non-numeric column gets a high-cardinality alert.
HIGH_CARDINALITY <- 20

# Candidate separators for sep = "auto", in order of preference on ties.
SEPARATORS <- c(",", ";", "\t", "|")

# Reads a delimited text file.
# - encoding: "UTF-8" or "CP1252". read.csv() stops at the first invalid byte
#   with only a warning, which would silently truncate the file: the bytes are
#   checked first.
# - sep = "auto" picks the most frequent candidate separator of the first line.
# - na_codes: cell texts (surrounding spaces ignored) turned into NA, in the
#   columns na_cols only (all columns when empty), before type conversion, so
#   that a numeric column with a code such as "." is still read as numeric.
read_table_file <- function(path, sep = "auto", header = TRUE, encoding = "UTF-8",
                            dec = ".", na_codes = "NA", na_cols = NULL) {
  lines <- read_lines_utf8(path, encoding)
  if (identical(sep, "auto")) {
    sep <- guess_separator(lines)
  }
  if (sep == dec) {
    stop("le séparateur et la marque décimale doivent être différents.", call. = FALSE)
  }
  file_encoding <- if (encoding == "UTF-8") "UTF-8-BOM" else encoding
  # Everything is read as text: codes are matched on the cell text, then each
  # column is converted as read.csv() would do.
  data <- utils::read.csv(path, header = header, sep = sep, dec = dec,
                          fileEncoding = file_encoding, colClasses = "character",
                          na.strings = character(0))
  data <- apply_missing_codes(data, na_codes, na_cols)
  data[] <- lapply(data, utils::type.convert, as.is = TRUE, dec = dec,
                   na.strings = character(0))
  data
}

# Lines of the file converted to UTF-8; error if a byte is invalid in the encoding.
read_lines_utf8 <- function(path, encoding) {
  lines <- readLines(path, warn = FALSE)
  if (length(lines) == 0) {
    stop("le fichier est vide.", call. = FALSE)
  }
  lines <- switch(encoding,
    "UTF-8" = if (all(validUTF8(lines))) lines else NA,
    "CP1252" = iconv(lines, from = "CP1252", to = "UTF-8"),
    stop("encodage non pris en charge : ", encoding, call. = FALSE)
  )
  if (anyNA(lines)) {
    stop("le fichier n'est pas encodé en ", encoding,
         " : essayer un autre encodage.", call. = FALSE)
  }
  lines
}

# Most frequent candidate separator on the first non-empty line; "," when none appears.
guess_separator <- function(lines) {
  line <- lines[nzchar(trimws(lines))][1]
  if (is.na(line)) {
    return(",")
  }
  counts <- vapply(SEPARATORS, function(s) {
    lengths(regmatches(line, gregexpr(s, line, fixed = TRUE)))
  }, integer(1))
  if (max(counts) == 0) "," else SEPARATORS[which.max(counts)]
}

# Codes that always apply to every column: they are the read.csv() defaults, and
# restricting "-1" to one column must not turn "NA" into text elsewhere.
UNIVERSAL_NA_CODES <- c("NA", "")

# data: character columns as read. na_cols restricts the other codes; its columns
# absent from data are ignored.
apply_missing_codes <- function(data, na_codes, na_cols = NULL) {
  na_codes <- unique(trimws(na_codes))
  universal <- intersect(na_codes, UNIVERSAL_NA_CODES)
  cols <- if (length(na_cols) == 0) names(data) else intersect(na_cols, names(data))
  data[] <- lapply(names(data), function(name) {
    x <- data[[name]]
    codes <- if (name %in% cols) na_codes else universal
    x[trimws(x) %in% codes] <- NA
    x
  })
  data
}

# Detection rule:
# - numeric -> quanti; a whole-number column whose values are all distinct stays
#   quanti but is listed in attr(, "possible_id");
# - character, factor or logical -> quali, or id when its values are all distinct
#   or when it has more than MAX_QUALI_LEVELS distinct values;
# - any other class (dates, lists) -> ignore.
# Distinct values are counted without NA.
detect_types <- function(data) {
  types <- vapply(data, detect_type, character(1))
  possible_id <- names(data)[vapply(data, is_possible_id, logical(1)) & types == "quanti"]
  attr(types, "possible_id") <- possible_id
  types
}

detect_type <- function(x) {
  if (is.numeric(x)) {
    return("quanti")
  }
  if (is.character(x) || is.factor(x) || is.logical(x)) {
    if (has_all_distinct(x) || n_distinct(x) > MAX_QUALI_LEVELS) {
      return("id")
    }
    return("quali")
  }
  "ignore"
}

is_possible_id <- function(x) {
  is.numeric(x) && all(x == round(x), na.rm = TRUE) && has_all_distinct(x)
}

n_distinct <- function(x) {
  length(unique(x[!is.na(x)]))
}

# A single non-missing value is not enough to call a column an identifier.
has_all_distinct <- function(x) {
  n <- sum(!is.na(x))
  n > 1 && n_distinct(x) == n
}

# Builds a dataset from raw data and types: quali columns become factors,
# quanti columns numeric (values that are not numbers become NA).
make_dataset <- function(data, var_types) {
  data <- as.data.frame(data)
  var_types <- var_types[names(data)]
  quali <- names(var_types)[var_types == "quali"]
  quanti <- names(var_types)[var_types == "quanti"]
  data[quali] <- lapply(data[quali], as_factor)
  data[quanti] <- lapply(data[quanti], to_numeric)
  list(data = data, var_types = var_types)
}

as_factor <- function(x) {
  if (is.factor(x)) x else factor(x)
}

to_numeric <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }
  if (is.logical(x)) {
    return(as.numeric(x))
  }
  suppressWarnings(as.numeric(as.character(x)))
}

# Number of non-missing values that to_numeric() turns into NA.
n_lost_numeric <- function(x) {
  sum(!is.na(x) & is.na(to_numeric(x)))
}

# Sets the type of vars and rebuilds the dataset from the data as read, so that
# successive changes (quanti -> quali -> quanti) never lose values.
# A switch to quanti is refused for a column without any numeric value.
# Returns list(dataset, n_lost), n_lost giving the values turned into NA per column.
change_types <- function(data, var_types, vars, new_type) {
  if (!new_type %in% VAR_TYPES) {
    stop("type inconnu : ", new_type, call. = FALSE)
  }
  vars <- intersect(vars, names(data))
  n_lost <- integer(0)
  if (new_type == "quanti" && length(vars) > 0) {
    n_lost <- vapply(data[vars], n_lost_numeric, integer(1))
    n_present <- vapply(data[vars], function(x) sum(!is.na(x)), integer(1))
    refused <- vars[n_present > 0 & n_lost == n_present]
    if (length(refused) > 0) {
      stop("aucune valeur numérique dans : ", paste(refused, collapse = ", "), call. = FALSE)
    }
    n_lost <- n_lost[n_lost > 0]
  }
  var_types[vars] <- new_type
  list(dataset = make_dataset(data, var_types), n_lost = n_lost)
}

# One row per column of the dataset, for the variables table of tab 1.
# detected: types given by detect_types() on the data as read.
summarize_variables <- function(dataset, detected) {
  data <- dataset$data
  types <- dataset$var_types
  is_num <- vapply(data, is.numeric, logical(1))
  n_values <- vapply(data, n_distinct, integer(1))
  high_card <- !is_num & types != "ignore" & n_values > HIGH_CARDINALITY
  possible_id <- is_num & types == "quanti" & vapply(data, is_possible_id, logical(1))
  alerts <- ifelse(high_card, "forte cardinalité", "")
  alerts <- ifelse(possible_id, "identifiant possible", alerts)
  data.frame(
    variable = names(data),
    class = vapply(data, function(x) class(x)[1], character(1)),
    detected = unname(detected[names(data)]),
    type = unname(types),
    n_values = unname(n_values),
    pct_na = unname(vapply(data, function(x) 100 * mean(is.na(x)), numeric(1))),
    alert = unname(alerts),
    row.names = NULL
  )
}

# TRUE when names and classes agree with var_types; never raises an error,
# so that modules can wait on req(validate_dataset(...)).
validate_dataset <- function(dataset) {
  if (!is.list(dataset) || !all(c("data", "var_types") %in% names(dataset))) {
    return(FALSE)
  }
  data <- dataset$data
  types <- dataset$var_types
  if (!is.data.frame(data) || !is.character(types) || is.null(names(types))) {
    return(FALSE)
  }
  if (!identical(names(types), names(data)) || !all(types %in% VAR_TYPES)) {
    return(FALSE)
  }
  # Column checks on the whole list, then subset: data[logical] would index rows on a data.table.
  is_num <- vapply(data, is.numeric, logical(1))
  is_fac <- vapply(data, is.factor, logical(1))
  all(is_num[types == "quanti"]) && all(is_fac[types == "quali"])
}

format_dims <- function(data) {
  sprintf("%d lignes, %d colonnes", nrow(data), ncol(data))
}

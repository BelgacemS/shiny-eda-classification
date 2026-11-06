# Reading, typing and validation of the dataset shared between tabs.
# A dataset is list(data = <data.frame>, var_types = <named chr>), see PLAN.md.

VAR_TYPES <- c("quanti", "quali", "id", "ignore")

# Above this number of distinct values, a character column is typed as an identifier.
MAX_QUALI_LEVELS <- 50

# Default import options; the import settings come in E5.
# read.csv() stops at the first invalid byte with only a warning, which would
# silently truncate a Latin-1 file: the encoding is checked first.
read_table_file <- function(path) {
  if (!all(validUTF8(readLines(path, warn = FALSE)))) {
    stop("le fichier n'est pas encodé en UTF-8.", call. = FALSE)
  }
  utils::read.csv(path, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
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

# Builds a dataset from raw data and types: quali columns become factors.
make_dataset <- function(data, var_types) {
  data <- as.data.frame(data)
  var_types <- var_types[names(data)]
  quali <- names(var_types)[var_types == "quali"]
  data[quali] <- lapply(data[quali], as_factor)
  list(data = data, var_types = var_types)
}

as_factor <- function(x) {
  if (is.factor(x)) x else factor(x)
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

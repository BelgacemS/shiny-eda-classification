# Train / internal validation / test split (PLAN.md, step E10a).
#
# Output: list(train, valid, test); each part is list(x = <predictors>,
# y = <0/1 target>, rows = <indices in the input>), the format expected by
# balance_train() and fit_model(). The validation set is taken from the
# training rows, with the same grouping. y must be 0/1 without NA: rows
# without target are removed before the split.
#
# Modes:
# - "stratified": within each class, test_prop of the rows go to test;
# - "grouped": a group (e.g. an accident) is never on two sides; groups are
#   stratified on "the group contains a positive";
# - "variable": test = rows whose `by` value is in test_values (e.g. a year);
#   the validation set is then stratified, or grouped when groups are given.
#   The test is defined by rows: a group with rows on both sides of the test
#   values is cut; attr "crossing_groups" counts such groups (0 for BAAC, where
#   the year is the same for every user of an accident) so that the interface
#   can warn.
# In the stratified mode, groups are ignored: choose the grouped mode to keep
# groups together. Every part must be non-empty, and the training and
# validation sets must contain both classes (the threshold is chosen on the
# validation set); otherwise the split stops with a message.

SPLIT_MODES <- c("stratified", "grouped", "variable")

split_data <- function(x, y, mode = "stratified", test_prop = 0.2, valid_prop = 0.2,
                       groups = NULL, by = NULL, test_values = NULL, seed = 1) {
  check_split_inputs(x, y, mode, test_prop, valid_prop, groups, by, test_values)
  grouped <- mode == "grouped" || (mode == "variable" && !is.null(groups))
  units <- split_units(y, if (grouped) groups, grouped = grouped)
  with_seed(seed, {
    if (mode == "variable") {
      test_rows <- which(by %in% test_values)
      train_rows <- setdiff(seq_along(y), test_rows)
    } else {
      test_units <- draw_units(units, seq_len(nrow(units)), test_prop)
      test_rows <- which(attr(units, "unit_of_row") %in% test_units)
      train_rows <- setdiff(seq_along(y), test_rows)
    }
    remaining <- unique(attr(units, "unit_of_row")[train_rows])
    valid_units <- draw_units(units, remaining, valid_prop)
    valid_rows <- train_rows[attr(units, "unit_of_row")[train_rows] %in% valid_units]
  })
  train_rows <- setdiff(train_rows, valid_rows)
  part <- function(rows) {
    rows <- sort(rows)
    list(x = x[rows, , drop = FALSE], y = y[rows], rows = rows)
  }
  out <- list(train = part(train_rows), valid = part(valid_rows), test = part(test_rows))
  check_split_parts(out)
  if (mode == "variable" && !is.null(groups)) {
    unit <- attr(units, "unit_of_row")
    attr(out, "crossing_groups") <- length(intersect(unit[test_rows], unit[-test_rows]))
  }
  out
}

# Sampling units: rows, or groups (a missing group id makes its row a group of
# its own). Each unit is labelled positive when it contains a positive row.
split_units <- function(y, groups, grouped) {
  n <- length(y)
  ids <- if (grouped) {
    g <- as.character(groups)
    ifelse(is.na(g), paste0("\u0001row", seq_len(n)), g)
  } else {
    as.character(seq_len(n))
  }
  unit_of_row <- match(ids, unique(ids))
  positive <- as.vector(tapply(y == 1, unit_of_row, any))
  units <- data.frame(unit = seq_along(positive), positive = positive)
  attr(units, "unit_of_row") <- unit_of_row
  units
}

# Draws prop of the given units within each stratum (positive or not); rounded
# per stratum with round(), which rounds half to even: with prop = 0.5, strata
# of 1, 3 and 5 units give 0, 2 and 2 draws.
draw_units <- function(units, candidates, prop) {
  drawn <- integer(0)
  for (stratum in c(TRUE, FALSE)) {
    pool <- candidates[units$positive[candidates] == stratum]
    k <- round(length(pool) * prop)
    if (k > 0) drawn <- c(drawn, pool[sample.int(length(pool), k)])
  }
  drawn
}

check_split_inputs <- function(x, y, mode, test_prop, valid_prop, groups, by, test_values) {
  if (!mode %in% SPLIT_MODES) {
    stop("mode de découpage inconnu : ", mode, call. = FALSE)
  }
  if (nrow(x) != length(y)) {
    stop("x et y n'ont pas le même nombre de lignes.", call. = FALSE)
  }
  if (anyNA(y) || !all(y %in% c(0, 1))) {
    stop("la cible doit valoir 0 ou 1, sans valeur manquante.", call. = FALSE)
  }
  for (p in c(test_prop, valid_prop)) {
    if (!is.numeric(p) || length(p) != 1 || is.na(p) || p <= 0 || p >= 1) {
      stop("les proportions doivent être dans ]0, 1[.", call. = FALSE)
    }
  }
  if (mode == "grouped" && (is.null(groups) || length(groups) != length(y))) {
    stop("le mode groupé demande un identifiant de groupe par ligne.", call. = FALSE)
  }
  if (!is.null(groups) && length(groups) != length(y)) {
    stop("un identifiant de groupe est attendu par ligne.", call. = FALSE)
  }
  if (mode == "variable" && (is.null(by) || length(by) != length(y) || length(test_values) == 0)) {
    stop("le mode par variable demande une colonne et des valeurs pour le test.", call. = FALSE)
  }
  if (mode == "variable" && anyNA(by)) {
    stop("la colonne du découpage par variable a des valeurs manquantes.", call. = FALSE)
  }
}

check_split_parts <- function(split) {
  labels <- c(train = "l'entraînement", valid = "la validation", test = "le test")
  for (p in names(split)) {
    if (length(split[[p]]$y) == 0) {
      stop(labels[[p]], " est vide : augmenter l'échantillon ou changer les proportions.", call. = FALSE)
    }
  }
  for (p in c("train", "valid")) {
    if (length(unique(split[[p]]$y)) < 2) {
      stop(labels[[p]], " ne contient qu'une classe de la cible : augmenter l'échantillon.",
           call. = FALSE)
    }
  }
}

# Row indices of a sample of about `size` rows: rows drawn at random, or whole
# groups drawn at random until `size` rows are reached, so that a group is never
# cut. Returns every row when size >= n.
sample_rows <- function(n, size, groups = NULL, seed = 1) {
  if (!is.numeric(size) || length(size) != 1 || is.na(size) || size < 1) {
    stop("la taille de l'échantillon doit être un nombre supérieur ou égal à 1.", call. = FALSE)
  }
  if (!is.null(groups) && length(groups) != n) {
    stop("un identifiant de groupe est attendu par ligne.", call. = FALSE)
  }
  if (size >= n) {
    return(seq_len(n))
  }
  if (is.null(groups)) {
    return(sort(with_seed(seed, sample.int(n, size))))
  }
  g <- as.character(groups)
  g[is.na(g)] <- paste0("\u0001row", which(is.na(g)))
  ids <- unique(g)
  order <- ids[with_seed(seed, sample.int(length(ids)))]
  sizes <- as.vector(table(factor(g, levels = order)))
  chosen <- order[seq_len(which(cumsum(sizes) >= size)[1])]
  sort(which(g %in% chosen))
}

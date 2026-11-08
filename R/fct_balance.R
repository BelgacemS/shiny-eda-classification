# Class rebalancing, applied to the training set only (PLAN.md, step E7).
#
# A split is list(train, valid, test); each part is list(x = <data.frame of
# predictors>, y = <target>), the target having no NA (rows without target are
# removed before the split). balance_train() returns the same split where only
# the training part changes; with the "weights" method it gets a `weights`
# element (`train$weights` is NULL otherwise).
# It is applied after the recipe: x is the transformed training set and y the
# target aligned with fitted$kept_rows.
#
# Resampling ratio: n_minority / n_majority in the training set after
# resampling, in (0, 1]; 1 gives two classes of the same size.

BALANCE_METHODS <- c("none", "weights", "under", "over")

# Class weights n / (K * n_c), named by class: each class then weighs n / K in total.
class_weights <- function(y) {
  check_target(y)
  counts <- table(as.character(y))
  n <- length(y)
  k <- length(counts)
  stats::setNames(as.numeric(n / (k * counts)), names(counts))
}

# Weight of each observation, from the weight of its class.
observation_weights <- function(y) {
  unname(class_weights(y)[as.character(y)])
}

# Row indices that keep every minority row and a sample without replacement of
# the majority rows, so that n_minority / n_majority = ratio (rounded).
undersample <- function(y, ratio = 1, seed = 1) {
  classes <- binary_classes(y, ratio)
  minority <- which(as.character(y) == classes$minority)
  majority <- which(as.character(y) == classes$majority)
  n_majority <- min(length(majority), max(1, round(length(minority) / ratio)))
  kept <- with_seed(seed, majority[sample.int(length(majority), n_majority)])
  sort(c(minority, kept))
}

# Row indices that keep every row and add minority rows drawn with replacement,
# so that n_minority / n_majority = ratio (rounded).
oversample <- function(y, ratio = 1, seed = 1) {
  classes <- binary_classes(y, ratio)
  minority <- which(as.character(y) == classes$minority)
  majority <- which(as.character(y) == classes$majority)
  n_extra <- max(0, round(ratio * length(majority)) - length(minority))
  extra <- with_seed(seed, minority[sample.int(length(minority), n_extra, replace = TRUE)])
  c(seq_along(y), extra)
}

balance_train <- function(split, method = "none", ratio = 1, seed = 1) {
  if (length(method) != 1 || !method %in% BALANCE_METHODS) {
    stop("méthode de rééquilibrage inconnue : ", method, call. = FALSE)
  }
  train <- split$train
  train$weights <- NULL
  if (method == "weights") {
    train$weights <- observation_weights(train$y)
  }
  if (method %in% c("under", "over")) {
    rows <- if (method == "under") undersample(train$y, ratio, seed) else
      oversample(train$y, ratio, seed)
    train$x <- train$x[rows, , drop = FALSE]
    rownames(train$x) <- NULL
    train$y <- train$y[rows]
    # The original row indices of split_data() follow the resampling.
    if (!is.null(train$rows)) train$rows <- train$rows[rows]
  }
  split$train <- train
  split
}

check_target <- function(y) {
  if (length(y) == 0 || anyNA(y)) {
    stop("la cible doit être non vide et sans valeur manquante.", call. = FALSE)
  }
}

# Minority and majority class names of a binary target; ties go to the first
# class in sorted order as the minority.
binary_classes <- function(y, ratio) {
  check_target(y)
  if (!is.numeric(ratio) || length(ratio) != 1 || is.na(ratio) || ratio <= 0 || ratio > 1) {
    stop("le ratio doit être dans ]0, 1].", call. = FALSE)
  }
  counts <- table(as.character(y))
  if (length(counts) != 2) {
    stop("le rééchantillonnage demande exactement deux classes.", call. = FALSE)
  }
  order <- order(as.numeric(counts))
  list(minority = names(counts)[order[1]], majority = names(counts)[order[2]])
}

# Evaluates expr with a given seed, then restores the global random state,
# which all sessions of the server share.
with_seed <- function(seed, expr) {
  env <- globalenv()
  had_seed <- exists(".Random.seed", envir = env, inherits = FALSE)
  old <- if (had_seed) get(".Random.seed", envir = env, inherits = FALSE)
  on.exit({
    if (had_seed) assign(".Random.seed", old, envir = env)
    else if (exists(".Random.seed", envir = env, inherits = FALSE)) rm(".Random.seed", envir = env)
  })
  set.seed(seed)
  expr
}

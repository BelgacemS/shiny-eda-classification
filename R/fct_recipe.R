# Learned preprocessing recipe (contract in PLAN.md).
#
# fit_recipe() learns every parameter on the training set, step by step: each
# step is fitted on the output of the previous one. apply_recipe() replays the
# steps with these parameters and never learns anything nor removes a row.
#
# Steps, in order: outliers, imputation, rare levels, scaling, dummy coding.
# Only "quanti" and "quali" columns are transformed; "id" and "ignore" columns
# pass through untouched, so the target can be kept out of the recipe by giving
# it the "ignore" type (or by passing the predictors only).
# Each step accepts an optional `vars`: the columns it applies to (all columns
# of the relevant type when NULL); names absent from the training set are ignored.

RECIPE_OTHER <- "Autre"
RECIPE_MISSING <- "Manquant"

default_recipe <- function() {
  list(
    # method: "tukey" (1.5 IQR, quantiles type 7) or "zscore" (|x - mean| > z sd, 1/n);
    # action: "none", "flag", "cap" (train and new data) or "remove" (training rows only).
    outliers = list(method = "tukey", z = 3, action = "none"),
    # quanti: "none", "median" or "mean"; quali: "none", "mode" or "missing_level".
    impute = list(quanti = "none", quali = "none"),
    # Levels whose share of the non-missing training values is below threshold go to "Autre".
    rare = list(threshold = 0),
    # method: "none", "standard" (mean and sd in 1/n) or "minmax".
    scale = list(method = "none"),
    # method: "none", "one_hot" (k columns) or "k_minus_1" (reference = most frequent level);
    # a variable that would need more than max_cols columns is dropped.
    dummy = list(method = "none", max_cols = 50)
  )
}

# Learns the recipe on the training set.
# Returns a "fitted_recipe": the parameters of each step, the transformed
# training set (`train`), the training rows kept (`kept_rows`, indices into the
# original train), the output column types (`output_types`) and the messages
# for the user (`messages`: dropped or refused columns).
fit_recipe <- function(recipe, train, var_types) {
  recipe <- utils::modifyList(default_recipe(), recipe)
  train <- as.data.frame(train)
  var_types <- var_types[intersect(names(var_types), names(train))]
  fitted <- structure(
    list(recipe = recipe, var_types = var_types, input_names = names(train),
         params = list(), messages = character(0), kept_rows = seq_len(nrow(train))),
    class = "fitted_recipe"
  )

  out <- fit_outliers(fitted, train)
  fitted <- out$fitted
  data <- out$data
  for (step in c("impute", "levels", "scale", "dummy")) {
    fitter <- switch(step, impute = fit_impute, levels = fit_levels,
                     scale = fit_scale, dummy = fit_dummy)
    fitted <- fitter(fitted, data)
    data <- apply_step(step, fitted, data)
  }
  fitted$output_names <- names(data)
  fitted$output_types <- output_types(fitted)
  names(fitted$output_types) <- fitted$output_names
  fitted$train <- data
  fitted
}

# Applies the learned parameters to new data, without learning anything.
# Never removes a row: with action "remove", outliers of new data are kept as they are.
apply_recipe <- function(fitted, newdata) {
  stopifnot(inherits(fitted, "fitted_recipe"))
  newdata <- as.data.frame(newdata)
  missing <- setdiff(fitted$input_names, names(newdata))
  if (length(missing) > 0) {
    stop("colonnes absentes des données : ", paste(missing, collapse = ", "), call. = FALSE)
  }
  data <- newdata[fitted$input_names]
  for (step in c("outliers", "impute", "levels", "scale", "dummy")) {
    data <- apply_step(step, fitted, data)
  }
  names(data) <- fitted$output_names
  data
}

apply_step <- function(step, fitted, data) {
  switch(step,
    outliers = apply_outliers(fitted, data),
    impute = apply_impute(fitted, data),
    levels = apply_levels(fitted, data),
    scale = apply_scale(fitted, data),
    dummy = apply_dummy(fitted, data)
  )
}

# Columns of a given type targeted by a step, restricted to `vars` when given.
step_cols <- function(fitted, data, type, vars = NULL) {
  cols <- names(fitted$var_types)[fitted$var_types %in% type]
  cols <- intersect(cols, names(data))
  if (!is.null(vars)) cols <- intersect(cols, vars)
  cols
}

observed <- function(x) x[!is.na(x)]

# --- 1. Outliers -------------------------------------------------------------

# Bounds outside which a value is an outlier, from the non-missing values of x.
outlier_bounds <- function(x, method = "tukey", z = 3) {
  x <- observed(x)
  if (length(x) == 0) {
    return(c(lower = -Inf, upper = Inf))
  }
  if (method == "tukey") {
    q <- stats::quantile(x, c(0.25, 0.75), type = 7, names = FALSE)
    iqr <- q[2] - q[1]
    return(c(lower = q[1] - 1.5 * iqr, upper = q[2] + 1.5 * iqr))
  }
  if (method == "zscore") {
    m <- mean_1n(x)
    s <- sd_1n(x)
    return(c(lower = m - z * s, upper = m + z * s))
  }
  stop("méthode d'outliers inconnue : ", method, call. = FALSE)
}

is_outlier <- function(x, bounds) {
  !is.na(x) & (x < bounds[["lower"]] | x > bounds[["upper"]])
}

# Fitted alone because "remove" changes the training rows; returns list(fitted, data).
fit_outliers <- function(fitted, train) {
  settings <- fitted$recipe$outliers
  cols <- if (settings$action == "none") character(0) else
    step_cols(fitted, train, "quanti", settings$vars)
  bounds <- lapply(train[cols], outlier_bounds, method = settings$method, z = settings$z)
  flags <- vapply(cols, function(v) is_outlier(train[[v]], bounds[[v]]),
                  logical(nrow(train)))
  flags <- matrix(flags, nrow = nrow(train), ncol = length(cols))  # (n, number of columns)
  fitted$params$outliers <- list(
    bounds = bounds,
    counts = stats::setNames(colSums(flags), cols)
  )
  if (settings$action == "remove") {
    keep <- rowSums(flags) == 0
    fitted$kept_rows <- which(keep)
    train <- train[keep, , drop = FALSE]
    rownames(train) <- NULL
  }
  list(fitted = fitted, data = apply_outliers(fitted, train))
}

# Only "cap" changes values; "remove" acts on the training rows in fit_outliers().
apply_outliers <- function(fitted, data) {
  if (fitted$recipe$outliers$action != "cap") {
    return(data)
  }
  bounds <- fitted$params$outliers$bounds
  for (v in names(bounds)) {
    data[[v]] <- pmin(pmax(data[[v]], bounds[[v]][["lower"]]), bounds[[v]][["upper"]])
  }
  data
}

# --- 2. Imputation -----------------------------------------------------------

# Most frequent value of x, or of a table of counts; ties go to the first level.
most_frequent <- function(x) {
  counts <- if (is.table(x)) x else table(x)
  if (length(counts) == 0 || max(counts) == 0) {
    return(NA_character_)
  }
  names(counts)[which.max(counts)]
}

fit_impute <- function(fitted, data) {
  settings <- fitted$recipe$impute
  quanti <- if (settings$quanti == "none") character(0) else
    step_cols(fitted, data, "quanti", settings$vars)
  quali <- if (settings$quali == "none") character(0) else
    step_cols(fitted, data, "quali", settings$vars)
  quanti_values <- lapply(data[quanti], function(x) {
    x <- observed(x)
    if (length(x) == 0) return(NA_real_)
    switch(settings$quanti, median = stats::median(x), mean = mean_1n(x),
           stop("imputation inconnue : ", settings$quanti, call. = FALSE))
  })
  quali_values <- lapply(data[quali], function(x) {
    switch(settings$quali, mode = most_frequent(x), missing_level = RECIPE_MISSING,
           stop("imputation inconnue : ", settings$quali, call. = FALSE))
  })
  fitted$params$impute <- c(quanti_values, quali_values)
  fitted
}

apply_impute <- function(fitted, data) {
  for (v in names(fitted$params$impute)) {
    value <- fitted$params$impute[[v]]
    x <- data[[v]]
    if (is.na(value)) next
    if (is.factor(x) && !value %in% levels(x)) levels(x) <- c(levels(x), value)
    x[is.na(x)] <- value
    data[[v]] <- x
  }
  data
}

# --- 3. Rare and unknown levels ------------------------------------------------

# Levels kept as they are: training levels with a share of the non-missing values
# at least equal to the threshold. Every other value (rare in training, or unseen)
# goes to "Autre" when "Autre" has training rows, else to the most frequent
# training level, so that no level of the output is empty in training.
fit_levels <- function(fitted, data) {
  threshold <- fitted$recipe$rare$threshold
  cols <- step_cols(fitted, data, "quali", fitted$recipe$rare$vars)
  fitted$params$levels <- lapply(stats::setNames(cols, cols), function(v) {
    values <- as.character(data[[v]])
    # Level order of the factor; sorted in the C locale for a character column.
    order <- if (is.factor(data[[v]])) levels(data[[v]]) else
      sort(unique(observed(values)), method = "radix")
    counts <- table(factor(values, levels = unique(c(order, observed(values)))))
    share <- counts / max(sum(counts), 1)
    kept <- names(counts)[counts > 0 & share >= threshold]
    has_other <- any(counts > 0 & share < threshold)
    target <- if (has_other) RECIPE_OTHER else most_frequent(counts)
    list(levels = unique(c(kept, if (has_other) RECIPE_OTHER)), target = target)
  })
  fitted
}

apply_levels <- function(fitted, data) {
  for (v in names(fitted$params$levels)) {
    p <- fitted$params$levels[[v]]
    x <- as.character(data[[v]])
    x[!is.na(x) & !x %in% p$levels] <- p$target
    data[[v]] <- factor(x, levels = p$levels)
  }
  data
}

# --- 4. Scaling ----------------------------------------------------------------

# A column with no spread in training (sd or range zero, or no value) is dropped.
fit_scale <- function(fitted, data) {
  method <- fitted$recipe$scale$method
  cols <- if (method == "none") character(0) else
    step_cols(fitted, data, "quanti", fitted$recipe$scale$vars)
  params <- lapply(data[cols], function(x) {
    x <- observed(x)
    if (length(x) == 0) return(c(center = NA_real_, scale = 0))
    switch(method,
      standard = c(center = mean_1n(x), scale = sd_1n(x)),
      minmax = c(center = min(x), scale = max(x) - min(x)),
      stop("normalisation inconnue : ", method, call. = FALSE)
    )
  })
  constant <- names(params)[vapply(params, function(p) !(p[["scale"]] > 0), logical(1))]
  if (length(constant) > 0) {
    fitted$messages <- c(fitted$messages, paste0(
      "colonne constante dans l'entraînement, retirée : ", paste(constant, collapse = ", ")))
  }
  fitted$params$scale <- list(params = params[setdiff(cols, constant)], dropped = constant)
  fitted
}

apply_scale <- function(fitted, data) {
  p <- fitted$params$scale
  for (v in names(p$params)) {
    data[[v]] <- (data[[v]] - p$params[[v]][["center"]]) / p$params[[v]][["scale"]]
  }
  data[setdiff(names(data), p$dropped)]
}

# --- 5. Dummy coding -------------------------------------------------------------

# Columns per variable: one per training level (one-hot), or all but the most
# frequent one (k - 1). A missing value gives NA in every column of the variable.
fit_dummy <- function(fitted, data) {
  settings <- fitted$recipe$dummy
  cols <- if (settings$method == "none") character(0) else
    step_cols(fitted, data, "quali", settings$vars)
  params <- lapply(stats::setNames(cols, cols), function(v) {
    x <- data[[v]]
    counts <- table(x)
    present <- names(counts)[counts > 0]
    reference <- if (length(present) > 0) names(counts)[which.max(counts)] else NA_character_
    coded <- switch(settings$method,
      one_hot = present,
      k_minus_1 = setdiff(present, reference),
      stop("dummification inconnue : ", settings$method, call. = FALSE)
    )
    names <- if (length(coded) > 0) make.names(paste(v, coded, sep = "_")) else character(0)
    list(levels = coded, reference = reference, names = names)
  })
  too_wide <- names(params)[vapply(params, function(p) length(p$levels) > settings$max_cols,
                                   logical(1))]
  if (length(too_wide) > 0) {
    fitted$messages <- c(fitted$messages, paste0(
      "plus de ", settings$max_cols, " colonnes après dummification, variable retirée : ",
      paste(too_wide, collapse = ", ")))
  }
  fitted$params$dummy <- list(params = params[setdiff(cols, too_wide)], dropped = too_wide)
  fitted
}

# Each dummified variable is replaced in place by its columns; names are made
# unique, since a dummy name such as "x_a" may already exist.
apply_dummy <- function(fitted, data) {
  p <- fitted$params$dummy
  data <- data[setdiff(names(data), p$dropped)]
  values <- list()
  for (v in names(data)) {
    if (!v %in% names(p$params)) {
      values[[length(values) + 1]] <- data[[v]]
      names(values)[length(values)] <- v
      next
    }
    x <- as.character(data[[v]])
    for (i in seq_along(p$params[[v]]$levels)) {
      values[[length(values) + 1]] <- as.numeric(x == p$params[[v]]$levels[i])
      names(values)[length(values)] <- p$params[[v]]$names[i]
    }
  }
  out <- data.frame(row.names = seq_len(nrow(data)))
  out[make.unique(names(values))] <- values
  rownames(out) <- NULL
  out
}

# Type of each output column: dummy columns are quanti, the rest keep their type.
output_types <- function(fitted) {
  dummy <- fitted$params$dummy
  dropped <- c(fitted$params$scale$dropped, dummy$dropped)
  kept <- setdiff(fitted$input_names, dropped)
  types <- lapply(kept, function(v) {
    if (v %in% names(dummy$params)) {
      return(rep("quanti", length(dummy$params[[v]]$levels)))
    }
    if (v %in% names(fitted$var_types)) fitted$var_types[[v]] else "ignore"
  })
  unlist(types, use.names = FALSE)
}

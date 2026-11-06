# Learned preprocessing recipe (contract in PLAN.md).
# In E2, fit_recipe() and apply_recipe() are identities with their final signatures;
# the five steps are implemented in E6a.

default_recipe <- function() {
  list(
    outliers = list(action = "none"),
    impute = list(quanti = "none", quali = "none"),
    rare = list(threshold = 0),
    scale = list(method = "none"),
    dummy = list(method = "none")
  )
}

# Learns the recipe parameters on the training set only.
fit_recipe <- function(recipe, train, var_types) {
  structure(
    list(recipe = recipe, var_types = var_types[intersect(names(var_types), names(train))]),
    class = "fitted_recipe"
  )
}

# Applies the learned parameters to new data, without learning anything.
apply_recipe <- function(fitted, newdata) {
  stopifnot(inherits(fitted, "fitted_recipe"))
  newdata
}

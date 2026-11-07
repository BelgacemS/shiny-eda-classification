toy_raw <- function() {
  make_dataset(
    data.frame(
      x = c(1, 2, 3, 4, 100, NA),
      y = c(2.5, 3, 3, NA, 4, 5),
      g = c("a", "b", "a", "b", NA, "a"),
      code = c("c1", "c2", NA, "c4", "c5", "c6")
    ),
    c(x = "quanti", y = "quanti", g = "quali", code = "id")
  )
}

# --- Pure functions ------------------------------------------------------------

test_that("clean_dataset removes columns from data and var_types together", {
  out <- clean_dataset(toy_raw(), drop_cols = c("y", "absent"))
  expect_equal(names(out$data), c("x", "g", "code"))
  expect_equal(names(out$var_types), c("x", "g", "code"))
  expect_true(validate_dataset(out))
})

test_that("incomplete rows are judged on quanti and quali columns only", {
  # Rows 4 (y), 5 (g) and 6 (x) have a missing value; row 3 only misses its id.
  out <- clean_dataset(toy_raw(), drop_incomplete = TRUE)
  expect_equal(out$data$x, c(1, 2, 3))
  expect_equal(rownames(out$data), c("1", "2", "3"))
  # Dropping y first keeps row 4.
  out <- clean_dataset(toy_raw(), drop_cols = "y", drop_incomplete = TRUE)
  expect_equal(out$data$x, c(1, 2, 3, 4))
})

test_that("outlier_summary gives the Tukey counts computed by hand", {
  # x observed: 1, 2, 3, 4, 100; quartiles (type 7) 2 and 4, fences -1 and 7: one outlier, 20 %.
  # y observed: 2.5, 3, 3, 4, 5; quartiles 3 and 4, fences 1.5 and 5.5: none.
  table <- outlier_summary(toy_raw())
  expect_equal(table$variable, c("x", "y"))
  expect_equal(table$lower, c(-1, 1.5))
  expect_equal(table$upper, c(7, 5.5))
  expect_equal(table$n_outliers, c(1L, 0L))
  expect_equal(table$pct_outliers, c(20, 0))
  expect_equal(attr(table, "rows_flagged"), 1L)
})

test_that("the outlier plot keeps every outlier when it samples the other points", {
  x <- c(seq_len(100), 1000, 2000, NA)
  points <- outlier_plot_data(x, outlier_bounds(x), max_points = 10)
  expect_true(attr(points, "sampled"))
  expect_equal(nrow(points), 12L)
  expect_setequal(points$value[points$outlier], c(1000, 2000))
  # Deterministic, and the global random state is left untouched.
  set.seed(42)
  before <- .Random.seed
  expect_identical(outlier_plot_data(x, outlier_bounds(x), max_points = 10), points)
  expect_identical(.Random.seed, before)
})

# --- Module ----------------------------------------------------------------------

test_that("a removed column leaves dataset_clean data and var_types", {
  ds <- toy_raw()
  shiny::testServer(mod_preprocess_server, args = list(dataset = shiny::reactive(ds)), {
    session$setInputs(drop_cols = "g")
    out <- session$returned$dataset_clean()
    expect_false("g" %in% names(out$data))
    expect_false("g" %in% names(out$var_types))
    expect_true(validate_dataset(out))
    # Without g, rows 4 (y) and 6 (x) are incomplete.
    session$setInputs(drop_incomplete = TRUE)
    expect_equal(nrow(session$returned$dataset_clean()$data), 4L)
  })
})

test_that("recipe() reflects the settings", {
  ds <- toy_raw()
  shiny::testServer(mod_preprocess_server, args = list(dataset = shiny::reactive(ds)), {
    session$setInputs(outlier_method = "zscore", outlier_z = 2.5, outlier_action = "cap",
                      impute_quanti = "median", impute_quali = "missing_level",
                      rare_pct = 5, scale_method = "minmax", dummy_method = "k_minus_1",
                      dummy_max_cols = 20)
    r <- session$returned$recipe()
    expect_equal(r$outliers, list(method = "zscore", z = 2.5, action = "cap"))
    expect_equal(r$impute, list(quanti = "median", quali = "missing_level"))
    expect_equal(r$rare$threshold, 0.05)
    expect_equal(r$scale$method, "minmax")
    expect_equal(r$dummy, list(method = "k_minus_1", max_cols = 20))
    # An emptied or out-of-range numeric input keeps the default.
    session$setInputs(outlier_z = NA, rare_pct = 150, dummy_max_cols = 0)
    r <- session$returned$recipe()
    expect_equal(r$outliers$z, 3)
    expect_equal(r$rare$threshold, 0)
    expect_equal(r$dummy$max_cols, 50)
    session$setInputs(outlier_z = -1)
    expect_equal(session$returned$recipe()$outliers$z, 3)
  })
})

test_that("the preview fits the recipe on the whole clean dataset", {
  ds <- toy_raw()
  shiny::testServer(mod_preprocess_server, args = list(dataset = shiny::reactive(ds)), {
    session$setInputs(impute_quanti = "mean", scale_method = "standard",
                      dummy_method = "one_hot", outlier_action = "remove")
    fitted <- preview_fit()
    # Row 5 (x = 100) is removed; the other 5 rows remain.
    expect_equal(nrow(fitted$train), 5L)
    x <- fitted$train$x
    expect_equal(sum(x) / length(x), 0)
    expect_match(output$preview_info, "1 lignes retirées")
    expect_true(all(c("g_a", "g_b") %in% names(fitted$train)))
  })
})

test_that("the export writes the preview as a UTF-8 CSV", {
  ds <- toy_raw()
  shiny::testServer(mod_preprocess_server, args = list(dataset = shiny::reactive(ds)), {
    session$setInputs(dummy_method = "k_minus_1")
    path <- output$export
    exported <- utils::read.csv(path, fileEncoding = "UTF-8")
    expect_equal(dim(exported), dim(preview_fit()$train))
    expect_equal(names(exported), names(preview_fit()$train))
  })
})

test_that("students.csv is preprocessed and exported through the whole app", {
  path <- test_path("fixtures", "course", "students.csv")
  skip_if_not(file.exists(path), "course CSV not available")
  path <- normalizePath(path)
  upload <- data.frame(name = basename(path), size = file.size(path), type = "text/csv",
                       datapath = path)
  app <- shiny::shinyAppDir(test_path("..", ".."))
  shiny::testServer(app, {
    session$setInputs(`data-file` = upload, `data-na_codes` = "NA", `data-na_empty` = TRUE)
    session$setInputs(`preprocess-drop_cols` = c("Major2", "Major3"),
                      `preprocess-impute_quanti` = "median",
                      `preprocess-impute_quali` = "mode",
                      `preprocess-rare_pct` = 5,
                      `preprocess-scale_method` = "standard",
                      `preprocess-dummy_method` = "k_minus_1")
    expect_equal(output$`explore-dims`, "48 lignes, 11 colonnes")
    exported <- utils::read.csv(output$`preprocess-export`, fileEncoding = "UTF-8")
    expect_equal(nrow(exported), 48L)
    expect_false(anyNA(exported))
    expect_equal(sum(exported$Height) / 48, 0, tolerance = 1e-12)
  })
})

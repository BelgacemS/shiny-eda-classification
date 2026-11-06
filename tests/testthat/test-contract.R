toy_dataset <- function() {
  make_dataset(
    data.frame(x = c(1, 2, 3), g = c("a", "b", "a")),
    c(x = "quanti", g = "quali")
  )
}

upload <- function(path) {
  data.frame(name = basename(path), size = file.size(path), type = "text/csv", datapath = path)
}

toy_csv <- function() {
  path <- tempfile(fileext = ".csv")
  write.csv(data.frame(id = 1:5, g = c("a", "b", "a", "c", "b"), x = c(1.2, 3.4, 5.6, 7.8, 9)),
            path, row.names = FALSE)
  path
}

test_that("make_dataset converts quali columns to factors", {
  ds <- toy_dataset()
  expect_s3_class(ds$data$g, "factor")
  expect_true(validate_dataset(ds))
})

test_that("validate_dataset rejects inconsistent datasets without error", {
  ds <- toy_dataset()
  renamed <- ds
  names(renamed$var_types) <- c("x", "h")
  reordered <- ds
  reordered$var_types <- rev(ds$var_types)
  wrong_class <- ds
  wrong_class$var_types["g"] <- "quanti"
  unknown_type <- ds
  unknown_type$var_types["g"] <- "text"
  expect_false(validate_dataset(renamed))
  expect_false(validate_dataset(reordered))
  expect_false(validate_dataset(wrong_class))
  expect_false(validate_dataset(unknown_type))
  expect_false(validate_dataset(NULL))
  expect_false(validate_dataset(list(data = ds$data)))
})

test_that("id and ignore columns accept any class", {
  ds <- list(
    data = data.frame(code = c("a", "b"), day = as.Date("2024-01-01") + 0:1),
    var_types = c(code = "id", day = "ignore")
  )
  expect_true(validate_dataset(ds))
})

test_that("mod_data_server returns a valid dataset after an upload", {
  path <- toy_csv()
  shiny::testServer(mod_data_server, args = list(baac_sample = NULL), {
    expect_null(session$returned())
    session$setInputs(file = upload(path))
    ds <- session$returned()
    expect_true(validate_dataset(ds))
    expect_equal(dim(ds$data), c(5L, 3L))
    expect_equal(as.vector(ds$var_types), c("quanti", "quali", "quanti"))
  })
})

test_that("mod_preprocess_server passes the dataset through", {
  ds <- toy_dataset()
  shiny::testServer(mod_preprocess_server, args = list(dataset = shiny::reactive(ds)), {
    out <- session$returned
    expect_identical(out$dataset_clean(), ds)
    expect_identical(out$recipe(), default_recipe())
  })
})

test_that("fit_recipe and apply_recipe are identities with their final signatures", {
  ds <- toy_dataset()
  fitted <- fit_recipe(default_recipe(), ds$data, ds$var_types)
  expect_s3_class(fitted, "fitted_recipe")
  expect_identical(apply_recipe(fitted, ds$data), ds$data)
})

test_that("load_rds_or_null returns NULL for a missing file", {
  expect_null(load_rds_or_null(tempfile(fileext = ".rds")))
})

test_that("tabs 3 and 4 show the dimensions given by read.csv", {
  path <- test_path("fixtures", "course", "students.csv")
  if (!file.exists(path)) path <- toy_csv()
  # testServer() on an app runs from the app directory: the path must be absolute.
  path <- normalizePath(path)
  expected <- dim(read.csv(path))
  expected_text <- sprintf("%d lignes, %d colonnes", expected[1], expected[2])
  app <- shiny::shinyAppDir(test_path("..", ".."))
  shiny::testServer(app, {
    session$setInputs(`data-file` = upload(path))
    expect_equal(output$`explore-dims`, expected_text)
    expect_equal(output$`model-dims`, expected_text)
  })
})

test_that("the case study tab shows a message without final results", {
  shiny::testServer(mod_case_study_server,
                    args = list(baac_sample = NULL, final_results = NULL), {
    expect_match(output$status$html, "absent")
  })
})

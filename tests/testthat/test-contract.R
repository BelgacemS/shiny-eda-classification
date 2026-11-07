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

test_that("the default recipe leaves the data unchanged", {
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

write_bytes <- function(bytes) {
  path <- tempfile(fileext = ".csv")
  writeBin(bytes, path)
  path
}

test_that("a non UTF-8 file is refused instead of being truncated", {
  latin1 <- write_bytes(c(charToRaw("x,ville\n1,Paris\n2,Orl"), as.raw(0xe9),
                          charToRaw("ans\n3,Lyon\n4,Nice\n")))
  expect_error(read_table_file(latin1), "UTF-8")
  shiny::testServer(mod_data_server, args = list(baac_sample = NULL), {
    session$setInputs(file = upload(latin1))
    expect_null(session$returned())
  })
})

test_that("a UTF-8 BOM and a missing final newline are handled", {
  bom <- write_bytes(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw("x,y\n1,a\n2,b")))
  # read.csv() only warns about the incomplete final line (message depends on the locale).
  expect_warning(data <- read_table_file(bom))
  expect_equal(names(data), c("x", "y"))
  expect_equal(nrow(data), 2L)
})

test_that("the contract holds on a data.table", {
  dt <- data.table::data.table(x = c(1, 2, 3), g = c("a", "b", "a"))
  ds <- make_dataset(dt, c(x = "quanti", g = "quali"))
  expect_true(validate_dataset(ds))
  bad <- list(data = dt, var_types = c(x = "quanti", g = "quali"))
  expect_false(validate_dataset(bad))
})

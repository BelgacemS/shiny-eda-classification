write_text <- function(text, ext = ".csv") {
  path <- tempfile(fileext = ext)
  writeBin(charToRaw(text), path)
  path
}

upload_file <- function(path) {
  data.frame(name = basename(path), size = file.size(path), type = "text/csv", datapath = path)
}

# --- Reading ---------------------------------------------------------------

test_that("guess_separator picks the most frequent candidate of the first line", {
  expect_equal(guess_separator("a,b,c"), ",")
  expect_equal(guess_separator(c("", "a;b;c")), ";")
  expect_equal(guess_separator("a\tb\tc"), "\t")
  expect_equal(guess_separator("a|b"), "|")
  expect_equal(guess_separator("a;b;c,d"), ";")
  expect_equal(guess_separator("single"), ",")
})

test_that("a TSV file is read with the automatic separator", {
  path <- write_text("x\tlabel\n1.5\tun\n2\tdeux\n", ".tsv")
  data <- read_table_file(path)
  expect_equal(names(data), c("x", "label"))
  expect_equal(data$x, c(1.5, 2))
  expect_equal(data$label, c("un", "deux"))
})

test_that("a semicolon file with decimal commas gives numeric columns", {
  path <- write_text("taille;poids;ville\n1,75;70,5;Paris\n1,62;58;Lyon\n")
  data <- read_table_file(path, dec = ",")
  expect_equal(data$taille, c(1.75, 1.62))
  expect_equal(data$poids, c(70.5, 58))
  expect_type(data$ville, "character")
})

test_that("a separator equal to the decimal mark is refused", {
  path <- write_text("a,b\n1,2\n")
  expect_error(read_table_file(path, sep = ",", dec = ","), "différents")
  expect_error(read_table_file(path, dec = ","), "différents")
})

test_that("header = FALSE keeps the first line as data", {
  path <- write_text("a,b\n1,2\n3,4\n")
  data <- read_table_file(path, header = FALSE)
  expect_equal(dim(data), c(3L, 2L))
  expect_equal(data$V1, c("a", "1", "3"))
})

test_that("a Windows-1252 file is read with the matching encoding", {
  path <- tempfile(fileext = ".csv")
  writeBin(c(charToRaw("x,ville\n1,Paris\n2,Orl"), as.raw(0xe9), charToRaw("ans\n")), path)
  expect_error(read_table_file(path), "UTF-8")
  data <- read_table_file(path, encoding = "CP1252")
  expect_equal(data$ville, c("Paris", "Orléans"))
  expect_true(all(validUTF8(data$ville)))
})

test_that("an empty file is refused with a message", {
  expect_error(read_table_file(write_text("")), "vide")
})

# --- Missing codes ---------------------------------------------------------

# Repeated values, so that no column is typed id.
toy_codes <- "x,g,n\n1,a,0\n-1,N/A,5\n.,b,0\n1,,7\n.,a,5\n"

test_that("declared codes become NA and numeric columns stay numeric", {
  data <- read_table_file(write_text(toy_codes), na_codes = c("-1", "N/A", ".", ""))
  expect_equal(data$x, c(1, NA, NA, 1, NA))
  expect_equal(data$g, c("a", NA, "b", NA, "a"))
  expect_equal(data$n, c(0L, 5L, 0L, 7L, 5L))
})

test_that("codes restricted to some columns leave the others untouched", {
  data <- read_table_file(write_text(toy_codes), na_codes = c("-1", ".", "0"), na_cols = "x")
  expect_equal(data$x, c(1, NA, NA, 1, NA))
  expect_equal(data$n, c(0L, 5L, 0L, 7L, 5L))
  # Without codes, the empty cell stays an empty string in a text column.
  expect_equal(data$g, c("a", "N/A", "b", "", "a"))
})

test_that("NA and empty cells stay missing everywhere when codes are restricted", {
  path <- write_text("x,y,g\n1,2.5,a\n-1,NA,b\n3,4.1,\n-1,2.5,a\n")
  data <- read_table_file(path, na_codes = c("NA", "", "-1"), na_cols = "x")
  expect_equal(data$x, c(1L, NA, 3L, NA))
  expect_equal(data$y, c(2.5, NA, 4.1, 2.5))
  expect_equal(data$g, c("a", "b", NA, "a"))
})

test_that("codes match the cell text with surrounding spaces ignored", {
  data <- read_table_file(write_text("x,y\n1, -1\n2,3\n"), na_codes = "-1")
  expect_equal(data$y, c(NA, 3L))
})

test_that("unknown columns in na_cols are ignored", {
  data <- read_table_file(write_text(toy_codes), na_codes = ".", na_cols = c("x", "absent"))
  expect_equal(data$x, c(1L, -1L, NA, 1L, NA))
})

# --- Variables table -------------------------------------------------------

test_that("summarize_variables gives exact NA percentages and counts", {
  data <- data.frame(
    x = c(1.5, NA, 3, NA, 5, 6),
    g = c("a", "b", NA, "a", "b", "a"),
    row = 1:6
  )
  types <- detect_types(data)
  ds <- make_dataset(data, types)
  table <- summarize_variables(ds, types)
  expect_equal(table$variable, c("x", "g", "row"))
  expect_equal(table$pct_na, c(100 * 2 / 6, 100 / 6, 0))
  expect_equal(table$n_values, c(4L, 2L, 6L))
  expect_equal(table$class, c("numeric", "factor", "integer"))
  expect_equal(table$alert, c("", "", "identifiant possible"))
})

test_that("a quali column above the cardinality threshold is flagged", {
  data <- data.frame(city = rep(sprintf("V%02d", 1:25), 2), x = 1:50 %% 7)
  types <- detect_types(data)
  table <- summarize_variables(make_dataset(data, types), types)
  expect_equal(unname(types["city"]), "quali")
  expect_equal(table$alert, c("forte cardinalité", ""))
})

# --- Type changes ----------------------------------------------------------

test_that("change_types converts classes and rebuilds from the data as read", {
  data <- data.frame(x = c(1.5, 2, 3), code = c("1", "2", "z"), g = c("a", "b", "a"))
  types <- detect_types(data)
  to_quali <- change_types(data, types, "x", "quali")$dataset
  expect_s3_class(to_quali$data$x, "factor")
  expect_true(validate_dataset(to_quali))
  back <- change_types(data, to_quali$var_types, "x", "quanti")$dataset
  expect_identical(back$data$x, data$x)

  partial <- change_types(data, types, "code", "quanti")
  expect_equal(partial$dataset$data$code, c(1, 2, NA))
  expect_equal(partial$n_lost, c(code = 1L))
  expect_error(change_types(data, types, "g", "quanti"), "aucune valeur numérique")
  expect_error(change_types(data, types, "g", "texte"), "type inconnu")
})

# --- Module ----------------------------------------------------------------

test_that("a type change reaches dataset()$var_types and the column class", {
  path <- write_text("x,g\n1.5,a\n2,b\n3,a\n")
  shiny::testServer(mod_data_server, args = list(baac_sample = NULL), {
    session$setInputs(file = upload_file(path))
    expect_equal(unname(session$returned()$var_types), c("quanti", "quali"))

    session$setInputs(type_vars = "x", type_new = "quali", type_apply = 1)
    ds <- session$returned()
    expect_equal(unname(ds$var_types["x"]), "quali")
    expect_s3_class(ds$data$x, "factor")
    expect_true(validate_dataset(ds))

    session$setInputs(type_vars = "g", type_new = "quanti", type_apply = 2)
    expect_identical(session$returned(), ds)
  })
})

test_that("changing an import option re-reads the current file", {
  path <- write_text("a;b\n1,5;x\n2,25;y\n1,5;y\n")
  shiny::testServer(mod_data_server, args = list(baac_sample = NULL), {
    session$setInputs(file = upload_file(path))
    expect_equal(session$returned()$data$a, factor(c("1,5", "2,25", "1,5")))
    session$setInputs(dec = ",")
    expect_equal(session$returned()$data$a, c(1.5, 2.25, 1.5))
    session$setInputs(sep = "tab")
    expect_equal(dim(session$returned()$data), c(3L, 1L))
  })
})

test_that("a re-read keeps the types set by hand", {
  path <- write_text("a,b\n1,x\n2,y\n1,.\n2,x\n")
  shiny::testServer(mod_data_server, args = list(baac_sample = NULL), {
    session$setInputs(file = upload_file(path))
    session$setInputs(type_vars = "a", type_new = "quali", type_apply = 1)
    session$setInputs(na_codes = ".")
    ds <- session$returned()
    expect_equal(unname(ds$var_types), c("quali", "quali"))
    expect_s3_class(ds$data$a, "factor")
    expect_equal(ds$data$b, factor(c("x", "y", NA, "x")))
    # A new file starts again from the detected types.
    session$setInputs(file = upload_file(write_text("a,b\n1,x\n2,y\n1,y\n")))
    expect_equal(unname(session$returned()$var_types), c("quanti", "quali"))
  })
})

test_that("missing codes declared in the UI reach dataset()", {
  path <- write_text(toy_codes)
  shiny::testServer(mod_data_server, args = list(baac_sample = NULL), {
    session$setInputs(file = upload_file(path), na_empty = TRUE)
    expect_equal(unname(session$returned()$var_types["x"]), "quali")
    session$setInputs(na_codes = c("-1", "N/A", "."))
    ds <- session$returned()
    expect_equal(ds$data$x, c(1, NA, NA, 1, NA))
    expect_equal(ds$data$g, factor(c("a", NA, "b", NA, "a")))
  })
})

test_that("the BAAC button loads the sample, and is disabled without it", {
  sample <- data.frame(y = factor(c("non", "oui", "non")), v = c(30, 50, 80))
  shiny::testServer(mod_data_server, args = list(baac_sample = sample), {
    expect_no_match(output$baac_ui$html, "disabled")
    session$setInputs(baac = 1)
    ds <- session$returned()
    expect_true(validate_dataset(ds))
    expect_identical(ds$data, sample)
  })
  shiny::testServer(mod_data_server, args = list(baac_sample = NULL), {
    expect_match(output$baac_ui$html, "disabled")
    expect_match(output$baac_ui$html, "absent")
    session$setInputs(baac = 1)
    expect_null(session$returned())
  })
})

load_in_module <- function(path, ...) {
  out <- NULL
  shiny::testServer(mod_data_server, args = list(baac_sample = NULL), {
    # The defaults of the UI: testServer leaves the inputs NULL.
    session$setInputs(file = upload_file(path), na_codes = "NA", na_empty = TRUE, ...)
    out <<- session$returned()
  })
  out
}

test_that("the course files load without error", {
  for (name in c("students.csv", "chiens.csv")) {
    path <- test_path("fixtures", "course", name)
    skip_if_not(file.exists(path), "course CSV not available")
    ds <- load_in_module(path)
    expect_true(validate_dataset(ds))
    expect_equal(dim(ds$data), dim(read.csv(path)))
  }
})

test_that("a 10 MB synthetic CSV loads without error", {
  n <- 300000
  set.seed(1)
  data <- data.frame(
    id = sprintf("ID%07d", seq_len(n)),
    x = round(rnorm(n), 4),
    y = round(runif(n, 0, 1000), 2),
    g = sample(c("alpha", "beta", "gamma", "delta"), n, replace = TRUE),
    k = sample(0:9, n, replace = TRUE)
  )
  path <- tempfile(fileext = ".csv")
  write.csv(data, path, row.names = FALSE)
  expect_gt(file.size(path), 10e6)
  ds <- load_in_module(path)
  expect_true(validate_dataset(ds))
  expect_equal(dim(ds$data), c(n, 5L))
  expect_equal(unname(ds$var_types), c("id", "quanti", "quanti", "quali", "quanti"))
})

test_that("the deployed BAAC sample loads through the button", {
  path <- test_path("..", "..", "data", "accidents_sample.rds")
  skip_if_not(file.exists(path), "BAAC sample not available")
  sample <- readRDS(path)
  shiny::testServer(mod_data_server, args = list(baac_sample = sample), {
    session$setInputs(baac = 1)
    ds <- session$returned()
    expect_true(validate_dataset(ds))
    expect_equal(dim(ds$data), dim(sample))
  })
})

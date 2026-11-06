# Tabs 1 to 4 must work on any CSV: BAAC column names may only appear in the
# files of tab 5 (mod_case_study.R, case_study_*.R).

BAAC_COLUMNS <- c("Num_Acc", "grav", "catu", "catv", "an_nais", "hrmn", "id_vehicule", "id_usager")

find_baac_names <- function(dir) {
  files <- list.files(dir, pattern = "\\.[rR]$", full.names = TRUE)
  files <- files[!grepl("^(mod_case_study|case_study_.*)\\.[rR]$", basename(files))]
  pattern <- paste0("\\b(", paste(BAAC_COLUMNS, collapse = "|"), ")\\b")
  hits <- character(0)
  for (f in files) {
    lines <- readLines(f, encoding = "UTF-8", warn = FALSE)
    found <- grep(pattern, lines, ignore.case = TRUE, perl = TRUE)
    hits <- c(hits, sprintf("%s:%d", basename(f), found))
  }
  hits
}

test_that("no BAAC column name appears in R/ outside the case study files", {
  hits <- find_baac_names(test_path("..", "..", "R"))
  expect(length(hits) == 0, paste("BAAC column names found in:", paste(hits, collapse = ", ")))
})

test_that("the check catches a BAAC name and spares the case study files", {
  dir <- withr::local_tempdir()
  writeLines("x <- data$GRAV", file.path(dir, "mod_explore.R"))
  writeLines("y <- data$Num_Acc", file.path(dir, "case_study_models.R"))
  writeLines("z <- gravity + id_usager2", file.path(dir, "fct_other.R"))
  expect_equal(find_baac_names(dir), "mod_explore.R:1")
})

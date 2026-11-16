# Without NOT_CRAN, testthat turns a missing package into a skip instead of a failure.
Sys.setenv(NOT_CRAN = "true")

# Shiny sources R/ automatically when the app starts, testthat does not.
for (f in list.files(test_path("..", "..", "R"), pattern = "\\.[rR]$", full.names = TRUE)) {
  source(f, encoding = "UTF-8")
}

# Ids of the inputs inside the closed advanced sections of a UI, and outside them.
advanced_input_ids <- function(ui) {
  html <- htmltools::renderTags(ui)$html
  sections <- regmatches(html, gregexpr("(?s)<details.*?</details>", html, perl = TRUE))[[1]]
  ids <- function(x) unlist(regmatches(x, gregexpr('(?<= id=")[^"]+', x, perl = TRUE)))
  list(sections = sections, inside = ids(sections),
       outside = ids(gsub("(?s)<details.*?</details>", "", html, perl = TRUE)))
}

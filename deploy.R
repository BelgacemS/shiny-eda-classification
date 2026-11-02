# Deploy the app to shinyapps.io from a clean main branch.
# Usage, from the project root: Rscript deploy.R

app_name <- "shiny-eda-classification"

git <- function(...) system2("git", c(...), stdout = TRUE)

branch <- git("rev-parse", "--abbrev-ref", "HEAD")
if (branch != "main") {
  stop("Deploy from main only (current branch: ", branch, ").", call. = FALSE)
}
if (length(git("status", "--porcelain")) > 0) {
  stop("Uncommitted changes: commit or stash them before deploying.", call. = FALSE)
}

app_files <- c(
  "app.R",
  "renv.lock",
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  list.files("data", pattern = "\\.rds$", full.names = TRUE)
)

rsconnect::deployApp(appDir = ".", appName = app_name, appFiles = app_files)

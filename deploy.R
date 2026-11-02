# Deploy the app to shinyapps.io from main, up to date with origin and without local changes.
# Usage, from the project root: Rscript deploy.R

app_name <- "shiny-eda-classification"

git <- function(...) {
  out <- suppressWarnings(system2("git", c(...), stdout = TRUE, stderr = TRUE))
  if (!is.null(attr(out, "status"))) {
    stop("git ", paste(...), " failed:\n", paste(out, collapse = "\n"), call. = FALSE)
  }
  out
}

branch <- git("rev-parse", "--abbrev-ref", "HEAD")
if (branch != "main") {
  stop("Deploy from main only (current branch: ", branch, ").", call. = FALSE)
}
if (length(git("status", "--porcelain")) > 0) {
  stop("Uncommitted changes: commit or stash them before deploying.", call. = FALSE)
}
invisible(git("fetch", "--quiet", "origin", "main"))
if (git("rev-parse", "HEAD") != git("rev-parse", "origin/main")) {
  stop("Local main differs from origin/main: pull, or drop local commits.", call. = FALSE)
}

app_files <- c(
  "app.R",
  "renv.lock",
  list.files("R", pattern = "\\.[rR]$", full.names = TRUE),
  list.files("data", pattern = "\\.rds$", full.names = TRUE)
)
message("Files to deploy:\n", paste0("  ", app_files, collapse = "\n"))

# rsconnect/ is not versioned: forceUpdate makes a machine without a deployment
# record update the existing app of the same name instead of failing.
rsconnect::deployApp(
  appDir = ".", appName = app_name, appFiles = app_files, forceUpdate = TRUE
)

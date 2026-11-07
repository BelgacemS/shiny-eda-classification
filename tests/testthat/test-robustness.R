# Robustness of the whole app: tricky files go through tabs 1 to 3 with two
# preprocessing settings, and every output must either render or stop with a
# planned message (validate / req), never with a raw R error.

OUTPUTS <- c(
  paste0("data-", c("dims", "variables", "preview", "baac_ui")),
  paste0("preprocess-", c("clean_info", "outlier_info", "outlier_table", "outlier_plot",
                          "outlier_plot_note", "preview_info", "preview_messages", "preview",
                          "export")),
  "explore-dims", "model-dims"
)
UNI_OUTPUTS <- paste0("explore-uni-", c("var_type", "excluded", "indicators", "quanti_table",
                                        "bars", "ecdf", "boxplot", "histogram",
                                        "cumulative_curve", "quali_table", "columns", "pie"))
BI_OUTPUTS <- paste0("explore-bi-", c("pair", "pair_info", "indices_qq", "scatter", "scatter_note",
                                      "backback", "scatter_hist", "scatter_hist_note",
                                      "indices_nc", "boxes", "groups", "contingency",
                                      "indices_cc", "chi2_warning", "profiles", "profile_table",
                                      "associations"))

# "ok", "planned" (validate or req) or the message of a raw error.
output_status <- function(session, name) {
  tryCatch({
    session$getOutput(name)
    "ok"
  }, error = function(e) {
    if (inherits(e, "shiny.silent.error") || inherits(e, "validation")) "planned" else conditionMessage(e)
  })
}

tricky_files <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  write <- function(name, text) {
    path <- file.path(dir, name)
    writeBin(charToRaw(enc2utf8(text)), path)
    path
  }
  set.seed(11)
  n <- 30
  lines <- function(df) paste(c(paste(names(df), collapse = ","),
                                apply(df, 1, paste, collapse = ",")), collapse = "\n")
  list(
    constants = write("constants.csv", lines(data.frame(
      x = round(rnorm(n), 2), k = rep(0.1, n), kint = rep(3, n),
      g = sample(c("a", "b"), n, TRUE), seul = rep("z", n)))),
    all_missing = write("all_missing.csv", paste0("x,vide,na\n",
      paste(sprintf("%d,,NA", 1:12), collapse = "\n"))),
    one_row = write("one_row.csv", "x,y,g\n1.5,2,a\n"),
    two_rows = write("two_rows.csv", "x,y,g\n1,1,a\n1,1,a\n"),
    accents = write("accents.csv", paste0("âge moyen,Région,poids\n",
      paste(sprintf("%d,%s,%.1f", sample(20:80, n, TRUE),
                    sample(c("Île-de-France", "Nouvelle-Aquitaine", "Occitanie"), n, TRUE),
                    runif(n, 50, 90)), collapse = "\n"))),
    many_levels = write("many_levels.csv", lines(data.frame(
      q = round(runif(80), 3), l30 = sprintf("L%02d", sample(1:30, 80, TRUE)),
      l100 = sprintf("M%03d", 1:80), y = sample(c("oui", "non"), 80, TRUE)))),
    semicolon = write("semicolon.csv", paste0("a;b;c\n",
      paste(sprintf("%s;%s;%s", sub(".", ",", sprintf("%.2f", runif(n)), fixed = TRUE),
                    sub(".", ",", sprintf("%.1f", runif(n, 0, 9)), fixed = TRUE),
                    sample(c("u", "v", "w"), n, TRUE)), collapse = "\n"))),
    one_column = write("one_column.csv", "x\n1\n2\n2\n3\n"),
    only_quali = write("only_quali.csv", lines(data.frame(
      a = sample(c("p", "q"), n, TRUE), b = sample(c("r", "s", "t"), n, TRUE)))),
    duplicate_names = write("duplicate_names.csv", "x,x,y\n1,2,a\n3,4,b\n5,6,a\n7,8,b\n"),
    outliers_only = write("outliers_only.csv", lines(data.frame(
      x = c(rep(1, n - 1), 1000), y = c(rep(2, n - 2), -500, 2))))
  )
}

SETTINGS <- list(
  defaults = list(),
  aggressive = list(
    `preprocess-drop_incomplete` = TRUE, `preprocess-outlier_method` = "zscore",
    `preprocess-outlier_z` = 1, `preprocess-outlier_action` = "remove",
    `preprocess-impute_quanti` = "mean", `preprocess-impute_quali` = "missing_level",
    `preprocess-rare_pct` = 30, `preprocess-scale_method` = "standard",
    `preprocess-dummy_method` = "one_hot", `preprocess-dummy_max_cols` = 2
  )
)

upload_input <- function(path) {
  data.frame(name = basename(path), size = file.size(path), type = "text/csv", datapath = path)
}

test_that("the harness tells a raw error from a planned stop", {
  app <- shiny::shinyApp(shiny::fluidPage(), function(input, output, session) {
    output$raw <- shiny::renderText(stop("boom"))
    output$val <- shiny::renderText(shiny::validate("message prévu"))
    output$rq <- shiny::renderText(shiny::req(FALSE))
    output$ok <- shiny::renderText("x")
  })
  shiny::testServer(app, {
    expect_equal(output_status(session, "raw"), "boom")
    expect_equal(output_status(session, "val"), "planned")
    expect_equal(output_status(session, "rq"), "planned")
    expect_equal(output_status(session, "ok"), "ok")
  })
})

test_that("every output of tabs 1 to 3 renders or stops with a planned message", {
  files <- tricky_files()
  app <- shiny::shinyAppDir(test_path("..", ".."))
  failures <- character(0)
  for (file in names(files)) {
    for (setting in names(SETTINGS)) {
      shiny::testServer(app, {
        session$setInputs(`data-file` = upload_input(files[[file]]), `data-na_codes` = "NA",
                          `data-na_empty` = TRUE)
        if (file == "semicolon") session$setInputs(`data-dec` = ",")
        if (length(SETTINGS[[setting]]) > 0) do.call(session$setInputs, SETTINGS[[setting]])
        check <- function(names, context) {
          for (name in names) {
            status <- output_status(session, name)
            if (!status %in% c("ok", "planned")) {
              failures <<- c(failures, sprintf("%s / %s / %s / %s: %s", file, setting, context,
                                               name, status))
            }
          }
        }
        check(OUTPUTS, "tabs")

        # testServer() on an app evaluates in the server function: `preprocess` is its local.
        clean <- tryCatch(preprocess$dataset_clean()$var_types, error = function(e) NULL)
        vars <- names(clean)[clean %in% c("quanti", "quali")]
        quanti <- names(clean)[clean == "quanti"]
        if (length(quanti) > 0) {
          session$setInputs(`preprocess-outlier_var` = quanti[1])
          check(c("preprocess-outlier_plot", "preprocess-outlier_plot_note"), "outlier plot")
        }
        for (v in vars) {
          session$setInputs(`explore-uni-var` = v, `explore-uni-table_mode` = "auto",
                            `explore-uni-class_rule` = "fd", `explore-uni-hist_scale` = "count",
                            `explore-uni-show_density` = TRUE)
          check(UNI_OUTPUTS, paste("uni", v))
        }
        pairs <- if (length(vars) > 1) utils::combn(vars, 2, simplify = FALSE) else list()
        for (pair in pairs) {
          session$setInputs(`explore-bi-x` = pair[1], `explore-bi-y` = pair[2],
                            `explore-bi-target` = pair[1], `explore-bi-profile` = "dodge")
          check(BI_OUTPUTS, paste("bi", pair[1], pair[2]))
        }
      })
    }
  }
  expect(length(failures) == 0,
         paste0("raw errors in outputs:\n", paste(failures, collapse = "\n")))
})

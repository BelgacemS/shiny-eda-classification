# shiny-eda-classification

R Shiny application for exploratory data analysis and binary classification on any tabular file (CSV or TSV), with a case study on the French road accident database (BAAC, ONISR). The interface is in French.

Work in progress: see `PLAN.md` for the roadmap.

## Requirements

- R 4.5.2, the latest version reported to be accepted by shinyapps.io (macOS: `R-4.5.2-arm64.pkg` from CRAN; Windows: `R-4.5.2-win.exe` from the CRAN previous releases, commands run in Git Bash)
- Windows: the R installer does not add R to the `PATH`. Add `C:\Program Files\R\R-4.5.2\bin` so that `Rscript` works in Git Bash.
- Package versions are pinned with `renv`.

## Setup

```bash
git clone git@github.com:BelgacemS/shiny-eda-classification.git
cd shiny-eda-classification
Rscript -e 'renv::restore()'
```

## Run

```bash
Rscript -e 'shiny::runApp(launch.browser = TRUE)'
```

## Test

```bash
Rscript -e 'testthat::test_dir("tests/testthat")'
```

The property tests draw 150 random data sets per property; a deeper run, about two minutes:

```bash
PROPERTY_DRAWS=2000 Rscript -e 'testthat::test_file("tests/testthat/test-properties.R")'
```

## Deploy

Deployment to shinyapps.io runs from a clean `main` branch. The account must first be registered once per machine with `rsconnect::setAccountInfo()` (token from the shinyapps.io dashboard, never committed).

```bash
Rscript deploy.R
```

Free plan limits that apply to this app (checked on 2026-11-06):

- memory: `large` instance, 1024 MB (`rsconnect::showProperties()`, shinyapps.io guide);
- bundle size: 1 GB;
- active hours: 25 for the account (5 applications at most), shinyapps.io pricing page;
- instance idle timeout: 5 minutes (`application.instances.idle-threshold`, set with `rsconnect::setProperty()`). On 2026-11-06 the value read back was still 15; on 2026-11-08 `rsconnect::showProperties()` returns 5 and the instance stopped 5 to 6 minutes after the last session closed (`docker_container_mem` series), so each wake-up costs about 5 minutes of active hours.

Memory and active hours of the deployed app (series names from the shinyapps.io metrics guide). `total_rss` is in bytes, for the whole instance, one value per minute; run the first command within the hour after the test. `showUsage()` counts this app only, not the other apps of the account:

```bash
Rscript -e 'rsconnect::showMetrics("docker_container_mem", "total_rss", appName = "shiny-eda-classification", from = "1h", interval = "1m")'
Rscript -e 'sum(rsconnect::showUsage(appName = "shiny-eda-classification", from = "30d", interval = "1d")$hours)'
```

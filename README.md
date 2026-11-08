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

Free plan limits that apply to this app, each with its date and source:

- memory: `large` instance, 1024 MB (`rsconnect::showProperties()` and shinyapps.io guide, checked on 2026-11-08);
- bundle size: 1 GB on the Free and Starter plans (shinyapps.io guide, applications page, checked on 2026-11-08);
- active hours: 25 for the account (5 applications at most), shinyapps.io home page plan table (checked on 2026-11-08); consumption is read with the `showUsage()` command below;
- instance idle timeout: 5 minutes (`application.instances.idle-threshold`, set with `rsconnect::setProperty()` on 2026-11-06). The value read back right after the change was still 15, but it applied from the next instance start: in the container start and stop events of `rsconnect::showLogs()`, the instance ran 15.7 minutes on the afternoon of 2026-11-06, then stopped about 6 minutes after the last session from that evening on. Active hours count instance uptime, so a visit costs its own length plus about 6 minutes.

Intended usage on the free plan: one person at a time. A single R process serves every session (`application.shiny.scheduler.max.processes = 1` in `rsconnect::showProperties()`, checked on 2026-11-08), so sessions share the 1024 MB and trainings run one after another. Measured on 2026-11-08 with the `docker_container_mem` series below: 832.5 MiB after two sessions had each trained a logistic regression and a random forest on 20 000 BAAC rows (J4), then a high-water mark of 921.4 MiB (per-minute series) over two sessions run one after the other in the same R process, each training the three models twice and computing the importances twice, memory not being released between them (J5). Two sessions at the same time were not measured and would likely exceed the limit.

Memory and active hours of the deployed app (series names from the shinyapps.io metrics guide). `total_rss` is in bytes, for the whole instance, one value per minute; run the first command within the hour after the test. `showUsage()` counts this app only, not the other apps of the account:

```bash
Rscript -e 'rsconnect::showMetrics("docker_container_mem", "total_rss", appName = "shiny-eda-classification", from = "1h", interval = "1m")'
Rscript -e 'sum(rsconnect::showUsage(appName = "shiny-eda-classification", from = "30d", interval = "1d")$hours)'
```

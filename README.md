# shiny-eda-classification

R Shiny application for exploratory data analysis and binary classification on any tabular file (CSV or TSV), with a case study on the French road accident database (BAAC, ONISR). The interface is in French.

Work in progress: see `PLAN.md` for the roadmap.

## Requirements

- R 4.6 (macOS: `brew install --cask r-app`; Windows: installer from CRAN, commands run in Git Bash)
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

## Deploy

Deployment to shinyapps.io runs from a clean `main` branch. The account must first be registered once per machine with `rsconnect::setAccountInfo()` (token from the shinyapps.io dashboard, never committed).

```bash
Rscript deploy.R
```

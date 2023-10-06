r <- getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

packages <- c(
  "cowplot",
  "dataMaid",
  "data.table",
  "dplyr",
  "estimatr",
  "foreign",
  "ggdist",
  "ggplot2",
  "gtools",
  "grf",
  "kableExtra",
  "modelsummary",
  "readr",
  "reshape2",
  "reticulate",
  "stringr",
  "tidyr",
  "xtable")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
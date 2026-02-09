# R/00_utils/packages.R — Load packages; install if missing
# Input:  none
# Output: none (side effect: attach packages)

need_pkg <- function(pkgs) {
  repos <- getOption("repos")
  if (is.null(repos) || repos["CRAN"] == "@CRAN@") {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      message("Installing ", p, " ...")
      install.packages(p)
    }
  }
  invisible(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))
}

suppressPackageStartupMessages({
  need_pkg(c(
    "dplyr", "readr", "stringr", "tibble", "tidyr", "lubridate",
    "ggplot2", "did", "fixest", "knitr"
  ))
})

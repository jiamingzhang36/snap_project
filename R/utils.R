# Utilities (comments in English)
need_pkg <- function(pkgs){
  repos <- getOption("repos")
  if (is.null(repos) || repos["CRAN"] == "@CRAN@") {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }
  for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  invisible(lapply(pkgs, require, character.only = TRUE))
}
suppressPackageStartupMessages(need_pkg(c(
  "dplyr","readr","stringr","tibble","tidyr","ggplot2","did","did2s","Matrix"
)))
fix_name <- function(x){
  x |>
    stringr::str_squish() |>
    stringr::str_to_title() |>
    stringr::str_replace_all("\\bSt\\.?\\s","St. ") |>
    stringr::str_remove("\\s*County\\b")
}
safe_dir <- function(p){ if(!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE); p }

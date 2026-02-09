# R/00_utils/helpers_io.R — Read/write and column checks
# Input:  none (defines functions)
# Output: none

#' Safe read CSV with required column check
read_panel_csv <- function(path, required = NULL) {
  if (!file.exists(path)) stop("File not found: ", path)
  x <- readr::read_csv(path, show_col_types = FALSE)
  if (!is.null(required)) {
    miss <- setdiff(required, names(x))
    if (length(miss) > 0) stop("Missing columns in ", path, ": ", paste(miss, collapse = ", "))
  }
  x
}

#' Save RDS to derived/ and create dir if needed
save_derived <- function(obj, name, dir_derived = get("DIR_DERIVED", envir = .GlobalEnv)) {
  path <- file.path(dir_derived, name)
  if (!dir.exists(dir_derived)) dir.create(dir_derived, recursive = TRUE)
  saveRDS(obj, path)
  invisible(path)
}

#' Load RDS from derived/
load_derived <- function(name, dir_derived = get("DIR_DERIVED", envir = .GlobalEnv)) {
  path <- file.path(dir_derived, name)
  if (!file.exists(path)) stop("Derived object not found: ", path)
  readRDS(path)
}

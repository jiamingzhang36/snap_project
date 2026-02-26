# R/01_build/02_clean_laus.R
# Input:  data_clean/fap_laus_merged_MI.csv (+ optional fap_laus_rucc_svi.csv) or snap_laus_with_policy / panel_with_acs
# Output: data/derived/panel_with_laus.rds — FAP + LAUS + RUCC/SVI + policy (waiver_covered, time_limit_proxy)

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)
source("R/00_utils/helpers_policy.R", local = TRUE)

path_merged   <- file.path(DIR_DATA_CLEAN, "fap_laus_merged_MI.csv")
path_rucc_svi <- file.path(DIR_DATA_CLEAN, "fap_laus_rucc_svi.csv")
path_snap     <- file.path(DIR_DATA_CLEAN, "snap_laus_with_policy.csv")
path_acs      <- file.path(DIR_DATA_CLEAN, "panel_with_acs.csv")

# Helper: clean county for policy (title case, no "County" suffix)
clean_county <- function(x) {
  x <- as.character(x)
  x <- stringr::str_remove(x, "\\s*County\\b")
  x <- stringr::str_squish(x)
  x <- stringr::str_to_title(x)
  x
}

if (file.exists(path_merged)) {
  d <- readr::read_csv(path_merged, show_col_types = FALSE)
  d <- d %>%
    dplyr::mutate(
      county_id = sprintf("%05d", as.integer(.data$county_id)),
      year  = as.integer(.data$year),
      month = as.integer(.data$month),
      county = clean_county(dplyr::coalesce(.data$county, .data$county_key))
    )
  if (file.exists(path_rucc_svi)) {
    rs <- readr::read_csv(path_rucc_svi, show_col_types = FALSE) %>%
      dplyr::mutate(county_id = sprintf("%05d", as.integer(.data$county_id))) %>%
      dplyr::select("county_id", "year", "month", "rucc_code", "rucc_desc", "urban_dummy", "svi_year", "svi_total")
    d <- d %>% dplyr::select(-dplyr::any_of(c("rucc_code", "rucc_desc", "urban_dummy", "svi_year", "svi_total"))) %>%
      dplyr::left_join(rs, by = c("county_id", "year", "month"))
  }
  d <- d %>%
    dplyr::filter(.data$county %in% MI_83) %>%
    add_waiver_policy()
  if (!dir.exists(DIR_DERIVED)) dir.create(DIR_DERIVED, recursive = TRUE)
  saveRDS(d, file.path(DIR_DERIVED, "panel_with_laus.rds"))
  message("Wrote data/derived/panel_with_laus.rds (built from fap_laus + RUCC/SVI + policy)  nrow = ", nrow(d))
} else if (file.exists(path_snap)) {
  d <- readr::read_csv(path_snap, show_col_types = FALSE)
  saveRDS(d, file.path(DIR_DERIVED, "panel_with_laus.rds"))
  message("Wrote data/derived/panel_with_laus.rds from snap_laus_with_policy.csv  nrow = ", nrow(d))
} else if (file.exists(path_acs)) {
  d <- readr::read_csv(path_acs, show_col_types = FALSE) %>%
    dplyr::mutate(
      county = clean_county(dplyr::coalesce(.data$county, .data$county_key)),
      county_id = sprintf("%05d", as.integer(.data$county_id))
    ) %>%
    dplyr::filter(.data$county %in% MI_83) %>%
    add_waiver_policy()
  saveRDS(d, file.path(DIR_DERIVED, "panel_with_laus.rds"))
  message("Wrote data/derived/panel_with_laus.rds from panel_with_acs.csv (+ policy)  nrow = ", nrow(d))
} else {
  panel_base <- readRDS(file.path(DIR_DERIVED, "panel_base.rds"))
  saveRDS(panel_base, file.path(DIR_DERIVED, "panel_with_laus.rds"))
  message("Wrote data/derived/panel_with_laus.rds (no LAUS merge; panel_base only)  nrow = ", nrow(panel_base))
}

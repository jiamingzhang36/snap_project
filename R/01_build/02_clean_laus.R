# R/01_build/02_clean_laus.R
# Input:  LAUS-related tables in data/raw or data_clean
# Output: data/derived/panel_with_laus.rds — panel_base + unemp_rate, du_yoy, du_mom, etc.

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)

# Migration: if snap_laus_with_policy or panel_with_acs exists, add LAUS vars and save
path_snap_laus <- file.path(DIR_DATA_CLEAN, "snap_laus_with_policy.csv")
path_panel_acs <- file.path(DIR_DATA_CLEAN, "panel_with_acs.csv")

if (file.exists(path_snap_laus)) {
  d <- readr::read_csv(path_snap_laus, show_col_types = FALSE)
  # Can add du_yoy, du_mom here
  saveRDS(d, file.path(DIR_DERIVED, "panel_with_laus.rds"))
  message("Wrote data/derived/panel_with_laus.rds from snap_laus_with_policy.csv  nrow = ", nrow(d))
} else if (file.exists(path_panel_acs)) {
  d <- readr::read_csv(path_panel_acs, show_col_types = FALSE)
  saveRDS(d, file.path(DIR_DERIVED, "panel_with_laus.rds"))
  message("Wrote data/derived/panel_with_laus.rds from panel_with_acs.csv  nrow = ", nrow(d))
} else {
  # No LAUS merge: use panel_base as placeholder
  panel_base <- readRDS(file.path(DIR_DERIVED, "panel_base.rds"))
  saveRDS(panel_base, file.path(DIR_DERIVED, "panel_with_laus.rds"))
  message("Wrote data/derived/panel_with_laus.rds (no LAUS yet)  nrow = ", nrow(panel_base))
}

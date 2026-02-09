# R/01_build/03_merge_panel.R
# Input:  data/derived/panel_with_laus.rds, data_clean/panel_with_G.csv (G and analysis vars)
# Output: data/derived/panel_analysis.rds — Final analysis panel (log_*, adult_share, exposure_baseline, etc.)

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)

# Migration: use existing panel_with_G as panel_analysis (already has G, y_*, unemployment_rate)
path_panel_G <- file.path(DIR_DATA_CLEAN, "panel_with_G.csv")
stopifnot(file.exists(path_panel_G))

panel_analysis <- readr::read_csv(path_panel_G, show_col_types = FALSE)
panel_analysis <- panel_analysis %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.na(date))

# Optional: add exposure_baseline (e.g. 2022 avgpp or 2017–2019 mean) here later
saveRDS(panel_analysis, file.path(DIR_DERIVED, "panel_analysis.rds"))
message("Wrote data/derived/panel_analysis.rds  nrow = ", nrow(panel_analysis))

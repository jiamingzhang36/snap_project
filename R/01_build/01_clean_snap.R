# R/01_build/01_clean_snap.R
# Input:  data_clean/fap_panel_derived.csv (or FAP + 01_clean upstream)
# Output: data/derived/panel_base.rds — SNAP county-month (cases/recipients/adult/child/payments/avgpp/avgpc)

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)

# Migration: read existing cleaned output
path_in <- file.path(DIR_DATA_CLEAN, "fap_panel_derived.csv")
if (!file.exists(path_in)) path_in <- file.path(DIR_DATA_CLEAN, "FAP_ALL_YEARS_COMBINED_MERGED.csv")
stopifnot(file.exists(path_in))

panel_base <- readr::read_csv(path_in, show_col_types = FALSE)
panel_base <- panel_base %>%
  mutate(
    date_cal = as.Date(paste(year, month, "01", sep = "-"), "%Y-%m-%d"),
    county = as.character(county)
  )

saveRDS(panel_base, file.path(DIR_DERIVED, "panel_base.rds"))
message("Wrote data/derived/panel_base.rds  nrow = ", nrow(panel_base))

# R/99_run/run_all.R — Run full pipeline
# From project root: setwd("path/to/snap_project"); source("R/99_run/run_all.R")

# Ensure we are at project root (contains config/ and R/)
if (!dir.exists("config")) {
  if (file.exists("snap_project.Rproj")) setwd(".") else setwd("..")
}
if (!dir.exists("config")) stop("Run from project root (directory containing config/ and R/).")
ROOT <- getwd()
setwd(ROOT)

source("config/paths.R", local = new.env())
source("R/00_utils/packages.R", local = new.env())

message("=== 01_build ===")
source("R/01_build/01_clean_snap.R", local = new.env())
source("R/01_build/02_clean_laus.R", local = new.env())
source("R/01_build/03_merge_panel.R", local = new.env())

message("=== 02_abawd ===")
source("R/02_abawd/01_construct_event_time.R", local = new.env())
source("R/02_abawd/02_estimate_event_study.R", local = new.env())
source("R/02_abawd/03_figures_tables.R", local = new.env())

message("=== 03_income ===")
source("R/03_income/01_estimate_dl_unemp.R", local = new.env())
source("R/03_income/02_heterogeneity_unemp.R", local = new.env())
source("R/03_income/03_figures_tables.R", local = new.env())

message("=== 04_ea ===")
source("R/04_ea/01_event_study_avg_benefit.R", local = new.env())
source("R/04_ea/02_event_study_participation.R", local = new.env())
source("R/04_ea/03_heterogeneity_intensity.R", local = new.env())
source("R/04_ea/04_figures_tables.R", local = new.env())

message("=== 05_forecast_2026 ===")
source("R/05_forecast_2026/01_baseline_model.R", local = new.env())
source("R/05_forecast_2026/02_scenarios.R", local = new.env())
source("R/05_forecast_2026/03_outputs_maps.R", local = new.env())

message("=== run_all done ===")

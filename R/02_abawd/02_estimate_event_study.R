# R/02_abawd/02_estimate_event_study.R
# Input:  data/derived/panel_analysis.rds (from 01_build/03_merge_panel)
# Output: outputs/step1_did/* (CS + DDD, event-study, robustness). Pipeline no longer depends on data_clean/panel_with_G.csv for DID.

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)

path_panel <- file.path(DIR_DERIVED, "panel_analysis.rds")
stopifnot(file.exists(path_panel))
# Load panel from paper-structure derived data (same content as panel_with_G when 03_merge_panel reads it)
panel_analysis <- readRDS(path_panel)
# 01_did_cs expects a data.frame with date, id, G, y_*, etc. Ensure date is Date for did package.
panel_raw <- as.data.frame(panel_analysis)
if (!"date" %in% names(panel_raw) && "ym_date" %in% names(panel_raw)) panel_raw$date <- panel_raw$ym_date
panel_raw$date <- as.Date(panel_raw$date)

outdir <- file.path(ROOT, "outputs", "step1_did")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
if (!dir.exists(DIR_OUT_FIGURES)) dir.create(DIR_OUT_FIGURES, recursive = TRUE)
if (!dir.exists(DIR_OUT_TABLES)) dir.create(DIR_OUT_TABLES, recursive = TRUE)

# Run full ABAWD DID (CS + DDD) with injected panel_raw; 01_did_cs.R uses it and writes to outdir
message("ABAWD event-study: running full DID from panel_analysis.rds ...")
source("R/01_did_cs.R", local = TRUE)
message("ABAWD event-study: done. Outputs in outputs/step1_did/.")
